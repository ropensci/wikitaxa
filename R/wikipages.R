# MediaWiki (general) ----------------

#' Parse MediaWiki Page URL
#'
#' Parse a MediaWiki page url into its component parts (wiki name, wiki type, and page title). Supports both static page urls and their equivalent API calls.
#'
#' @param url (character) MediaWiki page url.
#' @export
#' @family MediaWiki functions
#' @examples
#' parse_wiki_url("https://en.wikipedia.org/wiki/Malus_domestica")
#' parse_wiki_url("https://en.wikipedia.org/w/api.php?page=Malus_domestica")
parse_wiki_url <- function(url) {
  url <- curl::curl_unescape(url)
  if (grepl("/w/api.php?", url)) {
    matches <- stringr::str_match(url, "//([^\\.]+).([^\\.]+).[^/]*/w/api\\.php\\?.*page=([^&]+).*$")
  } else {
    matches <- stringr::str_match(url, "//([^\\.]+).([^\\.]+).[^/]*/wiki/([^\\?]+)")
  }
  return(list(
    wiki = matches[2],
    type = matches[3],
    page = matches[4]
  ))
}

#' Build MediaWiki Page URL
#'
#' Builds a MediaWiki page url from its component parts (wiki name, wiki type, and page title). Supports both static page urls and their equivalent API calls.
#'
#' @param wiki (character | list) Either the wiki name or a list with \code{$wiki}, \code{$type}, and \code{$page} (the output of \code{\link{parse_wiki_url}}).
#' @param type (character) Wiki type.
#' @param page (character) Wiki page title.
#' @param api (boolean) Whether to return an API call or a static page url (default). If \code{FALSE}, all following (API-only) arguments are ignored.
#' @param action (character) See \url{https://en.wikipedia.org/w/api.php} for supported actions. This function currently only supports "parse".
#' @param redirects (boolean) If the requested page is set to a redirect, resolve it.
#' @param format (character) See \url{https://en.wikipedia.org/w/api.php} for supported output formats.
#' @param utf8 (boolean) If \code{TRUE}, encodes most (but not all) non-ASCII characters as UTF-8 instead of replacing them with hexadecimal escape sequences.
#' @param prop (character) Properties to retrieve, either as a character vector or pipe-delimited string. See \url{https://en.wikipedia.org/w/api.php?action=help&modules=parse} for supported properties.
#' @export
#' @family MediaWiki functions
#' @examples
#' build_wiki_url("en", "wikipedia", "Malus domestica")
#' build_wiki_url(parse_wiki_url("https://en.wikipedia.org/wiki/Malus_domestica"))
#' build_wiki_url("en", "wikipedia", "Malus domestica", api = TRUE)
build_wiki_url <- function(wiki, type = NULL, page = NULL, api = FALSE, action = "parse", redirects = TRUE, format = "json", utf8 = TRUE, prop = c("text", "langlinks", "categories", "links", "templates", "images", "externallinks", "sections", "revid", "displaytitle", "iwlinks", "properties")) {
  if (is.null(type) && is.null(page)) {
    type <- wiki$type
    page <- wiki$page
    wiki <- wiki$wiki
  }
  page <- gsub(" ", "_", page)
  if (api) {
    base_url <- httr::parse_url(paste0("https://", wiki, ".", type, ".org/w/api.php"))
    if (!utf8) {
      utf8 <- "" # To ensure it is removed
    }
    prop <- paste(prop, collapse = "|")
    query <- c(page = page, mget(c("action", "redirects", "format", "utf8", "prop")))
    query <- query[sapply(query, "!=", "")]
    url <- httr::modify_url(base_url, query = query)
    return(url)
  } else {
    return(paste0("https://", wiki, ".", type, ".org/wiki/", page))
  }
}

#' Get MediaWiki Page from API
#'
#' Supports both static page urls and their equivalent API calls.
#'
#' @param url (character) MediaWiki page url.
#' @param ... Arguments passed to \code{\link{build_wiki_url}} if \code{url} is a static page url.
#' @export
#' @family MediaWiki functions
#' @examples
#' str(get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica"))
get_wiki_page <- function(url, ...) {
  if (!grepl("/w/api.php?", url)) {
    url <- build_wiki_url(parse_wiki_url(url), api = TRUE, ...)
  }
  return(httr::GET(url))
}

#' Parse MediaWiki Page
#'
#' Parses common properties from the result of a MediaWiki API page call.
#'
#' Available properties currently not parsed:
#' title, displaytitle, pageid, revid, redirects[], text[1], categories[], links[], templates[], images[], sections[], properties[], ...
#'
#' @param page (\code{\link[httr]{response}}) Result of \code{\link{get_wiki_page}}.
#' @param types (character) List of properties to parse.
#' @export
#' @family MediaWiki functions
#' @examples
#' pg <- get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' str(parse_wiki_page(pg))
parse_wiki_page <- function(page, types = c("langlinks", "iwlinks", "externallinks")) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Links to equivalent page in other languages
  if ("langlinks" %in% types) {
    result$langlinks <- unname(sapply(json$parse$langlinks, "[", "url"))
  }
  ## Other wiki links
  if ("iwlinks" %in% types) {
    result$iwlinks <- unname(sapply(json$parse$iwlinks, "[", "url"))
  }
  ## Links to external resources
  if ("externallinks" %in% types) {
    result$externallinks <- json$parse$externallinks
  }
  ## Return
  return(result)
}

# Wikipedia ----------------

#' Parse Wikipedia Page
#'
#' Parses properties from the result of a Wikipedia API page call.
#'
#' @param page (\code{\link[httr]{response}}) Result of \code{\link{get_wiki_page}}.
#' @param types (character) List of properties to parse.
#' @family Wikipedia functions
#' @export
#' @examples
#' pg <- get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' str(parse_wikipedia_page(pg))
#' pg <- get_wiki_page("https://en.wikipedia.org/wiki/Abelmoschus")
#' str(parse_wikipedia_page(pg)) # no common names
parse_wikipedia_page <- function(page, types = c("langlinks", "iwlinks", "externallinks", "common_names")) {
  result <- parse_wiki_page(page, types = types)
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    names_xml <- list(
      regular_bolds = xml2::xml_find_all(xml, xpath = "/html/body/p[count(preceding::div[contains(@id, 'toc') or contains(@class, 'toc')]) = 0 and count(preceding::h1) = 0 and count(preceding::h2) = 0 and count(preceding::h3) = 0]//b[not(parent::*[self::i]) and not(i)]"),
      regular_biotabox_header = xml2::xml_find_all(xml, xpath = "(//table[contains(@class, 'infobox biota') or contains(@class, 'infobox_v2 biota')]//th)[1]/b[not(parent::*[self::i]) and not(i)]")
    )
    regular_title <- na.omit(stringr::str_match(json$parse$displaytitle, "^([^<]*)$")[, 2]) # NOTE: Often unreliable.
    common_names <- unique(c(unlist(sapply(names_xml, xml2::xml_text)), regular_title))
    language <- stringr::str_match(page$url, 'http[s]*://([^\\.]*)\\.')[, 2]
    result$common_names <- lapply(common_names, function(name) {list(name = name, language = language)})
  }
  ## Return
  return(result)
}

# Wikicommons ----------------

#' Parse Wikimedia Commons Page
#'
#' Parses properties from the result of a Wikimedia Commons API page call.
#'
#' @param page (\code{\link[httr]{response}}) Result of \code{\link{get_wiki_page}}.
#' @param types (character) List of properties to parse.
#' @family Wikicommons functions
#' @export
#' @examples
#' pg <- get_wiki_page("https://commons.wikimedia.org/wiki/Malus_domestica")
#' str(parse_wikicommons_page(pg))
#' pg <- get_wiki_page("https://commons.wikimedia.org/wiki/Abelmoschus")
#' str(parse_wikicommons_page(pg))
parse_wikicommons_page = function(page, types = c("langlinks", "iwlinks", "externallinks", "common_names")) {
  result <- parse_wiki_page(page, types = types)
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    vernacular_html <- xml2::xml_find_all(xml, xpath = "//bdi[@class='vernacular']")
    # XML formats:
    # <bdi class="vernacular" lang="en"><a href="">name</a></bdi>
    # <bdi class="vernacular" lang="en">name</bdi>
    ## Name formats:
    # name1 / name2
    # name1, name2
    # name (category)
    common_names <- lapply(vernacular_html, function(x) {
      attributes <- xml2::xml_attrs(x)
      language <- attributes[["lang"]]
      name <- trimws(gsub("[ ]*\\(.*\\)", "", xml2::xml_text(x)))
      list(
        name = name,
        language = language
      )
    })
    result$common_names <- common_names
  }
  ## Return
  return(result)
}

# Wikispecies ----------------

#' Parse Wikimedia Species Page
#'
#' Parses properties from the result of a Wikimedia Species API page call.
#'
#' @param page (\code{\link[httr]{response}}) Result of \code{\link{get_wiki_page}}.
#' @param types (character) List of properties to parse.
#' @family Wikispecies functions
#' @export
#' @examples
#' pg <- get_wiki_page("https://species.wikimedia.org/wiki/Malus_domestica")
#' str(parse_wikispecies_page(pg))
#' pg <- get_wiki_page("https://species.wikimedia.org/wiki/Abelmoschus")
#' str(parse_wikispecies_page(pg))
parse_wikispecies_page <- function(page, types = c("langlinks", "iwlinks", "externallinks", "common_names")) {
  result <- parse_wiki_page(page, types = types)
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    # XML formats:
    # <b>language:</b>&nbsp;[name|<a>name</a>]
    # Name formats:
    # name1, name2
    vernacular_html <- xml2::xml_find_all(xml, xpath = "(//h2/span[@id='Vernacular_names']/parent::*/following-sibling::div)[1]/p")
    languages_html <- xml2::xml_find_all(vernacular_html, xpath = "b")
    languages <- gsub("\\s*:\\s*", "", sapply(languages_html, xml2::xml_text))
    names_html <- xml2::xml_find_all(vernacular_html, xpath = "b[not(following-sibling::*[1][self::a])]/following-sibling::text()[1] | b/following-sibling::*[1][self::a]/text()")
    common_names <- gsub("^\\s*", "", sapply(names_html, xml2::xml_text))
    result$common_names <- mapply(list, name = common_names, language = languages, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  ## Return
  return(result)
}
