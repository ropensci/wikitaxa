# Wiki (general) ----------------

#' Parse Wiki URL
#'
#' Supports both wiki page urls and their equivalent API calls.
#'
#' @export
#' @family Wiki functions
#' @examples
#' parse_wiki_url("https://en.wikipedia.org/wiki/Malus_domestica")
#' parse_wiki_url("https://en.wikipedia.org/w/api.php?page=Malus_domestica")
parse_wiki_url <- function(url, api = FALSE) {
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

#' Build Wiki URL
#'
#' Supports both wiki page urls and their equivalent API calls.
#'
#' @export
#' @family Wiki functions
#' @examples
#' build_wiki_url("en", "wikipedia", "Malus domestica")
#' build_wiki_url("en", "wikipedia", "Malus domestica", api = TRUE)
#' build_wiki_url(parse_wiki_url("https://en.wikipedia.org/wiki/Malus_domestica"))
build_wiki_url <- function(wiki, type = NULL, page = NULL, format = "json", action = "parse", redirects = TRUE, api = FALSE) {
  if (is.null(type) && is.null(page)) {
    type <- wiki$type
    page <- wiki$page
    wiki <- wiki$wiki
  }
  page <- gsub(" ", "_", page)
  if (api) {
    base_url <- httr::parse_url(paste0("https://", wiki, ".", type, ".org/w/api.php"))
    query <- c(page = page, mget(c("format", "action", "redirects")))
    query <- query[sapply(query, "!=", "")]
    url <- httr::modify_url(base_url, query = query)
    return(url)
  } else {
    return(paste0("https://", wiki, ".", type, ".org/wiki/", page))
  }
}

#' Get Wiki Page from API
#'
#' Supports both wiki page urls and their equivalent API calls.
#'
#' @export
#' @family Wiki functions
#' @examples
#' str(get_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica"))
get_wiki_page <- function(url, ...) {
  if (!grepl("/w/api.php?", url)) {
    url <- build_wiki_url(parse_wiki_url(url), ..., api = TRUE)
  }
  return(httr::GET(url))
}

#' Parse Wiki Page
#'
#' Parses common properties from Wikimedia API page results.
#'
#' NOTE: Currently not parsed:
#' title, displaytitle, pageid, revid, redirects[], text[1], categories[], links[], templates[], images[], sections[], properties[]
#'
#' @family Wiki functions
#' @export
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

# Wikimedia ----------------

#' Parse Wikimedia Commons Page
#'
#' @family Wiki functions
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

#' Parse Wikispecies Page
#'
#' @family Wiki functions
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
    vernacular_html <- xml2::xml_find_all(xml, xpath = "(//h2/span[@id='Vernacular_names']/parent::*/following-sibling::div)[1]")
    languages_html <- xml2::xml_find_all(vernacular_html, xpath = "b")
    languages <- gsub("\\s*:\\s*", "", sapply(languages_html, xml2::xml_text))
    names_html <- xml2::xml_find_all(vernacular_html, xpath = "b[not(following-sibling::*[1][self::a])]/following-sibling::text()[1] | b/following-sibling::*[1][self::a]/text()")
    common_names <- gsub("^\\s*", "", sapply(names_html, xml2::xml_text))
    result$common_names <- mapply(list, name = common_names, language = languages, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  ## Return
  return(result)
}

# Wikidata ----------------

#' Wikidata taxonomy data
#'
#' @export
#' @param x (character) a taxonomic name
#' @param property (character) a property id, e.g., P486
#' @param ... curl options passed on to \code{\link[httr]{GET}}
#' @param language (character) two letter language code
#' @param limit (integer) records to return. Default: 10
#' @examples \dontrun{
#' wiki_data("Poa annua")
#' wiki_data("Mimulus alsinoides")
#' wiki_data("Mimulus foliatus")
#' wiki_data("Mimulus foliatus", property = "P846")
#' wiki_data("Mimulus foliatus", property = c("P846", "P815"))
#'
#' wiki_data(get_wiki_id("Mimulus foliatus"))
#' }
wiki_data <- function(x, property = NULL, ...) {
  UseMethod("wiki_data")
}

#' @export
wiki_data.wiki_id <- function(x, property = NULL, ...) {
  data_wiki(x, property = property, ...)
}

#' @export
wiki_data.default <- function(x, property = NULL, ...) {
  x <- WikidataR::find_item(search_term = x, ...)
  if (length(x) == 0) stop("no results found", call. = FALSE)
  data_wiki(x[[1]]$id, property = property, ...)
}

#' @export
#' @rdname wiki_data
get_wiki_id <- function(x, language = "en", limit = 10, ...) {
  x <- WikidataR::find_item(search_term = x, language = language, limit = limit, ...)
  x <- if (length(x) == 0) NA else x[[1]]$id
  structure(x, class = "wiki_id")
}

data_wiki <- function(x, property = NULL, ...) {
  xx <- WikidataR::get_item(x, ...)

  if (is.null(property)) {
    claims <- create_claims(xx$claims)
  } else{
    cl <- Filter(function(x) x$mainsnak$property %in% property, xx$claims)
    if (length(cl) == 0) stop("No matching properties", call. = FALSE)
    claims <- create_claims(cl)
  }

  list(
    labels = dt_df(xx$labels),
    descriptions = dt_df(xx$descriptions),
    aliases = dt_df(xx$aliases),
    sitelinks = dt_df(lapply(xx$sitelinks, function(x) x[names(x) %in% c('site', 'title')])),
    claims = dt_df(claims)
  )
}

dt_df <- function(x) {
  (ffff <- data.table::setDF(data.table::rbindlist(x, fill = TRUE, use.names = TRUE)))
}

fetch_property <- function(x) {
  tmp <- WikidataR::get_property(x)
  list(
    property_value = tmp$labels$en$value,
    property_description = tmp$descriptions$en$value
  )
}

create_claims <- function(x) {
  lapply(x, function(z) {
    c(
      property = paste0(z$mainsnak$property, collapse = ","),
      fetch_property(z$mainsnak$property),
      value = {
        if (inherits(z$mainsnak$datavalue$value, "data.frame")) {
          z$mainsnak$datavalue$value$`numeric-id`
        } else {
          z$mainsnak$datavalue$value
        }
      }
    )
  })
}

# Helpers ----------------



# alldat$labels
# alldat$descriptions
# alldat$aliases
# alldat$sitelinks
# alldat$claims

# ## Wikipedia taxonomy - trying to incorporate
# library(WikipediR)
# library(xml2)
# library(dplyr)
#
# foo("Helianthus")
# foo(name = "Helianthus annuus")
# foo(name = "Quercus lobata")
# foo(name = "Poa annua")
#
# foo <- function(name) {
#   bb <- WikipediR::page_content("en", domain = "species.wikimedia.org", page_name = name)
#   html <- xml2::read_html(bb$parse$text$`*`)
#   xx <- rvest::html_table(
#     xml2::xml_find_first(html, '//table[@class="wikitable mw-collapsible mw-collapsed"]')
#   )
#   setNames(
#     dplyr::bind_rows(
#       lapply(strsplit(xx[[1]], "\n")[[1]], function(z) {
#         tmp <- gsub("^\\s+|\\s+$", "", strsplit(z, ":")[[1]])
#         dplyr::as_data_frame(t(tmp))
#       })
#     ), c('rank', 'name')
#   )
# }
#
#
#
# bb <- WikipediR::page_content("en", domain = "commons.wikimedia.org", page_name = name)
# html <- xml2::read_html(bb$parse$text$`*`)
# xx <- rvest::html_table(
#   xml2::xml_find_first(html, '//table[@class="wikitable mw-collapsible mw-collapsed"]')
# )
