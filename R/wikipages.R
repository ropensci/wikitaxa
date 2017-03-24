# MediaWiki (general) ----------------

#' Parse MediaWiki Page URL
#'
#' Parse a MediaWiki page url into its component parts (wiki name, wiki type,
#' and page title). Supports both static page urls and their equivalent API
#' calls.
#'
#' @export
#' @param url (character) MediaWiki page url.
#' @family MediaWiki functions
#' @return a list with elements:
#' \itemize{
#'  \item wiki - wiki language
#'  \item type - wikipedia type
#'  \item page - page name
#' }
#' @examples
#' wt_wiki_url_parse("https://en.wikipedia.org/wiki/Malus_domestica")
#' wt_wiki_url_parse("https://en.wikipedia.org/w/api.php?page=Malus_domestica")
wt_wiki_url_parse <- function(url) {
  url <- curl::curl_unescape(url)
  if (grepl("/w/api.php?", url)) {
    matches <-
      stringr::str_match(
        url, "//([^\\.]+).([^\\.]+).[^/]*/w/api\\.php\\?.*page=([^&]+).*$")
  } else {
    matches <- stringr::str_match(url,
                                  "//([^\\.]+).([^\\.]+).[^/]*/wiki/([^\\?]+)")
  }
  return(list(
    wiki = matches[2],
    type = matches[3],
    page = matches[4]
  ))
}

#' Build MediaWiki Page URL
#'
#' Builds a MediaWiki page url from its component parts (wiki name, wiki type,
#' and page title). Supports both static page urls and their equivalent API
#' calls.
#'
#' @export
#' @param wiki (character | list) Either the wiki name or a list with
#' `$wiki`, `$type`, and `$page` (the output of [wt_wiki_url_parse()]).
#' @param type (character) Wiki type.
#' @param page (character) Wiki page title.
#' @param api (boolean) Whether to return an API call or a static page url
#' (default). If `FALSE`, all following (API-only) arguments are ignored.
#' @param action (character) See <https://en.wikipedia.org/w/api.php>
#' for supported actions. This function currently only supports "parse".
#' @param redirects (boolean) If the requested page is set to a redirect,
#' resolve it.
#' @param format (character) See <https://en.wikipedia.org/w/api.php>
#' for supported output formats.
#' @param utf8 (boolean) If `TRUE`, encodes most (but not all) non-ASCII
#' characters as UTF-8 instead of replacing them with hexadecimal escape
#' sequences.
#' @param prop (character) Properties to retrieve, either as a character vector
#' or pipe-delimited string. See
#' <https://en.wikipedia.org/w/api.php?action=help&modules=parse> for
#' supported properties.
#' @family MediaWiki functions
#' @return a URL (character)
#' @examples
#' wt_wiki_url_build("en", "wikipedia", "Malus domestica")
#' wt_wiki_url_build(
#'   wt_wiki_url_parse("https://en.wikipedia.org/wiki/Malus_domestica"))
#' wt_wiki_url_build("en", "wikipedia", "Malus domestica", api = TRUE)
wt_wiki_url_build <- function(wiki, type = NULL, page = NULL, api = FALSE,
                           action = "parse", redirects = TRUE, format = "json",
                           utf8 = TRUE,
                           prop = c("text", "langlinks", "categories",
                                    "links", "templates", "images",
                                    "externallinks", "sections", "revid",
                                    "displaytitle", "iwlinks", "properties")) {

  if (is.null(type) && is.null(page)) {
    type <- wiki$type
    page <- wiki$page
    wiki <- wiki$wiki
  }
  page <- gsub(" ", "_", page)
  if (api) {
    base_url <- httr::parse_url(paste0("https://", wiki, ".", type,
                                       ".org/w/api.php"))
    if (!utf8) {
      # To ensure it is removed
      utf8 <- ""
    }
    prop <- paste(prop, collapse = "|")
    query <- c(page = page, mget(c("action", "redirects", "format", "utf8",
                                   "prop")))
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
#' @export
#' @param url (character) MediaWiki page url.
#' @param ... Arguments passed to [wt_wiki_url_build()] if `url`
#' is a static page url.
#' @family MediaWiki functions
#' @return an \pkg{httr} response
#' @examples
#' str(wt_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica"))
wt_wiki_page <- function(url, ...) {
  if (!grepl("/w/api.php?", url)) {
    url <- wt_wiki_url_build(wt_wiki_url_parse(url), api = TRUE, ...)
  }
  return(httr::GET(url))
}

#' Parse MediaWiki Page
#'
#' Parses common properties from the result of a MediaWiki API page call.
#'
#' Available properties currently not parsed:
#' title, displaytitle, pageid, revid, redirects, text, categories,
#' links, templates, images, sections, properties, ...
#'
#' @export
#' @param page ([httr::response()]) Result of [wt_wiki_page()]
#' @param types (character) List of properties to parse.
#' @family MediaWiki functions
#' @return a list
#' @examples
#' pg <- wt_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' str(wt_wiki_page_parse(pg))
wt_wiki_page_parse <- function(page, types = c("langlinks", "iwlinks",
                                            "externallinks"),
                               tidy = FALSE) {
  result <- list()
  json <- jsonlite::fromJSON(rawToChar(page$content), tidy)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Links to equivalent page in other languages
  if ("langlinks" %in% types) {
    result$langlinks <- if (tidy) {
      atbl(json$parse$langlinks)
    } else {
      vapply(json$parse$langlinks, "[[", "", "url")
    }
  }
  ## Other wiki links
  if ("iwlinks" %in% types) {
    result$iwlinks <- if (tidy) {
      atbl(json$parse$iwlinks$url)
    } else {
      vapply(json$parse$iwlinks, "[[", "", "url")
    }
  }
  ## Links to external resources
  if ("externallinks" %in% types) {
    result$externallinks <- json$parse$externallinks
  }
  ## Return
  return(result)
}
