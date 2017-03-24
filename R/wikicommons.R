# Wikicommons ----------------

#' WikiCommons
#'
#' @export
#' @param name (character) Wiki name - as a page title
#' @param utf8 (boolean) If `TRUE`, encodes most (but not all) non-ASCII
#' characters as UTF-8 instead of replacing them with hexadecimal escape
#' sequences.
#' @param page ([httr::response()]) Result of [wt_wiki_page()].
#' @param types (character) List of properties to parse
#' @family Wikicommons functions
#' @return `wt_wikicommons` returns a list, with slots:
#' \itemize{
#'  \item langlinks - language page links
#'  \item externallinks - external links
#'  \item common_names - a data.frame with `name` and `language` columns
#' }
#'
#' `wt_wikicommons_parse` returns a list
#' @examples
#' # high level
#' wt_wikicommons(name = "Malus domestica")
#' wt_wikicommons(name = "Poa annua")
#' wt_wikicommons(name = "Quercus")
#'
#' # low level
#' pg <- wt_wiki_page("https://commons.wikimedia.org/wiki/Malus_domestica")
#' wt_wikicommons_parse(pg)
#' pg <- wt_wiki_page("https://commons.wikimedia.org/wiki/Abelmoschus")
#' wt_wikicommons_parse(pg)
#'
#' # search wikicommons
#' wt_wikicommons_search(query = "Pinus")
#' wt_wikicommons_search(query = "pine tree", limit = 3)
#' wt_wikicommons_search(query = "pine tree", limit = 3, offset = 3)
#'
#' ## use search results to dig into pages
#' res <- wt_wikicommons_search(query = "Pinus")
#' lapply(res$query$search$title[1:3], wt_wikicommons)
wt_wikicommons <- function(name, utf8 = TRUE) {
  prop <- c("langlinks", "externallinks", "common_names", "classification")
  res <- wt_wiki_url_build(
    wiki = "commons", type = "wikimedia", page = name,
    utf8 = utf8,
    prop = prop)
  pg <- wt_wiki_page(res)
  wt_wikicommons_parse(pg, prop, tidy = TRUE)
}

#' @export
#' @rdname wt_wikicommons
wt_wikicommons_parse <- function(page, types = c("langlinks", "iwlinks",
                                          "externallinks", "common_names",
                                          "classification"),
                                 tidy = FALSE) {

  result <- wt_wiki_page_parse(page, types = types, tidy = tidy)
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    txt <- xml2::read_html(json$parse$text[[1]])
    vernacular_html <- xml2::xml_find_all(txt,
                                          xpath = "//bdi[@class='vernacular']")
    # XML formats:
    # <bdi class="vernacular" lang="en"><a href="">name</a></bdi>
    # <bdi class="vernacular" lang="en">name</bdi>
    ## Name formats:
    # name1 / name2
    # name1, name2
    # name (category)
    cnms <- lapply(vernacular_html, function(x) {
      attributes <- xml2::xml_attrs(x)
      language <- attributes[["lang"]]
      name <- trimws(gsub("[ ]*\\(.*\\)", "", xml2::xml_text(x)))
      list(
        name = name,
        language = language
      )
    })
    result$common_names <- if (tidy) atbl(dt_df(cnms)) else cnms
  }
  ## classification
  if ("classification" %in% types) {
    txt <- xml2::read_html(json$parse$text[[1]])
    html <- xml2::xml_find_all(txt, "//div[contains(., \"APG IV\")]")
    labels <- xml2::xml_text(xml2::xml_find_all(
      html,
      "b[not(following-sibling::*[1][self::a])]/following-sibling::text()[1] | b/following-sibling::*[1][self::a]/text()"
    ))
    labels <- gsub(
      "^\\s+|\\s$|\\(|\\)", "",
      gsub("^:\\s+|^\\s+\\u2022\\s+", "", labels)
    )
    values <- xml2::xml_text(xml2::xml_find_all(html, ".//b"))[-1]
    values <- gsub("^:\\s+|^.+:\\s?", "", values)
    clz <- mapply(list, rank = labels, name = values,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    result$classification <- if (tidy) atbl(dt_df(clz)) else clz
  }
  return(result)
}

#' @export
#' @rdname wt_wikicommons
wt_wikicommons_search <- function(query, limit = 10, offset = 0, utf8 = TRUE,
                                  ...) {

  args <- tc(list(
    action = "query", list = "search", srsearch = query,
    utf8 = if (utf8) "" else NULL, format = "json",
    srprop = "size|wordcount|timestamp|snippet",
    srlimit = limit, sroffset = offset
  ))
  res <- httr::GET(search_base("commons"), query = args, ...)
  httr::stop_for_status(res)
  txt <- httr::content(res, "text", encoding = "UTF-8")
  tmp <- jsonlite::fromJSON(txt)
  tmp$query$search <- atbl(tmp$query$search)
  return(tmp)
}
