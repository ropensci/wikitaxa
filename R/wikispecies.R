# Wikispecies ----------------

#' WikiSpecies
#'
#' @export
#' @param name (character) Wiki name - as a page title
#' @param utf8 (boolean) If `TRUE`, encodes most (but not all) non-ASCII
#' characters as UTF-8 instead of replacing them with hexadecimal escape
#' sequences.
#' @param page ([httr::response()]) Result of
#' [wt_wiki_page()].
#' @param types (character) List of properties to parse
#' @param query (character) query terms
#' @param limit (integer) number of results to return. Default: 10
#' @param offset (integer) record to start at. Default: 0
#' @param utf8 (logical) use utf-8. Default: `TRUE`
#' @param ... curl options, passed on to [httr::GET()]
#' @family Wikispecies functions
#' @return `wt_wikispecies` returns a list, with slots:
#' \itemize{
#'  \item langlinks - language page links
#'  \item externallinks - external links
#'  \item common_names - a data.frame with `name` and `language` columns
#' }
#'
#' `wt_wikispecies_parse` returns a list
#' @examples
#' # high level
#' wt_wikispecies(name = "Malus domestica")
#' wt_wikispecies(name = "Poa annua")
#' wt_wikispecies(name = "Quercus")
#'
#' # low level
#' pg <- wt_wiki_page("https://species.wikimedia.org/wiki/Malus_domestica")
#' wt_wikispecies_parse(pg)
#' pg <- wt_wiki_page("https://species.wikimedia.org/wiki/Abelmoschus")
#' wt_wikispecies_parse(pg)
#'
#' # search wikispecies
#' wt_wikispecies_search(query = "pine tree")
#' wt_wikispecies_search(query = "pine tree", limit = 3)
#' wt_wikispecies_search(query = "pine tree", limit = 3, offset = 3)
#'
#' ## use search results to dig into pages
#' res <- wt_wikispecies_search(query = "pine tree")
#' lapply(res$query$search$title[1:3], wt_wikispecies)
wt_wikispecies <- function(name, utf8 = TRUE) {
  prop <- c("langlinks", "externallinks", "common_names", "classification")
  res <- wt_wiki_url_build(
    wiki = "species", type = "wikimedia", page = name,
    utf8 = utf8,
    prop = prop)
  pg <- wt_wiki_page(res)
  wt_wikispecies_parse(pg, prop, tidy = TRUE)
}

#' @export
#' @rdname wt_wikispecies
wt_wikispecies_parse <- function(page, types = c("langlinks", "iwlinks",
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
    xml <- xml2::read_html(json$parse$text[[1]])
    # XML formats:
    # <b>language:</b>&nbsp;[name|<a>name</a>]
    # Name formats:
    # name1, name2
    vernacular_html <-
      xml2::xml_find_all(
        xml,
        "(//h2/span[contains(@id, 'Vernacular')]/parent::*/following-sibling::div)[1]")
    languages_html <- xml2::xml_find_all(vernacular_html, xpath = "b")
    languages <- gsub("\\s*:\\s*", "", sapply(languages_html, xml2::xml_text))
    names_html <-
      xml2::xml_find_all(
        vernacular_html,
        "b[not(following-sibling::*[1][self::a])]/following-sibling::text()[1] | b/following-sibling::*[1][self::a]/text()")
    common_names <- gsub("^\\s*", "", sapply(names_html, xml2::xml_text))
    cnms <-
      mapply(list, name = common_names,
             language = languages, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    result$common_names <- if (tidy) atbl(dt_df(cnms)) else cnms
  }
  ## classification
  if ("classification" %in% types) {
    txt <- xml2::read_html(json$parse$text[[1]])
    html <- xml2::xml_text(xml2::xml_find_first(txt, "//table[contains(@class, \"wikitable\")]//p"))
    html <- strsplit(html, "\n")[[1]]
    labels <-
      vapply(html, function(z) strsplit(z, ":")[[1]][1], "", USE.NAMES = FALSE)
    values <-
      vapply(html, function(z) strsplit(z, ":")[[1]][2], "", USE.NAMES = FALSE)
    values <- gsub("^\\s+|\\s+$", "", values)
    clz <- mapply(list, rank = labels, name = values,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    result$classification <- if (tidy) atbl(dt_df(clz)) else clz
  }
  return(result)
}

#' @export
#' @rdname wt_wikispecies
wt_wikispecies_search <- function(query, limit = 10, offset = 0, utf8 = TRUE,
                                  ...) {

  args <- tc(list(
    action = "query", list = "search", srsearch = query,
    utf8 = if (utf8) "" else NULL, format = "json",
    srprop = "size|wordcount|timestamp|snippet",
    srlimit = limit, sroffset = offset
  ))
  res <- httr::GET(search_base("species"), query = args, ...)
  httr::stop_for_status(res)
  txt <- httr::content(res, "text", "UTF-8")
  tmp <- jsonlite::fromJSON(txt)
  tmp$query$search <- atbl(tmp$query$search)
  return(tmp)
}
