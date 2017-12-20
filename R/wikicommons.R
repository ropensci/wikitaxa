#' WikiCommons
#'
#' @export
#' @template args
#' @family Wikicommons functions
#' @return `wt_wikicommons` returns a list, with slots:
#' \itemize{
#'  \item langlinks - language page links
#'  \item externallinks - external links
#'  \item common_names - a data.frame with `name` and `language` columns
#'  \item classification - a data.frame with `rank` and `name` columns
#' }
#'
#' `wt_wikicommons_parse` returns a list
#'
#' `wt_wikicommons_search` returns a list with slots for `continue` and
#' `query`, where `query` holds the results, with `query$search` slot with
#' the search results
#' @references <https://www.mediawiki.org/wiki/API:Search> for help on search
#' @examples \dontrun{
#' # high level
#' wt_wikicommons(name = "Malus domestica")
#' wt_wikicommons(name = "Pinus contorta")
#' wt_wikicommons(name = "Ursus americanus")
#' wt_wikicommons(name = "Balaenoptera musculus")
#' 
#' wt_wikicommons(name = "Category:Poeae")
#' wt_wikicommons(name = "Category:Pinaceae")
#'
#' # low level
#' pg <- wt_wiki_page("https://commons.wikimedia.org/wiki/Malus_domestica")
#' wt_wikicommons_parse(pg)
#'
#' # search wikicommons
#' wt_wikicommons_search(query = "Pinus")
#'
#' ## use search results to dig into pages
#' res <- wt_wikicommons_search(query = "Pinus")
#' lapply(res$query$search$title[1:3], wt_wikicommons)
#' }
wt_wikicommons <- function(name, utf8 = TRUE, ...) {
  assert(name, "character")
  stopifnot(length(name) == 1)
  prop <- c("langlinks", "externallinks", "common_names", "classification")
  res <- wt_wiki_url_build(
    wiki = "commons", type = "wikimedia", page = name,
    utf8 = utf8,
    prop = prop)
  pg <- wt_wiki_page(res, ...)
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
  # if output is NULL
  if (is.null(json$parse)) {
    return(result)
  }
  # if page not found
  txt <- xml2::read_html(json$parse$text[[1]])
  html <- tryCatch(
      xml2::xml_find_all(txt, 
        "//div[contains(., \"Domain\") or contains(., \"Phylum\")]")[[2]], 
      error = function(e) e)
    if (inherits(html, "error")) return(list())
  ## Common names
  if ("common_names" %in% types) {
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
    html <- tryCatch(
      xml2::xml_find_all(txt, 
        "//div[contains(., \"Domain\") or contains(., \"Phylum\")]")[[2]], 
      error = function(e) e)

    # labels
    labels <- c(gsub(":", "", strex(xml2::xml_text(html), "[A-Za-z]+\\)?:")[[1]]), "Authority")
    labels <- gsub("\\(|\\)", "", labels)
    labels <- labels[-1]

    # values    
    values <- xml2::xml_text(
      xml2::xml_find_all(if (inherits(html, "xml_nodes")) html[[2]] else html, ".//b"))
    values <- gsub("^:\\s+|^.+:\\s?", "", values)
    values <- values[-1]

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
  tmp <- g_et(search_base("commons"), sh(query, limit, offset, utf8), ...)
  tmp$query$search <- atbl(tmp$query$search)
  return(tmp)
}
