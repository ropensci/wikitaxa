#' Wikipedia
#'
#' @export
#' @template args
#' @family Wikipedia functions
#' @return `wt_wikipedia` returns a list, with slots:
#' \itemize{
#'  \item langlinks - language page links
#'  \item externallinks - external links
#'  \item common_names - a data.frame with `name` and `language` columns
#'  \item classification - a data.frame with `rank` and `name` columns
#'  \item synonyms - a character vector with taxonomic names
#' }
#'
#' `wt_wikipedia_parse` returns a list with same slots determined by
#' the `types` parmeter
#'
#' `wt_wikipedia_search` returns a list with slots for `continue` and
#' `query`, where `query` holds the results, with `query$search` slot with
#' the search results
#' @references <https://www.mediawiki.org/wiki/API:Search> for help on search
#' @examples
#' # high level
#' wt_wikipedia(name = "Malus domestica")
#' wt_wikipedia(name = "Poa annua")
#' wt_wikipedia(name = "Quercus")
#'
#' # low level
#' pg <- wt_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' wt_wikipedia_parse(pg)
#' wt_wikipedia_parse(pg, tidy = TRUE)
#' ## no common names
#' pg <- wt_wiki_page("https://en.wikipedia.org/wiki/Abelmoschus")
#' wt_wikipedia_parse(pg)
#'
#' # search wikipedia
#' wt_wikipedia_search(query = "Pinus")
#' wt_wikipedia_search(query = "pine tree", limit = 3)
#' wt_wikipedia_search(query = "pine tree", limit = 3, offset = 3)
#'
#' ## curl options
#' wt_wikipedia_search(query = "Pinus", verbose = TRUE)
#'
#' ## use search results to dig into pages
#' res <- wt_wikipedia_search(query = "Pinus")
#' lapply(res$query$search$title[1:3], wt_wikipedia)
wt_wikipedia <- function(name, utf8 = TRUE, ...) {
  assert(name, "character")
  prop <- c("langlinks", "externallinks", "common_names", "classification",
            "synonyms")
  res <- wt_wiki_url_build(
    wiki = "en", type = "wikipedia", page = name,
    utf8 = utf8,
    prop = prop)
  pg <- wt_wiki_page(res, ...)
  wt_wikipedia_parse(page = pg, types = prop, tidy = TRUE)
}

#' @export
#' @rdname wt_wikipedia
wt_wikipedia_parse <- function(page, types = c("langlinks", "iwlinks",
                                         "externallinks", "common_names",
                                         "classification"),
                               tidy = FALSE) {

  result <- wt_wiki_page_parse(page, types = types, tidy = tidy)
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = TRUE)
  if (is.null(json$parse)) {
    return(result)
  }
  ## Common names
  if ("common_names" %in% types) {
    xml <- xml2::read_html(json$parse$text[[1]])
    names_xml <- list(
      regular_bolds = xml2::xml_find_all(
        xml,
        xpath = "/html/body/p[count(preceding::div[contains(@id, 'toc') or contains(@class, 'toc')]) = 0 and count(preceding::h1) = 0 and count(preceding::h2) = 0 and count(preceding::h3) = 0]//b[not(parent::*[self::i]) and not(i)]"),
      regular_biotabox_header =
        xml2::xml_find_all(
          xml,
          xpath = "(//table[contains(@class, 'infobox biota') or contains(@class, 'infobox_v2 biota')]//th)[1]/b[not(parent::*[self::i]) and not(i)]")
    )
    # NOTE: Often unreliable.
    regular_title <- stats::na.omit(
      match_(json$parse$displaytitle, "^([^<]*)$")[2])
    common_names <- unique(c(unlist(sapply(names_xml, xml2::xml_text)),
                             regular_title))
    language <- match_(page$url, 'http[s]*://([^\\.]*)\\.')[2]
    cnms <- lapply(common_names, function(name) {
      list(name = name, language = language)
    })
    result$common_names <- if (tidy) atbl(dt_df(cnms)) else cnms
  }
  ## classification
  if ("classification" %in% types) {
    txt <- xml2::read_html(json$parse$text[[1]])
    html <-
      xml2::xml_find_all(txt, "//table[@class=\"infobox biota\"]//span")
    labels <- xml2::xml_attr(html, "class")
    labels <- gsub("^\\s+|\\s$|\\(|\\)", "", labels)
    values <- gsub("^\\s+|\\s$", "", xml2::xml_text(html))
    clz <- mapply(list, rank = labels, name = values,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    result$classification <- if (tidy) atbl(dt_df(clz)) else clz
  }
  ## synonyms
  if ("synonyms" %in% types) {
    syns <- list()
    txt <- xml2::read_html(json$parse$text[[1]])
    html <-
      xml2::xml_find_all(txt, "//table[@class=\"infobox biota\"]//td")
    syn_node <-
      xml2::xml_find_first(html, "//th/a[contains(text(), \"Synonyms\")]")
    if (length(stats::na.omit(xml2::xml_text(syn_node))) > 0) {
      syn <- strsplit(xml2::xml_text(html[length(html)]), "\n")[[1]]
      syns <- syn[nzchar(syn)]
    }
    result$synonyms <- syns
  }

  return(result)
}

#' @export
#' @rdname wt_wikipedia
wt_wikipedia_search <- function(query, limit = 10, offset = 0, utf8 = TRUE,
                                  ...) {
  tmp <- g_et(search_base("en", "wikipedia"), sh(query, limit, offset, utf8), ...)
  tmp$query$search <- atbl(tmp$query$search)
  return(tmp)
}
