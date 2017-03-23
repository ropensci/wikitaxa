# Wikipedia ----------------

#' Wikipedia
#'
#' @export
#' @param name (character) Wiki name - as a page title
#' @param utf8 (boolean) If \code{TRUE}, encodes most (but not all) non-ASCII
#' characters as UTF-8 instead of replacing them with hexadecimal escape
#' sequences.
#' @param page (\code{\link[httr]{response}}) Result of
#' \code{\link{wt_wiki_page}}.
#' @param types (character) List of properties to parse
#' @family Wikipedia functions
#' @return `wt_wikipedia` returns a list, with slots:
#' \itemize{
#'  \item langlinks - language page links
#'  \item externallinks - external links
#'  \item common_names - a data.frame with `name` and `language` columns
#' }
#'
#' But returns an empty list on no results found
#'
#' `wt_wikipedia_parse` returns a list
#' @examples
#' # high level
#' wt_wikipedia(name = "Malus domestica")
#' wt_wikipedia(name = "Poa annua")
#' wt_wikipedia(name = "Quercus")
#'
#' # low level
#' pg <- wt_wiki_page("https://en.wikipedia.org/wiki/Malus_domestica")
#' wt_wikipedia_parse(pg)
#' ## no common names
#' pg <- wt_wiki_page("https://en.wikipedia.org/wiki/Abelmoschus")
#' wt_wikipedia_parse(pg)
wt_wikipedia <- function(name, utf8 = TRUE) {
  prop <- c("langlinks", "externallinks", "common_names", "classification",
            "synonyms")
  res <- wt_wiki_url_build(
    wiki = "en", type = "wikipedia", page = name,
    utf8 = utf8,
    prop = prop)
  pg <- wt_wiki_page(res)
  tmp <- wt_wikipedia_parse(pg, prop)
  if (length(tmp$common_names)) tmp$common_names <- dt_df(tmp$common_names)
  if (length(tmp$classification)) tmp$classification <-
    dt_df(tmp$classification)
  return(tmp)
}

#' @export
#' @rdname wt_wikipedia
wt_wikipedia_parse <- function(page, types = c("langlinks", "iwlinks",
                                         "externallinks", "common_names",
                                         "classification")) {

  result <- wt_wiki_page_parse(page, types = types)
  json <- jsonlite::fromJSON(rawToChar(page$content), simplifyVector = FALSE)
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
    regular_title <- stats::na.omit(
      stringr::str_match(json$parse$displaytitle,
                         "^([^<]*)$")[, 2]) # NOTE: Often unreliable.
    common_names <- unique(c(unlist(sapply(names_xml, xml2::xml_text)),
                             regular_title))
    language <- stringr::str_match(page$url, 'http[s]*://([^\\.]*)\\.')[, 2]
    result$common_names <- lapply(common_names, function(name) {
      list(name = name, language = language)
    })
  }
  ## classification
  if ("classification" %in% types) {
    txt <- xml2::read_html(json$parse$text[[1]])
    html <-
      xml2::xml_find_all(txt, "//table[@class=\"infobox biota\"]//span")
    labels <- xml2::xml_attr(html, "class")
    labels <- gsub("^\\s+|\\s$|\\(|\\)", "", labels)
    values <- gsub("^\\s+|\\s$", "", xml2::xml_text(html))
    result$classification <- mapply(list, rank = labels, name = values,
                                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
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
