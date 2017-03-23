# Wikispecies ----------------

#' WikiSpecies
#'
#' @export
#' @param name (character) Wiki name - as a page title
#' @param utf8 (boolean) If \code{TRUE}, encodes most (but not all) non-ASCII
#' characters as UTF-8 instead of replacing them with hexadecimal escape
#' sequences.
#' @param page (\code{\link[httr]{response}}) Result of
#' \code{\link{wt_wiki_page}}.
#' @param types (character) List of properties to parse
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
#' str(wt_wikispecies_parse(pg))
#' pg <- wt_wiki_page("https://species.wikimedia.org/wiki/Abelmoschus")
#' str(wt_wikispecies_parse(pg))
wt_wikispecies <- function(name, utf8 = TRUE) {
  prop <- c("langlinks", "externallinks", "common_names", "classification")
  res <- wt_wiki_url_build(
    wiki = "species", type = "wikimedia", page = name,
    utf8 = utf8,
    prop = prop)
  pg <- wt_wiki_page(res)
  tmp <- wt_wikispecies_parse(pg, prop)
  tmp$common_names <- dt_df(tmp$common_names)
  tmp$classification <- dt_df(tmp$classification)
  return(tmp)
}

#' @export
#' @rdname wt_wikispecies
wt_wikispecies_parse <- function(page, types = c("langlinks", "iwlinks",
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
    # XML formats:
    # <b>language:</b>&nbsp;[name|<a>name</a>]
    # Name formats:
    # name1, name2
    vernacular_html <-
      xml2::xml_find_all(
        xml,
        "(//h2/span[@id='Vernacular_names']/parent::*/following-sibling::div)[1]")
    languages_html <- xml2::xml_find_all(vernacular_html, xpath = "b")
    languages <- gsub("\\s*:\\s*", "", sapply(languages_html, xml2::xml_text))
    names_html <-
      xml2::xml_find_all(
        vernacular_html,
        "b[not(following-sibling::*[1][self::a])]/following-sibling::text()[1] | b/following-sibling::*[1][self::a]/text()")
    common_names <- gsub("^\\s*", "", sapply(names_html, xml2::xml_text))
    result$common_names <-
      mapply(list, name = common_names,
             language = languages, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  ## classification
  if ("classification" %in% types) {
    txt <- xml2::read_html(json$parse$text[[1]])
    html <- xml2::xml_text(
      xml2::xml_find_first(txt, "//td[contains(text(), \"Classification\")]/p"))
    html <- strsplit(html, "\n")[[1]]
    labels <-
      vapply(html, function(z) strsplit(z, ":")[[1]][1], "", USE.NAMES = FALSE)
    values <-
      vapply(html, function(z) strsplit(z, ":")[[1]][2], "", USE.NAMES = FALSE)
    values <- gsub("^\\s+|\\s+$", "", values)
    result$classification <- mapply(list, rank = labels, name = values,
                                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  return(result)
}
