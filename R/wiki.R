# Wikidata ----------------

#' Wikidata taxonomy data
#'
#' @export
#' @param x (character) a taxonomic name
#' @param property (character) a property id, e.g., P486
#' @param ... curl options passed on to [httr::GET()]
#' @param language (character) two letter language code
#' @param limit (integer) records to return. Default: 10
#' @return `wt_data` searches Wikidata, and returns a list with elements:
#' \itemize{
#'  \item labels
#'  \item descriptions
#'  \item aliases
#'  \item sitelinks
#'  \item claims
#' }
#'
#' `wt_data_id` gets the Wikidata ID for the searched term, and
#' returns the ID as character
#' @examples \dontrun{
#' wt_data("Poa annua")
#' wt_data("Mimulus alsinoides")
#' wt_data("Mimulus foliatus")
#' wt_data("Mimulus foliatus", property = "P846")
#' wt_data("Mimulus foliatus", property = c("P846", "P815"))
#'
#' # get a taxonomic identifier
#' wt_data_id("Mimulus foliatus")
#'
#' wt_data(wt_data_id("Mimulus foliatus"))
#' }
wt_data <- function(x, property = NULL, ...) {
  UseMethod("wt_data")
}

#' @export
wt_data.wiki_id <- function(x, property = NULL, ...) {
  data_wiki(x, property = property, ...)
}

#' @export
wt_data.default <- function(x, property = NULL, ...) {
  x <- WikidataR::find_item(search_term = x, ...)
  if (length(x) == 0) stop("no results found", call. = FALSE)
  data_wiki(x[[1]]$id, property = property, ...)
}

#' @export
#' @rdname wt_data
wt_data_id <- function(x, language = "en", limit = 10, ...) {
  x <- WikidataR::find_item(search_term = x, language = language,
                            limit = limit, ...)
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
    sitelinks = dt_df(lapply(xx$sitelinks, function(x)
      x[names(x) %in% c('site', 'title')])),
    claims = dt_df(claims)
  )
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
