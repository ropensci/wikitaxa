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
#'  \item labels - data.frame with columns: language, value
#'  \item descriptions - data.frame with columns: language, value
#'  \item aliases - data.frame with columns: language, value
#'  \item sitelinks - data.frame with columns: site, title
#'  \item claims - data.frame with columns: claims, property_value,
#'  property_description, value (comma separted values in string)
#' }
#'
#' `wt_data_id` gets the Wikidata ID for the searched term, and
#' returns the ID as character
#'
#' @details Note that `wt_data` can take a while to run since when fetching
#' claims it has to do so one at a time for each claim
#'
#' You can search things other than taxonomic names with `wt_data` if you
#' like
#' @examples \dontrun{
#' # search by taxon name
#' # wt_data("Mimulus alsinoides")
#'
#' # choose which properties to return
#' wt_data("Mimulus foliatus", property = c("P846", "P815"))
#'
#' # get a taxonomic identifier
#' wt_data_id("Mimulus foliatus")
#' # the id can be passed directly to wt_data()
#' # wt_data(wt_data_id("Mimulus foliatus"))
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
      property = paste0(unique(z$mainsnak$property), collapse = ","),
      fetch_property(unique(z$mainsnak$property)),
      value = {
        if (inherits(z$mainsnak$datavalue$value, "data.frame")) {
          paste0(z$mainsnak$datavalue$value$`numeric-id`, collapse = ",")
        } else {
          paste0(z$mainsnak$datavalue$value, collapse = ",")
        }
      }
    )
  })
}
