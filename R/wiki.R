#' Wikidata taxonomy data
#'
#' @export
#' @param x (character) a taxonomic name
#' @param property (character) a property id, e.g., P486
#' @param ... curl options passed on to `httr::GET()`
#' @param language (character) two letter language code
#' @param limit (integer) records to return. Default: 10
#' @return `wt_data` searches Wikidata, and returns a list with elements:
#' \itemize{
#'  \item labels - data.frame with columns: language, value
#'  \item descriptions - data.frame with columns: language, value
#'  \item aliases - data.frame with columns: language, value
#'  \item sitelinks - data.frame with columns: site, title
#'  \item claims - data.frame with columns: claims, property_value,
#'  property_description, value (comma separated values in string)
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
#' wt_data("Mimulus alsinoides")
#'
#' # choose which properties to return
#' wt_data(x="Mimulus foliatus", property = c("P846", "P815"))
#'
#' # get a taxonomic identifier
#' wt_data_id("Mimulus foliatus")
#'
#' # the id can be passed directly to wt_data()
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
  result <- WikipediR::query(
    url = "https://www.wikidata.org/w/api.php",
    out_class = "list",
    clean_response = FALSE,
    query_param = list(
      action   = "wbsearchentities",
      type     = "item",
      language = "en",
      limit    = 10,
      search   = x
    ),
    ...
  )
  if (length(result$search) == 0) stop("no results found", call. = FALSE)
  data_wiki(result$search[[1]]$id, property = property, ...)
}

#' @export
#' @rdname wt_data
wt_data_id <- function(x, language = "en", limit = 10, ...) {
  result <- WikipediR::query(
    url = "https://www.wikidata.org/w/api.php",
    out_class = "list",
    clean_response = FALSE,
    query_param = list(
      action   = "wbsearchentities",
      type     = "item",
      language = language,
      limit    = limit,
      search   = x
    ),
    ...
  )
  out <- result$search
  out <- if (length(out) == 0) NA else out[[1]]$id
  structure(out, class = "wiki_id")
}

data_wiki <- function(x, property = NULL, ...) {
  x <- sub(x,pattern = '^([0-9]+)', replacement = 'Q\\1')
  xx <- lapply(x, function(page, ...){
    result <- WikipediR::page_content(
      domain = "wikidata.org",
      page_name = page,
      as_wikitext = TRUE,
      httr::user_agent("wikitaxa"),
      ...
    )
    out <- jsonlite::fromJSON(result$parse$wikitext[[1]])
    return(out)
  })

  if (is.null(property)) {
    claims <- create_claims(xx[[1]]$claims)
  } else{
    cl <- Filter(function(x) x$mainsnak$property %in% property, xx[[1]]$claims)
    if (length(cl) == 0) stop("No matching properties", call. = FALSE)
    claims <- create_claims(cl)
  }

  list(
    labels = dt_df(xx[[1]]$labels),
    descriptions = dt_df(xx[[1]]$descriptions),
    aliases = dt_df(xx[[1]]$aliases),
    sitelinks = dt_df(lapply(xx[[1]]$sitelinks, function(x)
      x[names(x) %in% c('site', 'title')])),
    claims = dt_df(claims)
  )
}

fetch_property <- function(x) {
  x <- sub(x,pattern = '^([0-9]+)', replacement = 'P\\1')
  x <- sub(x,pattern = '^(P[0-9]+)', replacement = 'Property:\\1')
  tmp <- lapply(x, function(page, ...){
    result <- WikipediR::page_content(
      domain = "wikidata.org",
      page_name = page,
      as_wikitext = TRUE,
      httr::user_agent("wikitaxa"),
      ...
    )
    out <- jsonlite::fromJSON(result$parse$wikitext[[1]])
    return(out)
  })

  list(
    property_value = tmp[[1]]$labels$en$value,
    property_description = tmp[[1]]$descriptions$en$value
  )
}

create_claims <- function(x) {
  lapply(x, function(z) {
    ff <- c(
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
    ff[vapply(ff, is.null, logical(1))] <- NA
    ff
  })
}
