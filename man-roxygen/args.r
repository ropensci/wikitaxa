#' @param name (character) Wiki name - as a page title, must be length 1
#' @param utf8 (logical) If `TRUE`, encodes most (but not all) non-ASCII
#' characters as UTF-8 instead of replacing them with hexadecimal escape
#' sequences. Default: `TRUE`
#' @param page ([httr::response()]) Result of [wt_wiki_page()]
#' @param types (character) List of properties to parse
#' @param tidy (logical). tidy output to data.frame's if possible.
#' Default: `FALSE`
#' @param query (character) query terms
#' @param limit (integer) number of results to return. Default: 10
#' @param offset (integer) record to start at. Default: 0
#' @param ... curl options, passed on to [httr::GET()]
