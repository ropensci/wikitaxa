tc <- function(l) Filter(Negate(is.null), l)

dt_df <- function(x) {
  (ffff <- data.table::setDF(data.table::rbindlist(x, fill = TRUE,
                                                   use.names = TRUE)))
}

search_base <- function(x, y = "wikimedia") {
  sprintf("https://%s.%s.org/w/api.php", x, y)
}

atbl <- function(x) tibble::as_tibble(x)

g_et <- function(url, args = list(), ...) {
  cli <- crul::HttpClient$new(url = url)
  res <- cli$get(query = args, ...)
  res$raise_for_status()
  jsonlite::fromJSON(res$parse("UTF-8"))
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!class(x) %in% y) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

sh <- function(query, limit, offset, utf8) {
  assert(limit, c("integer", "numeric"))
  assert(offset, c("integer", "numeric"))
  assert(utf8, "logical")
  tc(list(
    action = "query", list = "search", srsearch = query,
    utf8 = if (utf8) "" else NULL, format = "json",
    srprop = "size|wordcount|timestamp|snippet",
    srlimit = limit, sroffset = offset
  ))
}

match_ <- function(string, pattern) {
  pos <- regexec(pattern, string)
  regmatches(string, pos)[[1]]
}

strex <- function(string, pattern) {
  regmatches(string, gregexpr(pattern, string))
}
