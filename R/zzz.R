tc <- function(l) Filter(Negate(is.null), l)
tcnull <- function(x) if (all(sapply(x, is.null))) NULL else x

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

dt_df <- function(x) {
  (ffff <- data.table::setDF(data.table::rbindlist(x, fill = TRUE,
                                                   use.names = TRUE)))
}

search_base <- function(x, y = "wikimedia") {
  sprintf("https://%s.%s.org/w/api.php", x, y)
}

atbl <- function(x) tibble::as_tibble(x)
