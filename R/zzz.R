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
