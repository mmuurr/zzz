#' @title NA, Inf, and NaN replacement.
#' @description Replace NA, Inf, and NaN values in single vectors.
#' @param x The vector in which to replace the special value.
#' @param replacement The replacement value.
#' @return A new vector with the special value replaced by the replacement value.
#' @details \code{dplyr::coalesce} has similar functionality, but is meant to deal with parallel vectors (similar to many SQL-systems's \code{COALESCE} function).
#'          These calls are simpler and are only intended to replace values in basic vectors.
#'          \code{NA2} uses \code{is.na}, \code{Inf2} uses \code{is.infinite}, and \code{NaN2} uses \code{is.nan} as the predicate function.
#' @name Special-Value-Replacement

#' @rdname Special-Value-Replacement
NA2 <- function(x, replacement) {
    x[is.na(x)] <- replacement
    x
}

#' @rdname Special-Value-Replacement
Inf2 <- function(x, replacement) {
    x[is.infinite(x)] <- replacement
    x
}

#' @rdname Special-Value-Replacement
NaN2 <- function(x, replacement) {
    x[is.nan(x)] <- replacement
    x
}
