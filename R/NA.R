#' @title Missing NA values.
#'
#' @description Explicit NA values that are missing in base R.
#'
#' @details R already has \code{NA_integer_} and \code{NA_character_} for explicitly-typed NA values.
#'          \code{NA} is logical and \code{NA_real_} is double, but these aren't intuitive, so here are some additional explicit NA values to bring code into tighter alignment with the actual underlying base types.
#' @format A single (explicitly-typed) NA value.
#' @name NA_

#' @rdname NA_
NA_logical_ <- NA

#' @rdname NA_
NA_double_ <- NA_real_

