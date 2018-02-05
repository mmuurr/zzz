#' @title Not-In
#' @description The logical negation of R's \code{\%in\%} infix operator.
#' @name notin

#' @rdname notin
`%notin%` <- Negate(`%in%`)

#' @rdname notin
`%nin%` <- `%notin%`

#' @rdname notin
`%ni%` <- `%notin%`
