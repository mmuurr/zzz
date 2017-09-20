#' @title Object structure string.
#' @description Provide internal _str_ucture of an R object as a returned string.
#' @param obj Any R object.
#' @param ... Arguments passed on to \code{\link[utils]{str}}.
#' @param .name Provide obj's variable name (as passed when \code{sstr} was called)?
#'        (Adds an additional line to the output.)
#' @param .newline Trail the returned string with a newline character?
#' @return A string capturing the results of \code{\link[utils]{str(obj, ...)}}.
#' @seealso \code{\link[utils]{str}}.
sstr <- function(obj, ..., .name = FALSE, .newline = FALSE) {
    obj_name <- deparse(substitute(obj))
    str_output <- paste0(capture.output(str(obj, ...)), collapse = "\n")
    sprintf("%s%s%s",
            if(.name) sprintf("str(%s):\n", obj_name) else "",
            str_output,
            if(.newline) "\n" else "")
}

