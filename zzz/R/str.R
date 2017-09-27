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
    str_output <- paste0(capture.output(str(obj, ...)), collapse = "\n")

    header <-
        if(identical(.name, TRUE)) {
            sprintf("str(%s):\n", deparse(substitute(obj)))
        } else if(is.character(.name) && length(.name) == 1) {
            sprintf("str(%s):\n", .name)
        } else {
            ""
        }

    footer <-
        if(identical(.newline, TRUE)) {
            "\n"
        } else {
            ""
        }

    sprintf("%s%s%s", header, str_output, footer)
}
