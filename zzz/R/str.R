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
