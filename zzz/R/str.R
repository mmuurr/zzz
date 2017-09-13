sstr <- function(obj, ..., .name = FALSE, .newline = FALSE) {
    obj_name <- deparse(substitute(obj))
    str_output <- paste0(capture.output(str(obj, ...)), collapse = "\n")
    sprintf("%s%s%s",
            if(.name) sprintf("str(%s):\n", obj_name) else "",
            str_output,
            if(.newline) "\n" else "")
}

