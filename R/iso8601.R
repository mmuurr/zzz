#' @title Format a time or date to an ISO8601 string.
#' @param x a Date or POSIXt time vector.
#' @param what [string] What type of output?
#' "datetime" and "timestamp" are equivalent, "date" is just the date, and "time" is just the time (i.e. without date).
#' @param tz [Olson timezone string] The desired timezone for the output string.
#' @param tz_offset [boolean] Should the timezone offset be included in the output?
#' @param digits [integer] Subsecond digits desired in the output string, as specified by the "%OSn" format string in `base::strftime()`, where n is the number of digits.
#' @return An ISO8601-formatted character vector.
iso8601 <- function(x, what = c("datetime", "timestamp", "date", "time"), tz = NULL, tz_offset = TRUE, digits = 0) {
    what <- match.arg(what)
    if(what == "datetime") what <- "timestamp"

    if(!is.null(tz) && is.na(match(tz, OlsonNames()))) {
        stop(sprintf("unrecognized tz = %s", tz))
    }
    if(is.null(tz)) tz <- ""
    
    digits <- as.integer(digits)

    date_pattern <- "%Y-%m-%d"
    time_pattern <- paste("%H:%M:%OS", digits, sep = "")
    tz_pattern <- if(tz_offset) "%z" else ""
    
    strftime_pattern <- switch(
        what
       ,"timestamp" = sprintf("%sT%s%s", date_pattern, time_pattern, tz_pattern)
       ,"date" = date_pattern
       ,"time" = sprintf("%s%s", time_pattern, tz_pattern)
       ,stop(sprintf("unrecognized `what` = %s", what))
    )

    strftime(x, strftime_pattern, tz = tz, usetz = FALSE)
}
