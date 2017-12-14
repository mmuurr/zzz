## tz: in what time zone should the time be printed? changing this will also affect the tz_offset, if that's printed.
##     note that this can change the date if what == "date".
## tz_offset: include the tz offset? will be relative to the selected tz, which is the current (system) tz by default.
## digits: sub-second resolution; min = 0, max = 6.
iso8601 <- function(x, what = c("datetime", "timestamp", "date", "time"), tz = NULL, tz_offset = TRUE, digits = 0) {
    what <- match.arg(what)
    if(what == "datetime") what <- "timestamp"

    if(!is.null(tz) && is.na(match(tz, OlsonNames()))) {
        stop(sprintf("unrecognized tz = %s", tz))
    }
    
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
