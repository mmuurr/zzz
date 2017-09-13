## For factor recoding, use forcats::fct_recode instead.
## Q: What if a `to` value is NA and nomatch = NA?
## A: The returned NA's are indistinguishable.
##    Consider adding an attribute with the unmapped indices?
matchval <- function(query, from, to, nomatch = NA, .warn_nomatch = FALSE) {

    ## Janky factor level recoding... drops levels with there is no match.
    ## Use forcats::fct_recode instead.
    # if(is.factor(query)) {
    #     levels(query) <- matchval(levels(query), from, to, nomatch)
    #     return(query)
    # }

    match_iix <- match(x, from, nomatch = NA)
    if(.warn_nomatch) warning("missing matches detected", immediate. = TRUE)
    NA2(to[match_iix], nomatch)
} 

