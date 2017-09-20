#' @title Maps values between vectors.
#'
#' @description The positions of \code{x} in \code{from} are used to retrieve values from \code{to}.
#'
#' @param query The source vector of values.
#' @param from The 'key' vector against which \code{query} is matched.
#' @param to The 'return' vector from which values are returned based on position.
#' @param nomatch The returned value for cases where \code{query} is unmatched in \code{from}.
#' @param .warn_nomatch Warn in cases of no match? (Without a warning, one cannot distinguish the unmatched case vs the case where \code{to} contains the value \code{nomatch}.
#'
#' @return Values in \code{to} corresponding to the indices returned by \code{match(query,from)}, or \code{nomatch}.
#'
#' @details Given an entry in \code{query} find the matching index in \code{from}.
#'          Then retrieve the value at that index in \code{to}.
#'          If an entry in \code{query} cannot be found in \code{from}, the corresponding returned value is \code{nomatch}.
#'          Take note that if \code{to} contains any \code{nomatch} values, presence of \code{nomatch} in the returnd value doesn't indicate that some \code{query} was missing in \code{from}, but one can use the \code{.warn_nomatch} case to disambiguate between these two cases.
#'
#' @section What if \code{query} is a factor?
#'          To achieve factor recoding, use \code{forcats::fct_recode} instead.
#'
#' @section TODO:
#'   \itemize{
#'     \item Consider adding an attribute to the return value with any unmapped indices?
#'   }
matchval <- function(query, from, to, nomatch = NA, .warn_nomatch = FALSE) {

    ## Janky factor level recoding... drops levels with there is no match.
    ## Use forcats::fct_recode instead.
    # if(is.factor(query)) {
    #     levels(query) <- matchval(levels(query), from, to, nomatch)
    #     return(query)
    # }

    match_iix <- match(query, from, nomatch = NA)
    if(.warn_nomatch) warning("missing matches detected", immediate. = TRUE)
    NA2(to[match_iix], nomatch)
} 

