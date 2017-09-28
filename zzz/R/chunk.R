#' @title Chunk a vector into a list of vectors of roughly equal length.
#'
#' @param x The vector or list to be chunked.
#' @param n The number of integers to chunk (i.e. chunk the sequence \code{1:n}).
#' @param n_chunks The number of chunks desired.
#' @param max_chunk_size The maximum desired length of any chunk.
#' @param method The algorithm to use for chunking.
#'        \code{"seq"} _seq_uentially chunks \code{x}.
#'        \code{"mod"} creates chunks where original indices are alike after modulo arithmetic.
#'        \code{"rand"} specifies that \code{x} should be uniformly and randomly distributed across chunks.
#' @return For \code{chunk}, a list with \code{x} chunked across the list entries.
#'         For \code{chunk_int}, a list with the values \code{1:n} chunked across the list entries.
#' @details Exactly one of \code{n_chunks} and \code{max_chunk_size} should be \code{NULL} (to specify both as non-\code{NULL} is an error).
chunk <- function(x, n_chunks = NULL, max_chunk_size = NULL, method = c("seq", "mod", "rand")) {
    if((is.null(n_chunks) && is.null(max_chunk_size)) ||
       (!is.null(n_chunks) && !is.null(max_chunk_size))) {
        stop("exactly one of 'n_chunks' or 'max_chunk_size' must be non-NULL")
    }
    
    method <- match.arg(method)

    if(is.null(n_chunks))
        n_chunks <- ceiling(length(x) / max_chunk_size)
    
    split_iix <- (seq_along(x) - 1) %% n_chunks
    
    if(identical(method, "seq")) {
        split_iix <- sort(split_iix)
    } else if(identical(method, "rand") && length(split_iix) > 1) {
            split_iix <- sample(split_iix)
    }
    
    split(x, split_iix)
}

#' @rdname chunk
chunk_int <- function(n, n_chunks = NULL, max_chunk_size = NULL, method = c("seq", "mod", "rand")) {
    chunk(seq_len(n), n_chunks, max_chunk_size, method)
}
