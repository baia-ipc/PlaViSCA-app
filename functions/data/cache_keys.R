# Cache keys and the caches shared by all sessions.
# =============================================================================
# Results that depend only on the data and on a few settings (the cells
# selected by a set of filters, an embedding plot) are kept in caches of the R
# process, so that every session, and every later request with the same
# settings, gets them without recomputing. A cache holds a bounded number of
# entries; the least recently used one is dropped when it is full.
# =============================================================================

# Cache key made of the given values. Each argument is a vector or a list:
# the values of a vector are sorted, the entries of a list are named by their
# names. An absent value (NULL or empty) always gives the same key. The
# parts are joined by control characters, so that a value containing a comma
# or a bar cannot read as two values.
make_cache_key <- function(...) {
    args <- list(...)
    parts <- vapply(args, function(x) {
        if (length(x) == 0) {
            return("")
        }
        values <- vapply(seq_along(x), function(i) {
            paste(sort(as.character(x[[i]])), collapse = "\x1e")
        }, character(1))
        if (!is.null(names(x))) {
            values <- paste0(names(x), "=", values)
        }
        paste(sort(values), collapse = "\x1f")
    }, character(1))
    paste(parts, collapse = "\x1d")
}

# A cache for at most `size` entries.
shared_cache <- function(size) {
    cache <- new.env(parent = emptyenv())
    cache$size <- size
    cache$entries <- new.env(parent = emptyenv())
    cache$order <- character(0)
    cache
}

# The entry `key` of `cache`, or NULL if it has none.
cache_get <- function(cache, key) {
    if (!exists(key, envir = cache$entries, inherits = FALSE)) {
        return(NULL)
    }
    cache$order <- c(setdiff(cache$order, key), key)
    cache$entries[[key]]
}

# Stores `value` as the entry `key` of `cache` and returns it. NULL is not
# stored: it stands for "nothing to show" and costs nothing to recompute.
cache_set <- function(cache, key, value) {
    if (is.null(value)) {
        return(NULL)
    }
    cache$entries[[key]] <- value
    cache$order <- c(setdiff(cache$order, key), key)
    while (length(cache$order) > cache$size) {
        oldest <- cache$order[[1]]
        rm(list = oldest, envir = cache$entries)
        cache$order <- cache$order[-1]
    }
    value
}

# The entry `key` of `cache`, computed by `compute()` on the first request.
cache_fetch <- function(cache, key, compute) {
    value <- cache_get(cache, key)
    if (is.null(value)) {
        value <- cache_set(cache, key, compute())
    }
    value
}
