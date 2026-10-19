# Cache keys for the per-session data caches.

# Cache key made of the given values (vectors are sorted and joined).
make_cache_key <- function(...) {
    args <- list(...)
    parts <- lapply(args, function(x) {
        if (is.null(x)) {
            return("NULL")
        } else if (length(x) > 1) {
            return(paste(sort(as.character(x)), collapse = "|"))
        } else {
            return(as.character(x))
        }
    })
    paste(parts, collapse = "__")
}
