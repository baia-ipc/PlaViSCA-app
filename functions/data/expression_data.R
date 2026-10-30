# The expression data of each data type, loaded once per R process.
# =============================================================================
# expression_data(type) gives the expression matrix of a data type
# ("normalize" or "raw") and its gene table (genes()), shared by every
# session. The normalized data are loaded at startup (global.R), the raw
# data when a session first asks for them.
# =============================================================================

EXPRESSION_DATA <- new.env(parent = emptyenv())

# list(expression = the matrix, genes = genes()) of `type`. Stops with the
# message of the checks when the file does not match the metadata.
expression_data <- function(type) {
    if (is.null(EXPRESSION_DATA[[type]])) {
        load_expression_data(type)
    }
    loaded <- EXPRESSION_DATA[[type]]
    if (inherits(loaded, "error")) {
        stop(loaded)
    }
    loaded
}

# Reads the expression matrix of `type`, checks it against the metadata
# (stop_if_app_data_invalid(), with the bibliography if given) and keeps it
# with its gene table in EXPRESSION_DATA. Returns the check results. A file
# that fails the checks is read once: the error is kept and raised again on
# every later request.
load_expression_data <- function(type, bibliography = NULL) {
    loaded <- tryCatch({
        expression <- readRDS(DATA_PATHS[[EXPRESSION_FILES[[type]]]])
        checks <- stop_if_app_data_invalid(
            mr_data = mr_data,
            expression = expression,
            bibliography = bibliography,
            expr_label = EXPRESSION_FILES[[type]]
        )
        list(
            expression = expression,
            genes = genes(expression_gene_ids(expression), gff_data),
            checks = checks
        )
    }, error = function(e) e)
    EXPRESSION_DATA[[type]] <- loaded
    if (inherits(loaded, "error")) {
        stop(loaded)
    }
    invisible(loaded$checks)
}
