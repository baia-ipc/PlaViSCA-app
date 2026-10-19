# Checks of the server code (server/*.R) with shiny::testServer(), run in a
# separate R process by tests/test_server.R: global.R loads the packages and
# the data, as when the app starts. Prints one line per check:
#   PASS <name>   or   FAIL <name>: <message>

suppressMessages({
  library(shiny)
  source("global.R")
})

check <- function(name, expr) {
  msg <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (is.null(msg)) cat("PASS", name, "\n") else cat("FAIL ", name, ": ", gsub("\n", " ", msg), "\n", sep = "")
}

stop_unless <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)

# Runs the later() callbacks that are due after `seconds`.
wait_later <- function(seconds) {
  Sys.sleep(seconds)
  later::run_now()
}

# Filter values of a tab (as collect_filter_inputs() gives them) selecting
# the cells of one study.
study_filter <- function(study) {
  values <- setNames(vector("list", length(filter_field_keys())),
                     vapply(filter_field_keys(), get_data_column, character(1)))
  values[[get_data_column("study_label")]] <- study
  attr(values, "exclude") <- character(0)
  values
}

n_cells <- function(study) sum(mr_data$study_label == study)

STUDY <- "Sa2020"

# The data loaded as by data_loading_server(), normalized.
loaded_data <- function() {
  gene_data <- list(data = norm_df, genes = genes(norm_df, gff_data))
  list(
    gene_data = reactive(gene_data),
    data_type = reactive("normalize"),
    cached_data = list(normalize = norm_df),
    global_loading = reactiveVal(FALSE)
  )
}

# The cells of the Gene expression tab, unfiltered.
gex_cells <- function() {
  meta <- apply_secondary_filters(norm_df, study_filter(NULL), keep_gene_matrix = FALSE,
                                  genes = genes(norm_df, gff_data)$gene)
  list(data = meta, genes = genes(norm_df, gff_data), full = norm_df, row_idx = attr(meta, "row_idx"))
}

# one file of checks per server file: tests/server/<server file>.R
for (f in sort(list.files("tests/server", pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = TRUE)
}
