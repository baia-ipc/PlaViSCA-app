# Helper function for creating safe cache keys
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

# Create a cache key for filtered gene expression inputs
filtered_gene_exp_cache_key <- function(input) {
  get_field <- function(name) {
    if (is.null(input)) {
      return(NULL)
    }
    # If input is a list-like object (reactive input is a list proxy), try list access
    if (is.list(input) && !is.null(input[[name]])) {
      return(input[[name]])
    }
    # Otherwise try direct [[ access and catch errors
    tryCatch(
      input[[name]],
      error = function(e) NULL
    )
  }

  make_cache_key(
    "filtered_gene_exp",
    get_field("study_label_exp"),
    get_field("host_species_exp"),
    get_field("life_cycle_stage_exp"),
    get_field("treatment_exp"),
    get_field("parasite_stage_exp"),
    get_field("sample_type_exp"),
    get_field("strain_exp"),
    get_field("hour_post_invasion_exp"),
    get_field("sequencer_exp"),
    get_field("sc_technology_exp")
  )
}

# Helper function to sync data types between tabs
sync_data_type <- function(
    session,
    from_input,
    to_input,
    new_value,
    global_loading,
    delay = 1.5) {
  global_loading(TRUE)
  updateRadioButtons(
    session,
    to_input,
    selected = new_value
  )
  later(
    function() {
      tryCatch(
        {
          global_loading(FALSE)
        },
        error = function(e) {
          # Silent error handling
        }
      )
    },
    delay
  )
}

# Helper function to create data type
create_data_type_sync_observer <- function(
    input,
    session,
    input_name,
    target_input,
    expected_tabset,
    global_loading) {
  observeEvent(
    input[[input_name]],
    {
      if (
        !is.null(input[[input_name]]) &&
          !is.null(input$tabset) &&
          input$tabset == expected_tabset
      ) {
        sync_data_type(
          session = session,
          from_input = input_name,
          to_input = target_input,
          new_value = input[[input_name]],
          global_loading = global_loading
        )
      }
    },
    ignoreInit = TRUE
  )
}

# Helper function to format species names with italics
format_species_name <- function(species_names) {
  scientific_species <- c(
    "Anopheles freeborni",
    "Anopheles stephensi",
    "Anopheles dirus",
    "Saimiri boliviensis",
    "Aotus nancymaae",
    "Homo sapiens"
  )

  # Apply italic formatting to scientific names
  formatted_names <- ifelse(
    species_names %in% scientific_species,
    paste0("<i>", species_names, "</i>"),
    species_names
  )

  return(formatted_names)
}

# Function that builds the full Metadata tab
create_meta_filter_row <- function(suffix = "") {
  tagList(
    # First row (5 boxes)
    fluidRow(
      class = "meta-filter",
      style = "display: flex; flex-wrap: wrap; justify-content: space-between;",
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("study_label", suffix),
          "Study label:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("sample_type", suffix),
          "Sample type:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("host_species", suffix),
          "Host species:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }'),
            render = I(
              '{
              option: function(item, escape) {
                return "<div>" + item.label + "</div>";
              },
              item: function(item, escape) {
                return "<div>" + item.label + "</div>";
              }
            }'
            )
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("life_cycle_stage", suffix),
          "Life cycle stage:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("treatment", suffix),
          "Treatment:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    ),

    # Second row (5 boxes)
    fluidRow(
      class = "meta-filter",
      style = "display: flex; flex-wrap: wrap; justify-content: space-between;",
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("parasite_stage", suffix),
          "Parasite stage:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("strain", suffix),
          "Strain:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("hour_post_invasion", suffix),
          "Hour post invasion:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("sequencer", suffix),
          "Sequencer:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),
      column(
        width = 2,
        selectizeInput(
          inputId = paste0("sc_technology", suffix),
          "Single-cell technology:",
          choices = c(""),
          multiple = TRUE,
          options = list(
            placeholder = "Select options...",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    )
  )
}


# Navigate tabsetpanel
navigate_tabset <- function(session, menu_id, tab_name = NULL) {
  shinyjs::hide("home_section")
  shinyjs::hide("about_section")
  shinyjs::show("analysis_section")

  js_code <- sprintf(
    "
    $('.menu-link').removeClass('active');
    $('#%s').addClass('active');
  ",
    menu_id
  )
  shinyjs::runjs(js_code)

  if (!is.null(tab_name)) {
    updateTabsetPanel(session, inputId = "tabset", selected = tab_name)
  }
}


fun_filter_df <- function(data, selections) {
  if (is.null(selections) || length(selections) == 0) {
    return(data)
  }

  # Parse selections safely
  parts <- strsplit(selections, "__", fixed = TRUE)
  valid_parts <- Filter(function(x) length(x) == 2, parts)
  if (length(valid_parts) == 0) {
    return(data)
  }

  # Build filter list
  filter_list <- lapply(
    split(valid_parts, vapply(valid_parts, `[[`, character(1), 1)),
    function(lst) vapply(lst, `[[`, character(1), 2)
  )

  keep_rows <- rep(TRUE, nrow(data))
  for (colname in names(filter_list)) {
    if (colname %in% names(data)) {
      keep_rows <- keep_rows & data[[colname]] %in% filter_list[[colname]]
    } else {
      warning(sprintf(
        "Column '%s' not found in data; skipping filter.",
        colname
      ))
    }
  }

  # Always return with the same columns as input
  out <- data[keep_rows, , drop = FALSE]
  if (nrow(out) == 0) {
    out <- data[0, , drop = FALSE]
  }

  # subset the full expression matrix by row without re-running filters.
  attr(out, "row_idx") <- which(keep_rows)
  return(out)
}


# Helper function to apply secondary filters
apply_secondary_filters <- function(data, inputs, keep_gene_matrix = TRUE, genes = NULL) {
  # Filter out rows with invalid RNA data only if these columns exist
  filtered <- data
  if ("n_feature_rna" %in% names(data) && "n_count_rna" %in% names(data)) {
    filtered <- filtered %>%
      filter(n_feature_rna > 0 & n_count_rna > 0)
  }

  # Select relevant columns (keep all existing columns if they don't match the expected metadata columns)
  metadata_cols <- c(
    "study_label",
    "host_species",
    "life_cycle_stage",
    "treatment",
    "parasite_stage",
    "sample_type",
    "geographic_location",
    "sc_technology",
    "sequencer",
    "strain",
    "day_post_infection",
    "hour_post_invasion",
    "biological_replicate",
    "n_count_rna",
    "n_feature_rna"
  )

  # For gene expression data, keep all columns (metadata + gene expression)
  # For regular metadata, select only metadata columns
  available_metadata_cols <- intersect(metadata_cols, names(filtered))
  non_metadata_cols_count <- length(setdiff(names(filtered), metadata_cols))

  if (!keep_gene_matrix) {
    # If caller provided a genes vector, use it to explicitly drop gene columns
    if (!is.null(genes) && length(genes) > 0) {
      # Drop any columns that match the provided gene list
      keep_cols <- setdiff(names(filtered), genes)
      keep_cols <- intersect(keep_cols, names(filtered))
      # Ensure metadata columns are preserved
      keep_cols <- unique(c(keep_cols, available_metadata_cols))
      filtered <- filtered %>% select(any_of(keep_cols))
    } else {
      # Fallback: select metadata columns only
      filtered <- filtered %>% select(any_of(metadata_cols))
    }
  } else {
    if (non_metadata_cols_count > 10) {
      filtered <- filtered
    } else {
      filtered <- filtered %>% select(any_of(metadata_cols))
    }
  }

  # Dynamically apply filters for each non-empty input
  for (col in names(inputs)) {
    values <- inputs[[col]]
    if (!is.null(values) && length(values) > 0 && col %in% names(filtered)) {
      filtered <- filtered %>% filter(.data[[col]] %in% values)
    }
  }

  return(filtered)
}


# Get genes annotation
genes <- function(data, gff_data, pattern = "pvp") {
  data %>%
    colnames() %>%
    grep(pattern, ., ignore.case = TRUE, value = TRUE) %>%
    as.data.frame() %>%
    setNames("gene") %>%
    left_join(gff_data, by = c("gene" = "id")) %>%
    mutate(name = coalesce(name, gene)) %>%
    select(gene, name)
}

# Get gene column
get_gene_choices <- function(genes_df, checked) {
  if (is.null(genes_df) || nrow(genes_df) == 0) {
    return(NULL)
  }

  if (checked) {
    genes_df$gene
  } else {
    genes_df %>%
      filter(!str_detect(name, "^PVP01")) %>%
      pull(name)
  }
}

# Get gene id
get_gene_id <- function(gene_name, gene_names) {
  if (is.null(gene_name) || is.na(gene_name) || gene_name == "") {
    return(NA)
  }

  if (gene_name %in% gene_names$name) {
    return(gene_names$gene[gene_names$name == gene_name])
  } else {
    return(NA)
  }
}


# Generate UI subBlock in gene expression
subBlock <- function(i) {
  tagList(
    fluidRow(
      class = "gene-select-input",
      column(
        width = 6,
        selectizeInput(
          paste0("genes_exp_", i),
          label = "Gene Name",
          choices = NULL
        ),
        column(
          width = 2,
          class = "switch_exp",
          tags$label(
            class = "switch",
            tags$input(
              id = paste0("check_exp_", i),
              type = "checkbox",
              class = "toggle_exp"
            ),
            tags$span(class = "slider")
          )
        ),
        column(
          width = 4,
          id = paste0("gene_link_exp_", i),
          class = "gene_link_exp",
          tags$div(
            id = paste0("gene_link_ui_exp_", i),
            class = "gene_link_ui_exp",
            tags$a(
              id = paste0("plasmo_link_exp_", i),
              href = "#",
              target = "_blank",
              "🔬 PlasmoDB"
            ),
            span("|"),
            tags$a(
              id = paste0("ncbi_link_exp_", i),
              href = "#",
              target = "_blank",
              "🧬 NCBI"
            )
          )
        )
      ),
      column(
        width = 6,
        selectInput(
          paste0("group_exp_", i),
          "Group by",
          choices = group_choices,
          selected = if (!is.null(group_choices) && length(group_choices) > 0) group_choices[1] else "life_cycle_stage"
        )
      )
    ),
    fluidRow(
      width = 12,
      fluidRow(withSpinner(plotlyOutput(paste0("gene_exp_plotly_", i)), type = 6, size = 0.5, color = "#3498db")),
      fluidRow(
        class = "btn-add-rm-container",
        actionButton(
          paste0("remove_btn_", i),
          label = icon("minus"),
          class = "btn btn-danger btn_remove"
        ),
        actionButton(
          paste0("add_btn_", i),
          label = icon("plus"),
          class = "btn btn-success btn_add"
        )
      )
    )
  )
}

# Create new block helper
newBlock <- function(new_count, counter, choice_exp, dynamicBlock, session) {
  counter(new_count)
  insertUI(
    selector = "#gene_exp_container",
    where = "beforeEnd",
    ui = column(
      width = 6,
      id = paste0("dynamic_col_", new_count),
      dynamicBlock(new_count)
    )
  )

  shinyjs::delay(100, {
    updateSelectizeInput(
      session,
      inputId = paste0("genes_exp_", new_count),
      choices = choice_exp,
      server = TRUE
    )
  })
}


# Generate df for gene_exp0
df_exp0 <- function(gene_data, gene_input, group_input, check_exp, full_data = NULL, cache_env = NULL) {
  start_time <- Sys.time()
  df_full <- if (!is.null(full_data)) full_data else gene_data$data
  # If the filtered metadata provided row indices, subset the full matrix to those rows
  row_idx <- NULL
  if (!is.null(gene_data) && !is.null(gene_data$data)) {
    row_idx <- attr(gene_data$data, "row_idx")
  }
  if (!is.null(row_idx) && length(row_idx) > 0) {
    # subset rows of the full expression matrix to match filtered metadata
    df_full <- df_full[row_idx, , drop = FALSE]
  }
  genes <- gene_data$genes

  # Determine the gene column to use
  gene_exp <- if (!check_exp) {
    get_gene_id(gene_input, genes)
  } else {
    gene_input
  }

  # Identify gene columns from the genes annotation; fall back to a pattern
  gene_cols <- character(0)
  if (!is.null(genes) && nrow(genes) > 0 && "gene" %in% colnames(genes)) {
    gene_cols <- genes$gene
  }

  # Identify metadata columns (everything that's not a gene column)
  meta_cols <- setdiff(colnames(df_full), gene_cols)

  # Remove embedding coordinate columns
  embed_patterns <- c("^umap", "^pca", "^t_sne", "^tsne", "^tSNE")
  embed_regex <- paste(embed_patterns, collapse = "|")
  meta_cols <- meta_cols[!grepl(embed_regex, meta_cols, ignore.case = TRUE)]

  # Ensure the grouping column is present
  if (!(group_input %in% meta_cols)) {
    meta_cols <- unique(c(meta_cols, group_input))
  }

  # Build cache key for metadata and check cache first (if provided)
  row_idx_len <- NA
  if (!is.null(gene_data) && !is.null(gene_data$data)) row_idx_len <- length(attr(gene_data$data, "row_idx"))
  cache_key_meta <- make_cache_key("slim_meta", row_idx_len)
  if (!is.null(cache_env) && exists(cache_key_meta, envir = cache_env, inherits = FALSE)) {
    slim_meta <- cache_env[[cache_key_meta]]
    message(sprintf("df_exp0: meta cache hit key=%s", cache_key_meta))
  } else {
    # Build slim dataframe: metadata columns only
    slim_meta <- df_full %>% select(any_of(meta_cols))
    if (!is.null(cache_env)) {
      try(cache_env[[cache_key_meta]] <- slim_meta, silent = TRUE)
    }
  }

  # Build cache key for full slim_df and check cache first (if provided)
  cache_key <- make_cache_key("slim_df", gene_exp, group_input, row_idx_len)
  if (!is.null(cache_env) && exists(cache_key, envir = cache_env, inherits = FALSE)) {
    cached <- cache_env[[cache_key]]
    message(sprintf("df_exp0: cache hit key=%s", cache_key))
    return(cached)
  }

  # Add gene expression column
  # Extract gene vector first to avoid copying the whole matrix
  gene_vec <- if (!is.null(gene_exp) && gene_exp %in% colnames(df_full)) df_full[[gene_exp]] else rep(NA, nrow(df_full))
  slim_meta$gene_expr <- gene_vec

  # Free the large full dataframe
  obj_size <- object.size(slim_meta)
  # Free the potentially large full matrix reference
  if (is.null(full_data)) rm(df_full)
  invisible(gc(verbose = FALSE))

  if (!is.null(cache_env)) {
    try(cache_env[[cache_key]] <- slim_meta, silent = TRUE)
  }

  return(slim_meta)
}

# Generate df for gene_expi
df_expi <- function(id, input, gene_data, get_gene_id, full_data = NULL, cache_env = NULL) {
  reactive({
    start_time <- Sys.time()
    tryCatch(
      {
        req(
          input[[paste0("genes_exp_", id)]],
          input[[paste0("group_exp_", id)]]
        )
        df_full <- if (!is.null(full_data)) full_data else gene_data$data
        # Similarly subset rows to match filtered metadata if available
        row_idx <- NULL
        if (!is.null(gene_data) && !is.null(gene_data$data)) {
          row_idx <- attr(gene_data$data, "row_idx")
        }
        if (!is.null(row_idx) && length(row_idx) > 0) {
          df_full <- df_full[row_idx, , drop = FALSE]
        }

        # Check_exp input
        check_exp_input <- input[[paste0("check_exp_", id)]]
        if (is.null(check_exp_input)) {
          check_exp_input <- FALSE
        }

        gene_exp <- if (!check_exp_input) {
          get_gene_id(input[[paste0("genes_exp_", id)]], gene_data$genes)
        } else {
          input[[paste0("genes_exp_", id)]]
        }

        # Validate gene_exp is not NULL or empty
        if (is.null(gene_exp) || is.na(gene_exp) || gene_exp == "") {
          return(NULL)
        }

        # Identify gene columns from the genes annotation
        gene_cols <- character(0)
        if (!is.null(gene_data$genes) && nrow(gene_data$genes) > 0 && "gene" %in% colnames(gene_data$genes)) {
          gene_cols <- gene_data$genes$gene
        }

        # Metadata columns are columns that are not gene expression columns
        meta_cols <- setdiff(colnames(df_full), gene_cols)

        # Remove embedding coordinate columns to keep the slim dataframe small
        embed_patterns <- c("^umap", "^pca", "^t_sne", "^tsne", "^tSNE")
        embed_regex <- paste(embed_patterns, collapse = "|")
        meta_cols <- meta_cols[!grepl(embed_regex, meta_cols, ignore.case = TRUE)]

        group_col <- input[[paste0("group_exp_", id)]]

        # Ensure the grouping column is present in metadata
        if (!(group_col %in% meta_cols)) {
          meta_cols <- unique(c(meta_cols, group_col))
        }

        # Build cache key for metadata and check cache first (if provided)
        row_idx_len <- NA
        if (!is.null(gene_data) && !is.null(gene_data$data)) row_idx_len <- length(attr(gene_data$data, "row_idx"))
        cache_key_meta <- make_cache_key("slim_meta", row_idx_len)
        if (!is.null(cache_env) && exists(cache_key_meta, envir = cache_env, inherits = FALSE)) {
          slim_df <- cache_env[[cache_key_meta]]
          message(sprintf("df_expi(id=%s): meta cache hit key=%s", id, cache_key_meta))
        } else {
          # Build slim dataframe: metadata columns only
          slim_df <- df_full %>% select(any_of(meta_cols))
          if (!is.null(cache_env)) {
            try(cache_env[[cache_key_meta]] <- slim_df, silent = TRUE)
          }
        }

        # Build cache key for full slim_df and check cache first (if provided)
        cache_key <- make_cache_key("slim_df", gene_exp, group_col, row_idx_len, id)
        if (!is.null(cache_env) && exists(cache_key, envir = cache_env, inherits = FALSE)) {
          message(sprintf("df_expi(id=%s): cache hit key=%s", id, cache_key))
          return(cache_env[[cache_key]])
        }

        # Add gene expression column
        # Extract gene vector first to avoid copying the big matrix
        gene_vec <- if (gene_exp %in% colnames(df_full)) df_full[[gene_exp]] else rep(NA, nrow(df_full))
        slim_df$gene_expr <- gene_vec

        # Free the full dataframe
        obj_size <- object.size(slim_df)
        if (is.null(full_data)) rm(df_full)
        invisible(gc(verbose = FALSE))

        dur <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        # message(sprintf(
        #   "df_expi(id=%s): rows=%d cols=%d size=%s dur=%.3fs",
        #   id, nrow(slim_df), ncol(slim_df), format(obj_size, units = "auto"), dur
        # ))

        if (!is.null(cache_env)) try(cache_env[[cache_key]] <- slim_df, silent = TRUE)

        return(slim_df)
      },
      error = function(e) {
        message("Error in df_expi for id ", id, ": ", e$message)
        return(NULL)
      }
    )
  })
}

# Non-reactive variant of df_expi for headless testing and batch runs.
df_expi_nr <- function(id, input_list, gene_data, get_gene_id, full_data = NULL, cache_env = NULL) {
  start_time <- Sys.time()

  # Emulate req by checking required fields
  gene_input <- input_list[[paste0("genes_exp_", id)]]
  group_input <- input_list[[paste0("group_exp_", id)]]
  if (is.null(gene_input) || is.null(group_input)) {
    return(NULL)
  }

  df_full <- if (!is.null(full_data)) full_data else gene_data$data

  # Subset rows if row_idx present
  row_idx <- NULL
  if (!is.null(gene_data) && !is.null(gene_data$data)) {
    row_idx <- attr(gene_data$data, "row_idx")
  }
  if (!is.null(row_idx) && length(row_idx) > 0) {
    df_full <- df_full[row_idx, , drop = FALSE]
  }

  # check_exp
  check_exp_input <- input_list[[paste0("check_exp_", id)]]
  if (is.null(check_exp_input)) check_exp_input <- FALSE

  gene_exp <- if (!check_exp_input) get_gene_id(gene_input, gene_data$genes) else gene_input
  if (is.null(gene_exp) || is.na(gene_exp) || gene_exp == "") {
    return(NULL)
  }

  # Identify gene columns from annotation
  gene_cols <- character(0)
  if (!is.null(gene_data$genes) && nrow(gene_data$genes) > 0 && "gene" %in% colnames(gene_data$genes)) {
    gene_cols <- gene_data$genes$gene
  }

  meta_cols <- setdiff(colnames(df_full), gene_cols)
  embed_patterns <- c("^umap", "^pca", "^t_sne", "^tsne", "^tSNE")
  embed_regex <- paste(embed_patterns, collapse = "|")
  meta_cols <- meta_cols[!grepl(embed_regex, meta_cols, ignore.case = TRUE)]

  group_col <- group_input
  if (!(group_col %in% meta_cols)) meta_cols <- unique(c(meta_cols, group_col))

  # Build cache key for metadata and check cache first (if provided)
  row_idx_len <- NA
  if (!is.null(gene_data) && !is.null(gene_data$data)) row_idx_len <- length(attr(gene_data$data, "row_idx"))
  cache_key_meta <- make_cache_key("slim_meta", row_idx_len)
  if (!is.null(cache_env) && exists(cache_key_meta, envir = cache_env, inherits = FALSE)) {
    slim_df <- cache_env[[cache_key_meta]]
    message(sprintf("df_expi_nr(id=%s): meta cache hit key=%s", id, cache_key_meta))
  } else {
    # Build slim dataframe: metadata columns only
    slim_df <- df_full %>% select(any_of(meta_cols))
    if (!is.null(cache_env)) {
      try(cache_env[[cache_key_meta]] <- slim_df, silent = TRUE)
    }
  }

  # Caching for full slim_df
  cache_key <- make_cache_key("slim_df", gene_exp, group_col, row_idx_len, id)
  if (!is.null(cache_env) && exists(cache_key, envir = cache_env, inherits = FALSE)) {
    message(sprintf("df_expi_nr(id=%s): cache hit key=%s", id, cache_key))
    return(cache_env[[cache_key]])
  }

  gene_vec <- if (gene_exp %in% colnames(df_full)) df_full[[gene_exp]] else rep(NA, nrow(df_full))
  slim_df$gene_expr <- gene_vec

  obj_size <- object.size(slim_df)
  if (is.null(full_data)) rm(df_full)
  invisible(gc(verbose = FALSE))

  dur <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  # message(sprintf("df_expi_nr(id=%s): rows=%d cols=%d size=%s dur=%.3fs", id, nrow(slim_df), ncol(slim_df), format(obj_size, units = "auto"), dur))

  if (!is.null(cache_env)) try(cache_env[[cache_key]] <- slim_df, silent = TRUE)

  return(slim_df)
}

# Set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# Generate metadata plot
meta_plotly <- function(data, x_var, x_label, y_label, plt_type = "bar") {
  if (!x_var %in% colnames(data)) {
    stop(paste("Column", x_var, "not found in data"))
  }

  # Remove rows with missing values in the x_var column
  data <- data %>%
    filter(!is.na(.data[[x_var]]) & .data[[x_var]] != "")

  # Check if we have any data left after filtering
  if (nrow(data) == 0) {
    return(
      plot_ly() %>%
        add_annotations(
          text = paste("No data available for", x_label),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5
        ) %>%
        layout(
          xaxis = list(title = x_label),
          yaxis = list(title = y_label),
          showlegend = FALSE
        )
    )
  }

  data_summary <- data %>%
    group_by(.data[[x_var]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(count)

  # Format species names with italics if this is host_species column
  if (x_var == "host_species") {
    data_summary$formatted_labels <- format_species_name(data_summary[[x_var]])
  } else {
    data_summary$formatted_labels <- data_summary[[x_var]]
  }

  data_summary[[x_var]] <- factor(
    data_summary[[x_var]],
    levels = data_summary[[x_var]]
  )

  plot_ly(
    data_summary,
    x = ~count,
    y = ~formatted_labels,
    color = ~ .data[[x_var]],
    colors = brewer.pal(
      n = length(unique(data_summary[[x_var]])),
      name = "Set3"
    ),
    type = plt_type
  ) %>%
    layout(
      xaxis = list(title = x_label),
      yaxis = list(
        title = y_label,
        ticklen = 5,
        tickwidth = 2,
        tickcolor = "transparent"
      ),
      showlegend = FALSE,
      barmode = "group",
      bargap = 0.1,
      margin = list(l = 180, r = 50, t = 50, b = 80)
    )
}

# Generate plot for RNA and feature count
feature_plot <- function(df, x, y, color, y_title, title) {
  req(df)
  plot_ly(
    data = df,
    x = ~ get(x),
    y = ~ get(y),
    color = ~ get(color),
    colors = colors,
    type = "violin",
    box = list(visible = TRUE),
    meanline = list(visible = TRUE),
    jitter = 0.4
  ) %>%
    layout(
      xaxis = list(title = x),
      yaxis = list(title = y_title),
      legend = list(title = list(text = color))
    )
}

# Generate Elbow plot for PCA
elbow_plot <- function(pca_df, selected_study) {
  req(pca_df)

  pca_df <- pca_df %>%
    filter(study == !!selected_study)

  plot_ly(
    data = pca_df,
    x = ~pc,
    y = ~std,
    type = "scatter",
    mode = "lines+markers",
    marker = list(size = 8, color = "red"),
    line = list(color = "blue"),
    text = ~ paste("PCA:", pc, "<br>Stdv:", std),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = "PCA"),
      yaxis = list(title = "Standard Deviation"),
      showlegend = FALSE
    )
}

# Generate plot for top variable genes
var_genes_plot <- function(df, study, top_n = 10) {
  req(df)

  df <- df %>%
    filter(study == !!study)

  # Calculate counts AFTER filtering by study
  counts <- table(df$variable)
  var_count <- counts["TRUE"]
  nonvar_count <- counts["FALSE"]

  # Create separate datasets for each category
  non_variable_genes <- df %>% filter(variable == FALSE)
  variable_genes <- df %>% filter(variable == TRUE)

  top_hvf <- variable_genes %>%
    arrange(rank) %>%
    head(top_n)

  # Remove top genes from regular variable genes to avoid overlap
  regular_variable_genes <- variable_genes %>%
    anti_join(top_hvf, by = "name")

  plot_ly() %>%
    # Non-variable genes (black)
    add_trace(
      data = non_variable_genes,
      x = ~mean,
      y = ~variance.standardized,
      type = "scatter",
      mode = "markers",
      marker = list(size = 5, opacity = 0.7, color = "black"),
      text = ~name,
      hovertemplate = paste(
        "Gene: %{text}<br>",
        "Mean: %{x}<br>",
        "Variance: %{y}<extra></extra>"
      ),
      name = paste0("Non-variable (n = ", nonvar_count, ")"),
      showlegend = TRUE
    ) %>%
    # Regular variable genes (red)
    add_trace(
      data = regular_variable_genes,
      x = ~mean,
      y = ~variance.standardized,
      type = "scatter",
      mode = "markers",
      marker = list(size = 5, opacity = 0.7, color = "red"),
      text = ~name,
      hovertemplate = paste(
        "Gene: %{text}<br>",
        "Mean: %{x}<br>",
        "Variance: %{y}<extra></extra>"
      ),
      name = paste0("Variable (n = ", var_count, ")"),
      showlegend = TRUE
    ) %>%
    # Top variable genes (orange)
    add_trace(
      data = top_hvf,
      x = ~mean,
      y = ~variance.standardized,
      type = "scatter",
      mode = "markers",
      marker = list(size = 6, opacity = 1, color = "orange"),
      text = ~name,
      hovertemplate = paste(
        "Gene: %{text}<br>",
        "Mean: %{x}<br>",
        "Variance: %{y}<extra></extra>"
      ),
      name = paste("Top", top_n, "Variable"),
      showlegend = TRUE
    ) %>%
    layout(
      xaxis = list(title = "Mean Expression"),
      yaxis = list(title = "Standardized Variance"),
      legend = list(title = list(text = "<b>Variable Status</b>"))
    )
}


# Generate violin plot with jittered points overlay
gene_exp_plot <- function(data, x_col) {
  actual_col <- colnames(data)[tolower(colnames(data)) == tolower(x_col)]

  if (length(actual_col) == 0) {
    stop(paste(
      "Column",
      x_col,
      "not found in data. Available columns:",
      paste(colnames(data), collapse = ", ")
    ))
  }

  # Use the first match
  actual_col <- actual_col[1]

  # Ensure gene_expr column exists
  if (!"gene_expr" %in% colnames(data)) {
    stop("gene_expr column not found in data")
  }

  # Remove any rows with missing values to avoid plotly issues
  clean_data <- data[complete.cases(data[c(actual_col, "gene_expr")]), ]

  if (nrow(clean_data) == 0) {
    return(plotly::plot_ly() %>% plotly::layout(title = ""))
  }

  # Get unique groups
  unique_groups <- unique(clean_data[[actual_col]])
  n_groups <- length(unique_groups)

  # Use Set2 palette for better colors
  colors <- if (n_groups <= 8) {
    RColorBrewer::brewer.pal(max(3, n_groups), "Set2")[1:n_groups]
  } else {
    viridis::viridis(n_groups)
  }

  # Calculate statistics for each group for hover text
  stats_data <- clean_data %>%
    group_by(!!sym(actual_col)) %>%
    summarise(
      mean_val = round(mean(gene_expr, na.rm = TRUE), 3),
      sd_val = round(sd(gene_expr, na.rm = TRUE), 3),
      .groups = "drop"
    )

  # Merge statistics back to the main data for hover text
  clean_data <- clean_data %>%
    left_join(stats_data, by = actual_col)

  # Create violin plot
  p <- plotly::plot_ly(
    data = clean_data,
    x = clean_data[[actual_col]],
    y = ~gene_expr,
    type = "violin",
    box = list(visible = FALSE),
    meanline = list(visible = FALSE),
    color = clean_data[[actual_col]],
    colors = colors,
    showlegend = FALSE,
    points = "all",
    jitter = 0.3,
    pointpos = 0,
    text = ~ paste(
      "Group:",
      clean_data[[actual_col]],
      "<br>",
      "Expression:",
      round(gene_expr, 3),
      "<br>",
      "Mean:",
      mean_val,
      "<br>",
      "SD:",
      sd_val
    ),
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      yaxis = list(title = "Exp. value")
    )

  return(p)
}

# Generate PlasmoDB and NCBI links
gene_links <- function(gene_id) {
  plasmo_url <- paste0("https://plasmodb.org/plasmo/app/record/gene/", gene_id)
  ncbi_url <- paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_id)

  shinyjs::runjs(sprintf(
    "
    document.querySelectorAll('[id^=plasmo_link]').forEach(function(el) {
      el.href = '%s';
    });
  ",
    plasmo_url
  ))

  shinyjs::runjs(sprintf(
    "
    document.querySelectorAll('[id^=ncbi_link]').forEach(function(el) {
      el.href = '%s';
    });
  ",
    ncbi_url
  ))
}

y_col <- function(check_input, x_col, unintegrated, integrated) {
  if (check_input) {
    setdiff(unintegrated, x_col)
  } else {
    setdiff(integrated, x_col)
  }
}

# Generate color column based on input option
get_color_column <- function(color_type, gene_input, gene_data_obj) {
  switch(color_type,
    "genes" = get_gene_id(gene_input, gene_data_obj),
    "cca_snn" = gene_input,
    color_type
  )
}

# Assign colors based on the number of categories
assign_colors <- function(num_categories) {
  if (num_categories <= 8) {
    return(RColorBrewer::brewer.pal(
      n = max(num_categories, 3),
      name = "Set2"
    )[1:num_categories])
  } else {
    return(rep(
      RColorBrewer::brewer.pal(8, "Set2"),
      length.out = num_categories
    ))
  }
}


# Check if the data is categorical
is_categorical_data <- function(x) {
  is.character(x) || is.factor(x)
}

# Helper function to prepare colors for categorical data
prepare_categorical_colors <- function(df, color) {
  # Ensure the column is character for manipulation, and handle NAs
  df[[color]] <- as.character(df[[color]])
  df[[color]][is.na(df[[color]])] <- "NA (missing)"

  unique_values <- unique(df[[color]])

  # Separate "NA (missing)" to keep it at the top of the legend
  has_na <- "NA (missing)" %in% unique_values
  if (has_na) {
    actual_values <- unique_values[unique_values != "NA (missing)"]
  } else {
    actual_values <- unique_values
  }

  # Sort the actual values
  if (length(actual_values) > 0) {
    numeric_values <- suppressWarnings(as.numeric(actual_values))
    if (all(!is.na(numeric_values))) {
      # Sort numerically if all are numbers
      sorted_values <- actual_values[order(numeric_values)]
    } else {
      # Otherwise, sort alphabetically
      sorted_values <- sort(actual_values)
    }
  } else {
    sorted_values <- character(0)
  }

  # Reconstruct the final sorted levels, with NA first if it exists
  final_levels <- if (has_na) {
    c("NA (missing)", sorted_values)
  } else {
    sorted_values
  }

  # Assign colors
  num_actual_colors <- length(sorted_values)
  palette <- if (num_actual_colors > 0) {
    assign_colors(num_actual_colors)
  } else {
    character(0)
  }

  colors <- if (has_na) {
    setNames(c("#A9A9A94D", palette), final_levels)
  } else {
    setNames(palette, final_levels)
  }

  # Crucially, create a factor with the correctly ordered levels
  df[[color]] <- factor(df[[color]], levels = final_levels)

  return(list(df = df, colors = colors))
}

# Get plot dimension
get_dim <- function(plot_type) {
  list(
    pca_u = c("pca_u_1", "pca_u_2", "pca_u_3"),
    pca_i = c("pca_i_1", "pca_i_2", "pca_i_3"),
    umap_i = c("umap_i_1", "umap_i_2", "umap_i_3"),
    umap_u = c("umap_u_1", "umap_u_2", "umap_u_3"),
    tSNE = c("t_sne_1", "t_sne_2", "t_sne_3")
  )[[plot_type]]
}

# Get legend and format chart
get_legend_layout <- function(legend_title) {
  list(
    title = list(text = legend_title),
    orientation = "v",
    x = 1.02,
    xanchor = "left",
    y = 1,
    yanchor = "top",
    len = 0.5
  )
}

# Generate 2d plot
plot_2d <- function(
    df,
    x,
    y,
    color,
    legend_title,
    size,
    is_categorical,
    selected_color,
    colors) {
  # Format selected_color for display if it's host_species
  display_color <- if (color == "host_species") {
    format_species_name(selected_color)
  } else {
    selected_color
  }

  hover_text <- ~ paste(
    "Cell:",
    df$barcode,
    "<br>Cluster:",
    df$seurat_clusters,
    "<br>Study:",
    df$study_label,
    "<br>Life cycle stage:",
    df$life_cycle_stage,
    "<br>Color by",
    legend_title,
    ":",
    display_color,
    "<br>n_Features_RNA:",
    df$n_feature_rna,
    "<br>X:",
    x,
    "<br>Y:",
    y
  )

  legend_layout <- get_legend_layout(legend_title)
  df$plotly_key <- seq_len(nrow(df))

  if (is_categorical) {
    p <- plot_ly(
      data = df,
      x = x,
      y = y,
      type = "scatter",
      mode = "markers",
      color = ~ factor(df[[color]]),
      marker = list(size = size),
      text = hover_text,
      hoverinfo = "text",
      colors = colors,
      width = 1000,
      height = 700,
      source = "embedding",
      key = ~plotly_key
    ) %>%
      layout(
        xaxis = list(title = paste("X-", x)),
        yaxis = list(title = paste("Y-", y)),
        legend = legend_layout,
        font = list(size = 14),
        margin = list(l = 50, r = 50, t = 25, b = 50)
      )
  } else {
    p <- plot_ly(
      data = df,
      x = x,
      y = y,
      type = "scatter",
      mode = "markers",
      text = hover_text,
      hoverinfo = "text",
      width = 1000,
      height = 700,
      key = ~plotly_key,
      source = "embedding",
      marker = list(
        size = size,
        color = selected_color,
        colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
        showscale = TRUE,
        colorbar = legend_layout
      )
    ) %>%
      layout(
        xaxis = list(title = paste("X-", x)),
        yaxis = list(title = paste("Y-", y)),
        font = list(size = 14),
        margin = list(l = 50, r = 50, t = 25, b = 50)
      )
  }
  return(p)
}

# Generate 3d plot
plot_3d <- function(
    df,
    color,
    legend_title,
    size,
    selected_color,
    is_categorical,
    colors,
    coords) {
  x_col <- coords[1]
  y_col <- coords[2]
  z_col <- coords[3]

  x_vals <- df[[x_col]]
  y_vals <- df[[y_col]]
  z_vals <- df[[z_col]]

  # Format selected_color for display if it's host_species
  display_color <- if (
    !is.null(color) && !is.na(color) && color == "host_species"
  ) {
    format_species_name(selected_color)
  } else {
    selected_color
  }

  hover_text <- paste(
    "Cell:",
    df$barcode,
    "<br>Cluster:",
    df$seurat_clusters,
    "<br>Study:",
    df$study_label,
    "<br>Life cycle stage:",
    df$life_cycle_stage,
    "<br>Color by",
    legend_title,
    ":",
    display_color,
    "<br>n_Features_RNA:",
    df$n_feature_rna,
    "<br>X:",
    x_vals,
    "<br>Y:",
    y_vals,
    "<br>Z:",
    z_vals
  )

  legend_layout <- get_legend_layout(legend_title)

  if (is_categorical) {
    p <- plot_ly(
      x = x_vals,
      y = y_vals,
      z = z_vals,
      type = "scatter3d",
      mode = "markers",
      color = factor(df[[color]]),
      key = ~ df$id,
      source = "embedding",
      marker = list(size = as.numeric(size) * 0.4),
      colors = colors,
      text = hover_text,
      hoverinfo = "text",
      width = 1000,
      height = 700
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste("X-", x_col)),
          yaxis = list(title = paste("Y-", y_col)),
          zaxis = list(title = paste("Z-", z_col))
        ),
        legend = legend_layout,
        font = list(size = 14),
        margin = list(l = 50, r = 50, t = 25, b = 50)
      )
  } else {
    p <- plot_ly(
      x = x_vals,
      y = y_vals,
      z = z_vals,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = as.numeric(size) * 0.4,
        color = selected_color,
        colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
        showscale = TRUE,
        colorbar = legend_layout
      ),
      text = hover_text,
      hoverinfo = "text",
      width = 1000,
      height = 700,
      source = "embedding",
      key = ~ df$id
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste("X-", x_col)),
          yaxis = list(title = paste("Y-", y_col)),
          zaxis = list(title = paste("Z-", z_col))
        ),
        font = list(size = 14),
        margin = list(l = 50, r = 50, t = 25, b = 50)
      )
  }
  return(p)
}


# Main pca, umap and tsne plotting function
embedding_plot <- function(
    df,
    xaxis_input,
    yaxis_input,
    color,
    legend_title,
    size,
    plot_type = "pca_u",
    plot_dim = "2d") {
  # Validate inputs
  if (is.null(color) || is.na(color) || color == "" || !color %in% names(df)) {
    color <- "sample_type" # Safe fallback
    warning("Invalid color column, using 'sample_type' as fallback")
  }

  x <- df[[xaxis_input]]
  y <- df[[yaxis_input]]
  selected_color <- df[[color]]
  is_categorical <- is_categorical_data(selected_color)

  if (!is.numeric(selected_color)) {
    legend_title <- color
  }

  if (is_categorical) {
    prep <- prepare_categorical_colors(df, color)
    df <- prep$df
    colors <- prep$colors
  } else {
    colors <- NULL
  }

  if (plot_dim == "2d") {
    p <- plot_2d(
      df,
      x,
      y,
      color,
      legend_title,
      size,
      is_categorical,
      selected_color,
      colors
    )
  } else {
    coords <- get_dim(plot_type)
    p <- plot_3d(
      df,
      color,
      legend_title,
      size,
      selected_color,
      is_categorical,
      colors,
      coords
    )
  }

  return(p)
}
