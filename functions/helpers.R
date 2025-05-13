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
              id = paste0("check_exp_", i), type = "checkbox",
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
              id = paste0("plasmo_link_exp_", i), href = "#",
              target = "_blank", "🔬 PlasmoDB"
            ),
            span("|"),
            tags$a(
              id = paste0("ncbi_link_exp_", i), href = "#",
              target = "_blank", "🧬 NCBI"
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
          selected = "Stage"
        )
      )
    ),
    fluidRow(
      width = 12,
      fluidRow(plotlyOutput(paste0("gene_exp_plotly_", i))),
      fluidRow(
        class = "btn-add-rm-container",
        actionButton(paste0("remove_btn_", i),
          label = icon("minus"),
          class = "btn btn-danger btn_remove"
        ),
        actionButton(paste0("add_btn_", i),
          label = icon("plus"),
          class = "btn btn-success btn_add"
        )
      )
    )
  )
}

# Create new block
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
df_exp0 <- function(gene_data, gene_input, group_input, check_exp) {
  df <- gene_data$data
  genes <- gene_data$genes
  gene_exp <- if (!check_exp) {
    get_gene_id(gene_input, genes)
  } else {
    gene_input
  }

  if (!(gene_exp %in% colnames(df))) {
    df$gene_expr <- NA
  } else {
    df <- df %>% mutate(gene_expr = .data[[gene_exp]])
  }

  if (!(tolower(group_input) %in% colnames(df))) {
    df[[group_input]] <- NA
  }
  return(df)
}

# Generate df for gene_expi
df_expi <- function(id, input, gene_data, get_gene_id) {
  reactive({
    req(
      input[[paste0("genes_exp_", id)]],
      input[[paste0("group_exp_", id)]]
    )

    df <- gene_data$data
    gene_exp <- if (!input[[paste0("check_exp_", id)]]) {
      get_gene_id(input[[paste0("genes_exp_", id)]], gene_data$genes)
    } else {
      input[[paste0("genes_exp_", id)]]
    }

    if (!(gene_exp %in% colnames(df))) {
      df$gene_expr <- NA
    } else {
      df <- df %>% mutate(gene_expr = .data[[gene_exp]])
    }

    group_col <- input[[paste0("group_exp_", id)]]
    if (!(tolower(group_col) %in% colnames(df))) {
      df[[group_col]] <- NA
    }
    return(df)
  })
}


# Set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# Generate metadata plot
meta_plotly <- function(data, x_var, x_label, y_label, plt_type = "bar") {
  data_summary <- data %>%
    group_by(.data[[x_var]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(count)

  data_summary[[x_var]] <- factor(data_summary[[x_var]],
    levels = data_summary[[x_var]]
  )

  plot_ly(data_summary,
    x = ~count,
    y = as.formula(paste("~", x_var)),
    color = ~ .data[[x_var]],
    colors = brewer.pal(
      n = length(unique(data_summary[[x_var]])),
      name = "Set3"
    ),
    type = plt_type
  ) %>%
    layout(
      xaxis = list(title = x_label),
      yaxis = list(title = y_label),
      showlegend = FALSE,
      barmode = "group",
      bargap = 0.1
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
elbow_plot <- function(pca_std_df) {
  req(pca_std_df)
  plot_ly(
    data = pca_std_df,
    x = ~pc,
    y = ~std,
    type = "scatter",
    mode = "lines+markers",
    marker = list(size = 8, color = "red"),
    line = list(color = "blue"),
    text = ~ paste("PCA:", pc, "<br>Stdv:", pca_std_df),
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = "PCA"),
      yaxis = list(title = "Standard Deviation"),
      showlegend = FALSE
    )
}

# Generate plot for top variable genes
var_genes_plot <- function(df, top_n = 10) {
  req(df)

  counts <- table(df$variable)
  var_count <- counts["TRUE"]
  nonvar_count <- counts["FALSE"]

  df$variable_status <- ifelse(
    df$variable,
    paste0("Variable (n = ", var_count, ")"),
    paste0("Non-variable (n = ", nonvar_count, ")")
  )

  top_hvf <- df %>%
    filter(variable == TRUE) %>%
    arrange(rank) %>%
    head(top_n)

  plot_ly() %>%
    add_trace(
      data = df,
      x = ~mean,
      y = ~variance.standardized,
      color = ~variable_status,
      colors = c("black", "red"),
      type = "scatter",
      mode = "markers",
      marker = list(size = 5, opacity = 0.7),
      text = ~name,
      hovertemplate = paste(
        "Gene: %{text}<br>",
        "Mean: %{x}<br>",
        "Variance: %{y}<extra></extra>"
      ),
      name = "Variable Genes"
    ) %>%
    add_trace(
      data = top_hvf,
      x = ~mean,
      y = ~variance.standardized,
      color = I("orange"),
      type = "scatter",
      mode = "markers",
      marker = list(size = 6, opacity = 1),
      text = ~name,
      hovertemplate = paste(
        "Gene: %{text}<br>",
        "Mean: %{x}<br>",
        "Variance: %{y}<extra></extra>"
      ),
      name = paste("Top", top_n, "Variable")
    ) %>%
    layout(
      xaxis = list(title = "Mean Expression"),
      yaxis = list(title = "Standardized Variance"),
      legend = list(title = list(text = "<b>Variable Status</b>"))
    )
}


# Generate violine gene exp plot
gene_exp_plot <- function(data, x_col) {
  plot_ly(
    data = data,
    x = ~ .data[[x_col]],
    y = ~gene_expr,
    type = "violin",
    color = ~ .data[[x_col]],
    box = list(visible = TRUE),
    meanline = list(visible = TRUE)
  ) %>%
    layout(
      xaxis = list(title = x_col),
      yaxis = list(title = "Expression"),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.1,
        yanchor = "bottom"
      )
    )
}


# Generate PlasmoDB and NCBI links
gene_links <- function(gene_id) {
  plasmo_url <- paste0("https://plasmodb.org/plasmo/app/record/gene/", gene_id)
  ncbi_url <- paste0("https://www.ncbi.nlm.nih.gov/protein/", gene_id)

  shinyjs::runjs(sprintf("
    document.querySelectorAll('[id^=plasmo_link]').forEach(function(el) {
      el.href = '%s';
    });
  ", plasmo_url))

  shinyjs::runjs(sprintf("
    document.querySelectorAll('[id^=ncbi_link]').forEach(function(el) {
      el.href = '%s';
    });
  ", ncbi_url))
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
  if (num_categories <= 12) {
    return(RColorBrewer::brewer.pal(
      n = max(num_categories, 3),
      name = "Set3"
    )[1:num_categories])
  } else {
    return(rep(RColorBrewer::brewer.pal(12, "Set3"),
      length.out = num_categories
    ))
  }
}

# Check if the data is categorical
is_categorical_data <- function(x) {
  is.character(x) || is.factor(x)
}

prepare_categorical_colors <- function(df, color) {
  df[[color]] <- as.character(df[[color]])
  df[[color]][is.na(df[[color]])] <- "NA (missing)"
  unique_categories <- unique(df[[color]])

  if ("NA (missing)" %in% unique_categories) {
    none_missing <- unique_categories[unique_categories != "NA (missing)"]
    assigned <- if (length(none_missing) > 0) {
      assign_colors(length(none_missing))
    } else {
      character(0)
    }
    colors <- setNames(c("#A9A9A94D", assigned), c("NA (missing)", none_missing))
    df[[color]] <- factor(df[[color]], levels = c("NA (missing)", none_missing))
  } else {
    assigned <- assign_colors(length(unique_categories))
    colors <- setNames(assigned, unique_categories)
    df[[color]] <- factor(df[[color]], levels = unique_categories)
  }

  return(list(df = df, colors = colors))
}

# Get plot dimension
get_dim <- function(plot_type) {
  list(
    PCA = c("pc_1", "pc_2", "pc_3"),
    PCA_integrated = c(
      "integratedrpca_1", "integratedrpca_2", "integratedrpca_3"
    ),
    UMAP = c("umap_1", "umap_2", "umap_3"),
    UMAP_unintegrated = c(
      "umapunintegrated_1", "umapunintegrated_2",
      "umapunintegrated_3"
    ),
    tSNE = c("t_sne_1", "t_sne_2", "t_sne_3")
  )[[plot_type]]
}

# Get legend and format chart
get_legend_layout <- function(legend_title) {
  list(
    title = list(text = legend_title),
    orientation = "v",
    x = 1.02, xanchor = "left",
    y = 1, yanchor = "top",
    len = 0.5
  )
}

# Generate 2d plot
plot_2d <- function(df, x, y, color, legend_title, size,
                    is_categorical, selected_color, colors) {
  hover_text <- ~ paste(
    "Cell:", df$id,
    "<br>Cluster:", df$ident,
    "<br>Color by", legend_title, ":", selected_color,
    "<br>n_Features_RNA:", df$n_feature_rna,
    "<br>X:", x,
    "<br>Y:", y
  )

  legend_layout <- get_legend_layout(legend_title)

  if (is_categorical) {
    p <- plot_ly(
      data = df, x = x, y = y,
      type = "scatter", mode = "markers",
      color = ~ factor(df[[color]]),
      marker = list(size = size),
      text = hover_text, hoverinfo = "text",
      colors = colors,
      width = 1000, height = 750,
      source = "embedding",
      key = ~id
    ) %>%
      layout(
        xaxis = list(title = paste("X-", x)),
        yaxis = list(title = paste("Y-", y)),
        legend = legend_layout
      )
  } else {
    p <- plot_ly(
      data = df, x = x, y = y,
      type = "scatter", mode = "markers",
      marker = list(
        size = size,
        color = selected_color,
        colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
        showscale = TRUE,
        colorbar = legend_layout
      ),
      text = hover_text, hoverinfo = "text",
      width = 1000, height = 750,
      source = "embedding",
      key = ~id
    ) %>%
      layout(
        xaxis = list(title = paste("X-", x)),
        yaxis = list(title = paste("Y-", y))
      )
  }
  return(p)
}


# Generate 3d plot
plot_3d <- function(df, color, legend_title, size, selected_color,
                    is_categorical, colors, coords) {
  x_col <- coords[1]
  y_col <- coords[2]
  z_col <- coords[3]

  x_vals <- df[[x_col]]
  y_vals <- df[[y_col]]
  z_vals <- df[[z_col]]

  hover_text <- paste(
    "Cell:", df$id,
    "<br>Cluster:", df$ident,
    "<br>Color by", legend_title, ":", selected_color,
    "<br>n_Features_RNA:", df$n_feature_rna,
    "<br>X:", x_vals,
    "<br>Y:", y_vals,
    "<br>Z:", z_vals
  )

  legend_layout <- get_legend_layout(legend_title)

  if (is_categorical) {
    p <- plot_ly(
      x = x_vals, y = y_vals, z = z_vals,
      type = "scatter3d", mode = "markers",
      color = factor(df[[color]]),
      marker = list(size = as.numeric(size) * 0.4),
      colors = colors,
      text = hover_text, hoverinfo = "text",
      width = 1000, height = 750,
      source = "embedding",
      key = ~ df$id
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste("X-", x_col)),
          yaxis = list(title = paste("Y-", y_col)),
          zaxis = list(title = paste("Z-", z_col))
        ),
        legend = legend_layout
      )
  } else {
    p <- plot_ly(
      x = x_vals, y = y_vals, z = z_vals,
      type = "scatter3d", mode = "markers",
      marker = list(
        size = as.numeric(size) * 0.4,
        color = selected_color,
        colorscale = list(c(0, "#0000FF4C"), c(1, "red")),
        showscale = TRUE,
        colorbar = legend_layout
      ),
      text = hover_text, hoverinfo = "text",
      width = 1000, height = 750,
      source = "embedding",
      key = ~ df$id
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste("X-", x_col)),
          yaxis = list(title = paste("Y-", y_col)),
          zaxis = list(title = paste("Z-", z_col))
        )
      )
  }
  return(p)
}


# Main pca, umap and tsne plotting function
embedding_plot <- function(df, xaxis_input, yaxis_input, color, legend_title,
                           size, plot_type = "PCA", plot_dim = "2d") {
  x <- df[[xaxis_input]]
  y <- df[[yaxis_input]]
  selected_color <- df[[color]]
  is_categorical <- is_categorical_data(selected_color)

  if (!is.numeric(selected_color)) legend_title <- color

  if (is_categorical) {
    prep <- prepare_categorical_colors(df, color)
    df <- prep$df
    colors <- prep$colors
  } else {
    colors <- NULL
  }

  if (plot_dim == "2d") {
    p <- plot_2d(
      df, x, y, color, legend_title, size, is_categorical, selected_color,
      colors
    )
  } else {
    coords <- get_dim(plot_type)
    p <- plot_3d(
      df, color, legend_title, size, selected_color,
      is_categorical, colors, coords
    )
  }
  p <- p %>%
    config(displaylogo = FALSE, scrollZoom = TRUE) %>%
    event_register("plotly_click")
  return(p)
}
