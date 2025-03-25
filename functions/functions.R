# function to get gene name
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

# function get gene name
get_gene_id <- function(gene_name, gene_names) {
  if (gene_name %in% gene_names$name) {
    return(gene_names$gene[gene_names$name == gene_name])
  } else {
    return(gene_name)
  }
}

# set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# function to generate plot for for RNA and feature count
rna_feature_plot <- function(df, x, y, color, y_title, title) {
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
      title = paste("Number of", title, "per sample"),
      xaxis = list(title = x),
      yaxis = list(title = y_title),
      legend = list(title = list(text = color))
    )
}

# Elbow plot for PCA
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

# function to plot the variable genes
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
      title = "Variable Features",
      xaxis = list(title = "Mean Expression"),
      yaxis = list(title = "Standardized Variance"),
      legend = list(title = list(text = "<b>Variable Status</b>"))
    )
}

# function to plot top expression gene per cluster
top_genes_exp_plot <- function(df, default_gene = "PVP01_1460700") {
  req(df)
  plot_ly(
    data = df,
    x = ~ident,
    y = ~expression,
    color = ~ident,
    colors = colors,
    type = "violin",
    transforms = list(
      list(
        type = "filter",
        target = ~name,
        operation = "=",
        value = default_gene
      )
    )
  ) %>%
    layout(
      title = "Top expression genes per ident",
      xaxis = list(title = "Idents"),
      yaxis = list(title = "Average expression"),
      legend = list(title = list(text = "<b>Idents</b>")),
      updatemenus = list(
        list(
          type = "dropdown",
          buttons = lapply(unique(df$name), function(g) {
            list(
              method = "restyle",
              args = list("transforms[0].value", g),
              label = g
            )
          }),
          direction = "down",
          x = 0.1,
          y = 1.1
        )
      )
    )
}


# function to generate plots for PCA, UMAP, and tSNE
generate_plotly <- function(df, xaxis_input, yaxis_input, color, legend_title,
                            size, plot_type = "PCA", plot_dim = "2d") {
  x <- df[[xaxis_input]]
  y <- df[[yaxis_input]]
  selected_color <- df[[color]]
  
  if (!is.numeric(selected_color)) {
    legend_title <- color
  }
  
  is_categorical <- is.character(selected_color) || is.factor(selected_color)
  
  if (is_categorical) {
    # Handle missing values and ensure unique factor levels
    df[[color]] <- as.character(df[[color]])  
    df[[color]][is.na(df[[color]])] <- "NA (missing)"
    
    unique_categories <- unique(df[[color]])
    
    # Assign colors
    num_colors <- length(unique_categories)
    
    # Default Set2 colors plus more options (blue, purple, etc.)
    if (num_colors > 1) {
      extended_palette <- c(
        "gray",
        brewer.pal(max(min(num_colors - 1, 8), 3), "Set2"),
        brewer.pal(max(min(num_colors - 1, 12), 3), "Paired"),
        "blue", "purple", "green", "orange", "pink", "cyan"
      )
      
      # Ensure we have enough colors
      category_colors <- extended_palette[1:num_colors]
    } else {
      category_colors <- "gray"
    }
    
    
    # If more colors are needed, extend with gray
    if (length(category_colors) < num_colors) {
      category_colors <- c(category_colors,
                           rep("gray", num_colors - length(category_colors)))
    }
    
    colors <- setNames(category_colors, unique_categories)
    
    df[[color]] <- factor(df[[color]], levels = unique_categories)
  } else {
    colors <- colorRampPalette(c("gray", "red"))(100)
  }
  
  hover_text <- ~ paste(
    "Cell:", id,
    "<br>Cluster:", ident,
    "<br>Color by", legend_title, ":", selected_color,
    "<br>n_Features_RNA:", n_feature_rna,
    "<br>X:", x,
    "<br>Y:", y
  )
  
  legend_layout <- list(
    title = list(text = legend_title),
    orientation = "v",
    x = 1.02,
    xanchor = "left",
    y = 1,
    yanchor = "top",
    len = 0.5
  )
  
  if (plot_dim == "2d") {
    if (is_categorical) {
      plot <- plot_ly(
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
        height = 750
      ) %>%
        layout(
          xaxis = list(title = paste("X-", xaxis_input)),
          yaxis = list(title = paste("Y-", yaxis_input)),
          legend = legend_layout
        )
    } else {
      plot <- plot_ly(
        data = df,
        x = x,
        y = y,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = size,
          color = selected_color,
          colorscale = list(c(0, "gray"), c(1, "red")),
          showscale = TRUE,
          colorbar = legend_layout
        ),
        text = hover_text,
        hoverinfo = "text",
        width = 1000,
        height = 750
      ) %>%
        layout(
          xaxis = list(title = paste("X-", xaxis_input)),
          yaxis = list(title = paste("Y-", yaxis_input))
        )
    }
  } else {
    dim_map <- list(
      PCA = c("pc_1", "pc_2", "pc_3"),
      PCA_integrated = c(
        "integratedrpca_1", "integratedrpca_2",
        "integratedrpca_3"
      ),
      UMAP = c("umap_1", "umap_2", "umap_3"),
      UMAP_unintegred = c(
        "umapunintegrated_1", "umapunintegrated_2",
        "umapunintegrated_3"
      ),
      tSNE = c("t_sne_1", "t_sne_2", "t_sne_3")
    )
    
    coords <- dim_map[[plot_type]]
    x_col <- coords[1]
    y_col <- coords[2]
    z_col <- coords[3]
    
    if (is_categorical) {
      plot <- plot_ly(
        data = df,
        x = ~ get(x_col),
        y = ~ get(y_col),
        z = ~ get(z_col),
        type = "scatter3d",
        mode = "markers",
        color = ~ factor(df[[color]]),
        marker = list(size = as.numeric(size) * 0.4),
        colors = colors,
        text = hover_text,
        hoverinfo = "text",
        width = 1000,
        height = 750
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
      plot <- plot_ly(
        data = df,
        x = ~ get(x_col),
        y = ~ get(y_col),
        z = ~ get(z_col),
        type = "scatter3d",
        mode = "markers",
        marker = list(
          size = as.numeric(size) * 0.4,
          color = selected_color,
          colorscale = list(c(0, "gray"), c(1, "red")),
          showscale = TRUE,
          colorbar = legend_layout
        ),
        text = hover_text,
        hoverinfo = "text",
        width = 1000,
        height = 750
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = paste("X-", x_col)),
            yaxis = list(title = paste("Y-", y_col)),
            zaxis = list(title = paste("Z-", z_col))
          )
        )
    }
  }
  
  return(plot %>% config(displaylogo = FALSE, scrollZoom = TRUE))
}
