# set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# function to generate plot for metadata
violin_plotly <- function(df, x, y, color, title) {
  req(df)
  plot_ly(df,
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
      yaxis = list(title = y),
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
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(size = 8, color = 'red'),
    line = list(color = 'blue'),
    text = ~paste("PCA:", pc, "<br>Stdv:", pca_std_df),
    hoverinfo = 'text'
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
generate_plotly <- function(df, xaxis_input, yaxis_input, color, size,
                            plot_type = "PCA", plot_dim = "2d") {
  req(df)
  req(xaxis_input, yaxis_input, color)
  x <- df[[xaxis_input]]
  y <- df[[yaxis_input]]
  selected_color <- df[[color]]

  if (is.character(selected_color) || is.factor(selected_color)) {
    colors <- RColorBrewer::brewer.pal(8, "Set2")
  } else {
    colors <- colorRampPalette(c("gray", "red"))(100)
  }

  if (plot_dim == "2d") {
    plot <- plot_ly(
      data = df,
      x = x,
      y = y,
      type = "scatter",
      mode = "markers",
      color = as.formula(paste0("~", color)),
      marker = list(size = size),
      text = ~ paste(
        "Cell:", id,
        "<br>", "cluster:", ident,
        "<br>", "color by", color, ":", selected_color,
        "<br>", "n_Features_RNA:", n_feature_rna,
        "<br>x:", x,
        "<br>y:", y
      ),
      hoverinfo = "text",
      colors = colors,
      width = 1000,
      height = 750
    ) %>%
      layout(
        xaxis = list(title = paste("X-", xaxis_input)),
        yaxis = list(title = paste("Y-", yaxis_input)),
        legend = list(title = list(text = color))
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  } else {
    if (plot_type == "PCA") {
      x_col <- "pc_1"
      y_col <- "pc_2"
      z_col <- "pc_3"
    } else if (plot_type == "PCA_integrated") {
      x_col <- "integratedrpca_1"
      y_col <- "integratedrpca_2"
      z_col <- "integratedrpca_3"
    } else if (plot_type == "UMAP") {
      x_col <- "umap_1"
      y_col <- "umap_2"
      z_col <- "umap_3"
    } else if (plot_type == "UMAP_unintegred") {
      x_col <- "umapunintegrated_1"
      y_col <- "umapunintegrated_2"
      z_col <- "umapunintegrated_3"
    } else if (plot_type == "tSNE") {
      x_col <- "t_sne_1"
      y_col <- "t_sne_2"
      z_col <- "t_sne_3"
    }


    plot <- plot_ly(
      data = df,
      x = ~ get(x_col),
      y = ~ get(y_col),
      z = ~ get(z_col),
      type = "scatter3d",
      mode = "markers",
      color = as.formula(paste0("~", color)),
      marker = list(size = as.numeric(size) * 0.4),
      colors = colors,
      text = ~ paste(
        "Cell:", id,
        "<br>", "cluster:", ident,
        "<br>", "color by", color, ":", selected_color,
        "<br>", "n_Features_RNA:", n_feature_rna,
        "<br>x:", x,
        "<br>y:", y
      ),
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
        legend = list(title = list(text = color))
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  }

  return(plot)
}
