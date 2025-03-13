# set color pallet
colors <- RColorBrewer::brewer.pal(12, "Set3")

# function to generate plot for metadata
violin_plotly <- function(data, x, y, color) {
  req(data)
  plot <- plot_ly(data,
                  x = ~get(x),
                  y = ~get(y),
                  color = ~get(color),
                  colors = colors,
                  type = "violin",
                  box = list(visible = TRUE),
                  meanline = list(visible = TRUE),
                  jitter = 0.4
                  ) %>% 
    layout(title = paste(y, "vs", x),
           xaxis = list(title = x),
           yaxis = list(title = y),
           legend = list(title = list(text = color))
    )
  
  return(plot)
}

# function to generate the variable genes
var_genes_plot <- function(ndata) {
  plot_ly() %>%
    add_trace(
      data = ndata,
      x = ~mean_expr,
      y = ~std_variance,
      text = ~name,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 6, color = 'lightgray'),
      hoverinfo = 'text',
      name = paste('Non-variable: (', nrow(ndata), ')', sep = '')
    ) %>%
    add_trace(
      data = ndata[ndata$variable_genes, ],
      x = ~mean_expr,
      y = ~std_variance,
      text = ~name,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 8, color = 'blue'),
      hoverinfo = 'text',
      name = paste('Variable: (', nrow(ndata[ndata$variable_genes, ]), ')',
                   sep = '')
    ) %>%
    add_trace(
      data = ndata[ndata$top, ],
      x = ~mean_expr,
      y = ~std_variance,
      text = ~name,
      type = 'scatter',
      mode = 'markers+text',
      marker = list(size = 10, color = 'red'),
      textposition = 'top',
      hoverinfo = 'text',
      name = paste('Top Variable Genes (', nrow(ndata[ndata$top, ]), ')',
                   sep = '')
    ) %>%
    layout(
      title = "Variable Features Plot",
      xaxis = list(title = "Average Expression"),
      yaxis = list(title = "Standardized Variance"),
      showlegend = TRUE
    )
}

# function to generate plots for PCA, UMAP, and tSNE
generate_plotly <- function(data, xaxis_input, yaxis_input, color, size,
                            plot_type = "PCA", plot_dim = "2d") {
  req(data)
  colors <- colorRampPalette(brewer.pal(n = 8, name = "Set2"))(50)
  req(xaxis_input, yaxis_input, color)
  
  selected_color <- data[[color]]
  
  if (plot_dim == "2d") {
    # 2D plot
    plot <- plot_ly(
      data = data,
      x = data[[xaxis_input]],
      y = data[[yaxis_input]],
      type = "scatter",
      mode = "markers",
      color = ~selected_color,
      marker = list(size = size),
      colors = colors,
      width = 1200,
      height = 800
    ) %>%
      layout(
        xaxis = list(title = paste("X-", xaxis_input)),
        yaxis = list(title = paste("Y-", yaxis_input))
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
    
  } else {
    # 3D plot
    if (plot_type == "PCA") {
      x_col <- "pc_1"
      y_col <- "pc_2"
      z_col <- "pc_3"
    } else if (plot_type == "UMAP") {
      x_col <- "umap_1"
      y_col <- "umap_2"
      z_col <- "umap_3"
    } else if (plot_type == "tSNE") {
      x_col <- "t_sne_1"
      y_col <- "t_sne_2"
      z_col <- "t_sne_3"
    } else {
      stop("Invalid plot_type. Choose 'PCA', 'UMAP', or 'tSNE'.")
    }
    
    plot <- plot_ly(
      data = data,
      x = ~get(x_col),
      y = ~get(y_col),
      z = ~get(z_col),
      type = "scatter3d",
      mode = "markers",
      color = ~selected_color,
      marker = list(size = size),
      colors = colors,
      width = 1200,
      height = 800
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = paste("X-", x_col)),
          yaxis = list(title = paste("Y-", y_col)),
          zaxis = list(title = paste("Z-", z_col))
        )
      ) %>%
      config(displaylogo = FALSE, scrollZoom = TRUE)
  }
  
  return(plot)
}
