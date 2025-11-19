# Embedding plotting helpers

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
    # Use Set2 palette for better colors (as user prefers)
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

    # Use a solid gray (no alpha) for NA to keep missing values visible but muted
    colors <- if (has_na) {
        setNames(c("#7F7F7F", palette), final_levels)
    } else {
        setNames(palette, final_levels)
    }

    # Crucially, create a factor with the correctly ordered levels
    df[[color]] <- factor(df[[color]], levels = final_levels)

    return(list(df = df, colors = colors))
}


# Build a plotly-compatible colorscale (list of [position, color]) from a vector of hex colors
make_colorscale <- function(cols) {
    if (is.null(cols) || length(cols) == 0) {
        return(list(list(0, "#000000"), list(1, "#FFFFFF")))
    }
    n <- length(cols)
    lapply(seq_along(cols) - 1, function(i) list(i / (n - 1), cols[i + 1]))
}

# Map numeric vector to hex colors using a two-color ramp (default blue->red)
numeric_to_hex <- function(vals, colors = c("#99CCFF", "#FFB3B3")) {
    if (length(vals) == 0) {
        return(character(0))
    }
    if (!is.numeric(vals)) {
        # if already hex strings, try to return them (strip alpha)
        return(strip_hex_alpha(as.character(vals)))
    }
    rng <- range(vals, na.rm = TRUE)
    if (is.na(rng[1]) || is.infinite(rng[1]) || rng[1] == rng[2]) {
        return(rep(colors[1], length(vals)))
    }
    scaled <- (vals - rng[1]) / (rng[2] - rng[1])
    ramp <- grDevices::colorRamp(colors)(scaled)
    grDevices::rgb(ramp[, 1], ramp[, 2], ramp[, 3], maxColorValue = 255)
}

# Convert hex color(s) to 'rgba(r,g,b,a)' strings (accepts vector input)
hex_to_rgba <- function(hex, alpha = 0.6) {
    if (length(hex) == 0) {
        return(character(0))
    }
    sapply(hex, function(h) {
        if (is.na(h) || h == "") {
            return(NA_character_)
        }
        # strip leading '#'
        hh <- gsub("^#", "", h)
        # handle 3/6/8 char hex
        if (nchar(hh) == 3) {
            hh <- paste0(substr(hh, 1, 1), substr(hh, 1, 1), substr(hh, 2, 2), substr(hh, 2, 2), substr(hh, 3, 3), substr(hh, 3, 3))
        }
        if (nchar(hh) >= 6) hh <- substr(hh, 1, 6)
        r <- as.integer(paste0("0x", substr(hh, 1, 2)))
        g <- as.integer(paste0("0x", substr(hh, 3, 4)))
        b <- as.integer(paste0("0x", substr(hh, 5, 6)))
        sprintf("rgba(%d,%d,%d,%.3f)", r, g, b, alpha)
    }, USE.NAMES = FALSE)
}

# Helper to remove alpha channel from 8-character hex colors (#RRGGBBAA -> #RRGGBB)
strip_hex_alpha <- function(hex) {
    if (is.null(hex)) {
        return(hex)
    }
    if (!is.character(hex)) {
        return(hex)
    }
    # handle vector input
    vapply(hex, function(h) {
        if (is.na(h)) {
            return(NA_character_)
        }
        if (grepl("^#([A-Fa-f0-9]{8})$", h)) {
            return(substr(h, 1, 7))
        }
        return(h)
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

build_plot_slim <- function(meta, full_mat, gene_exp, group_col, extra_cols = NULL) {
    if (is.null(meta)) meta <- data.frame()

    # Ensure the group column exists in the returned slim df
    needed <- unique(c(group_col, extra_cols))
    available <- intersect(needed, colnames(meta))
    # If group_col not present, create it with NA so plotting code fails clearly
    if (!(group_col %in% colnames(meta))) {
        meta[[group_col]] <- NA_character_
    }

    slim <- meta %>% select(any_of(needed))

    # Extract gene expression vector from the full matrix using row_idx
    row_idx <- attr(meta, "row_idx")
    if (!is.null(full_mat) && !is.null(gene_exp) && gene_exp %in% colnames(full_mat)) {
        if (!is.null(row_idx) && length(row_idx) > 0) {
            gene_vec <- full_mat[[gene_exp]][row_idx]
        } else {
            # No row indices: fallback to full column but trim to meta rows if possible
            gene_vec <- full_mat[[gene_exp]]
            if (nrow(slim) > 0) gene_vec <- gene_vec[seq_len(nrow(slim))]
        }
    } else {
        gene_vec <- rep(NA, nrow(slim))
    }

    slim$gene_expr <- gene_vec
    return(slim)
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
get_legend_layout <- function(legend_title, y_pos = 1) {
    list(
        title = list(text = legend_title),
        orientation = "v",
        # Move legend slightly outside the plot area to the right
        x = 1.02,
        xanchor = "left",
        y = y_pos,
        yanchor = "top",
        len = 0.5,
        tracegroupgap = 10
    )
}

# Helper to build hover text for 2D and 3D plots to avoid duplication
build_hover_text <- function(df, legend_title, display_color, x_vals = NULL, y_vals = NULL, z_vals = NULL) {
    # Conditionally include Z label/value only when provided
    z_label <- if (is.null(z_vals)) "" else "<br>Z:"
    z_value <- if (is.null(z_vals)) "" else z_vals

    return(~ paste(
        "Cell_id:",
        df$barcode,
        "<br>Color by",
        legend_title,
        ":",
        display_color,
        "<br>Cluster:",
        df$seurat_clusters,
        "<br>Study:",
        df$study_label,
        "<br>Life cycle stage:",
        df$life_cycle_stage,
        "<br>Host species:",
        format_species_name(df$host_species),
        "<br>Sample type:",
        df$sample_type,
        "<br>Sequencer:",
        df$sequencer,
        "<br>Strain:",
        df$strain,
        "<br>n_Features_RNA:",
        df$n_feature_rna,
        "<br>X:",
        x_vals,
        "<br>Y:",
        y_vals,
        z_label,
        z_value
    ))
}

build_hover_text_string <- function(df, legend_title, display_color, x_vals = NULL, y_vals = NULL, z_vals = NULL) {
    # Conditionally include Z label/value only when provided
    z_label <- if (is.null(z_vals)) "" else "<br>Z:"
    z_value <- if (is.null(z_vals)) "" else z_vals

    return(paste(
        "Cell_id:",
        df$barcode,
        "<br>Color by",
        legend_title,
        ":",
        display_color,
        "<br>Cluster:",
        df$seurat_clusters,
        "<br>Study:",
        df$study_label,
        "<br>Life cycle stage:",
        df$life_cycle_stage,
        "<br>Host species:",
        format_species_name(df$host_species),
        "<br>Sample type:",
        df$sample_type,
        "<br>Sequencer:",
        df$sequencer,
        "<br>Strain:",
        df$strain,
        "<br>n_Features_RNA:",
        df$n_feature_rna,
        "<br>X:",
        x_vals,
        "<br>Y:",
        y_vals,
        z_label,
        z_value
    ))
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
    colors,
    symbol = NULL) {
    # Format selected_color for display if it's host_species
    display_color <- if (color == "host_species") {
        format_species_name(selected_color)
    } else {
        selected_color
    }

    hover_text <- build_hover_text_string(df, legend_title, display_color, x, y)

    legend_layout <- get_legend_layout(legend_title)
    # Fixed legend icon size for 3D (independent of plotted point size)
    legend_symbol_size <- 18
    df$plotly_key <- seq_len(nrow(df))
    # Fixed legend icon size for 2D (independent of plotted point size)
    legend_symbol_size <- 18

    # For large datasets use WebGL-based scattergl for much faster rendering
    gl_threshold <- 3000
    # If the user requested a symbol mapping, prefer SVG 'scatter' so marker
    # symbol names render accurately (plotly.js WebGL often remaps unsupported
    # glyph names). Otherwise, fall back to scattergl for large datasets.
    if (!is.null(symbol) && symbol %in% names(df)) {
        trace_type <- "scatter"
    } else {
        trace_type <- if (!is.null(nrow(df)) && nrow(df) > gl_threshold) "scattergl" else "scatter"
    }

    # Treat explicit 'unselected' or empty symbol choices as no symbol
    if (!is.null(symbol) && tolower(symbol) %in% c("unselected", "", "none")) {
        symbol <- NULL
    }

    if (is_categorical) {
        if (!is.null(symbol) && symbol %in% names(df)) {
            # When both color and symbol are used, create separate traces for proper legends
            df$symbol_factor <- factor(df[[symbol]])
            symbol_levels <- levels(df$symbol_factor)
            color_levels <- levels(factor(df[[color]]))

            # Define symbol sets: full SVG set for scatter, GL-safe subset for scattergl
            svg_symbol_shapes <- c(
                "circle",
                "x",
                "cross",
                "triangle-up",
                "triangle-down",
                "triangle-left",
                "triangle-right",
                "star",
                "pentagon",
                "hexagon"
            )
            gl_safe_shapes <- c("circle", "x", "diamond", "cross", "x", "triangle-up", "triangle-down")

            # Legend symbol size (fixed, not proportional to plotted point size)
            legend_symbol_size <- 14

            # Plotted marker size for points when symbols are enabled (slightly reduced)
            plotted_marker_size <- {
                s <- as.numeric(size)
                if (is.na(s) || s <= 4) {
                    5
                } else {
                    # slightly smaller than before to reduce overlap
                    max(5, floor(s * 0.95))
                }
            }

            # For non-GL scatter use plotly's native mapping (symbol=~) which lets plotly
            # build combined color+symbol legends. For WebGL (scattergl), native symbol
            # mapping can be unreliable, so fall back to building one trace per symbol
            # level and assign per-point categorical colors.
            # choose shape source depending on GL vs SVG rendering
            shape_source <- if (trace_type == "scattergl") gl_safe_shapes else svg_symbol_shapes
            n_sym <- length(shape_source)
            mapped_shapes <- shape_source[((seq_len(length(symbol_levels)) - 1) %% n_sym) + 1]
            symbols_map <- setNames(mapped_shapes, symbol_levels)
            safe_colors <- if (!is.null(colors)) strip_hex_alpha(unname(colors)) else NULL

            # If any mapped shape is 'x', increase outline thickness and slightly
            # bump marker size so the 'x' glyph is visible (SVG 'x' can be thin).
            has_x_shape <- any(mapped_shapes == "x")
            scatter_marker_size <- if (has_x_shape) max(4, plotted_marker_size - 1) else max(3, plotted_marker_size - 2)
            scatter_line_width <- if (has_x_shape) 1.6 else 0.8

            if (trace_type == "scatter") {
                # For SVG scatter let plotly handle color+symbol mapping natively so both
                # legends (color and symbol) are produced automatically.
                clean_colors <- safe_colors
                p <- plot_ly(
                    data = df, x = x, y = y, type = "scatter", mode = "markers",
                    color = ~ factor(df[[color]]), symbol = ~ factor(df[[symbol]]),
                    symbols = symbols_map, colors = clean_colors,
                    marker = list(size = scatter_marker_size, opacity = 0.98, line = list(color = "rgba(40,40,40,0.22)", width = scatter_line_width)),
                    text = hover_text, hoverinfo = "text", key = ~plotly_key, width = 1000, height = 700, source = "embedding"
                )
            } else {
                # For scattergl build one trace per symbol level (WebGL doesn't reliably
                # support full named symbol sets), assign per-point colors for each trace.
                p <- plot_ly(width = 1000, height = 700, source = "embedding")
                for (j in seq_along(symbol_levels)) {
                    sym_val <- symbol_levels[j]
                    idx <- which(df$symbol_factor == sym_val)
                    if (length(idx) == 0) next
                    # per-point colors from named 'colors' mapping (categorical)
                    if (!is.null(colors)) {
                        # map each key safely to a hex color, avoid dataframe subsetting errors
                        col_keys <- as.character(df[[color]][idx])
                        trace_colors <- vapply(col_keys, function(k) {
                            if (is.data.frame(colors)) {
                                tmp_colors <- as.character(unlist(colors))
                                # try to find by name
                                if (!is.null(names(tmp_colors)) && k %in% names(tmp_colors)) {
                                    return(strip_hex_alpha(tmp_colors[[k]]))
                                }
                                return(NA_character_)
                            } else {
                                if (!is.null(names(colors)) && k %in% names(colors)) {
                                    return(strip_hex_alpha(colors[[k]]))
                                }
                                return(NA_character_)
                            }
                        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
                    } else {
                        trace_colors <- NULL
                    }
                    p <- p %>% add_trace(
                        x = x[idx],
                        y = y[idx],
                        type = trace_type,
                        mode = "markers",
                        name = sym_val,
                        legendgroup = sym_val,
                        showlegend = TRUE,
                        marker = list(
                            size = max(3, plotted_marker_size - 2),
                            color = trace_colors,
                            symbol = symbols_map[[sym_val]],
                            line = list(color = "rgba(40,40,40,0.22)", width = 0.8),
                            opacity = 0.98
                        ),
                        text = hover_text[idx],
                        hoverinfo = "text",
                        key = df$plotly_key[idx]
                    )
                }

                # (No helper legend traces: rely on native legend rendering when using SVG)
            }

            p <- p %>% layout(
                xaxis = list(title = paste("X-", x)),
                yaxis = list(title = paste("Y-", y)),
                legend = legend_layout,
                font = list(size = 14),
                margin = list(l = 50, r = 50, t = 25, b = 50)
            )
        } else {
            # Original categorical plotting without symbol
            # Categorical without symbol: ensure opaque colors
            safe_colors <- strip_hex_alpha(unname(colors))
            p <- plot_ly(
                data = df,
                x = x,
                y = y,
                type = trace_type,
                mode = "markers",
                color = ~ factor(df[[color]]),
                # Slightly smaller markers and near-opaque fills with light outlines
                marker = list(size = max(3, ceiling(as.numeric(size) * 0.95) - 1), opacity = 0.99, line = list(color = "rgba(60,60,60,0.16)", width = 0.7)),
                text = hover_text,
                hoverinfo = "text",
                colors = safe_colors,
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
        }
    } else {
        if (!is.null(symbol) && symbol %in% names(df)) {
            # Continuous color with symbols
            df$symbol_factor <- factor(df[[symbol]])
            symbol_levels <- levels(df$symbol_factor)

            # Define a broader set of visually-distinct symbol shapes for symbol-only traces
            # 2D-friendly symbol set for continuous+symbol
            symbol_shapes <- c(
                "circle",
                "x",
                "diamond",
                "cross",
                "triangle-up",
                "triangle-down",
                "triangle-left",
                "triangle-right",
                "star",
                "pentagon",
                "hexagon"
            )

            # Lower categorical legend when color is continuous so it doesn't overlap colorbar
            # Place the categorical legend below the middle of the plot for easier separate control
            cat_legend_layout <- if (!is_categorical) get_legend_layout(symbol, y_pos = 0.45) else legend_layout

            p <- plot_ly(width = 1000, height = 700, source = "embedding")

            # If color is continuous, add a persistent invisible trace to own the colorbar
            if (!is_categorical) {
                safe_selected <- strip_hex_alpha(selected_color)
                p <- p %>% add_trace(
                    x = x,
                    y = y,
                    type = trace_type,
                    mode = "markers",
                    marker = list(
                        size = 1,
                        color = safe_selected,
                        colorscale = list(c(0, "#0000FF"), c(1, "red")),
                        showscale = TRUE,
                        colorbar = modifyList(legend_layout, list(x = 1.18, xanchor = "left", thickness = 15)),
                        opacity = 0
                    ),
                    showlegend = FALSE,
                    hoverinfo = "none"
                )
            }

            # Plotted marker size for points when symbols are enabled (slightly reduced)
            plotted_marker_size <- {
                s <- as.numeric(size)
                if (is.na(s) || s <= 4) {
                    7
                } else {
                    ceiling(s * 1.05)
                }
            }

            # Create a trace for each symbol level
            for (j in seq_along(symbol_levels)) {
                symbol_val <- symbol_levels[j]
                subset_indices <- df$symbol_factor == symbol_val

                if (sum(subset_indices) > 0) {
                    # Ensure colors are opaque for these per-symbol traces
                    if (is.numeric(selected_color)) {
                        trace_colors <- numeric_to_hex(selected_color[subset_indices], colors = c("#0000FF", "#FF0000"))
                    } else {
                        trace_colors <- strip_hex_alpha(selected_color[subset_indices])
                    }
                    # Use a legendgroup so the separate legend trace controls the plotted trace
                    lg <- paste0("sym_", j)
                    p <- p %>% add_trace(
                        x = x[subset_indices],
                        y = y[subset_indices],
                        type = trace_type,
                        mode = "markers",
                        name = symbol_val,
                        legendgroup = lg,
                        showlegend = TRUE,
                        marker = list(
                            size = max(3, plotted_marker_size - 2),
                            # increase fill opacity and use a light translucent white outline to
                            # prevent outlines from darkening overlapping regions
                            color = hex_to_rgba(trace_colors, alpha = 0.96),
                            symbol = symbol_shapes[((j - 1) %% length(symbol_shapes)) + 1],
                            # subtle dark outline to provide definition without heavy dark strokes
                            line = list(color = "rgba(60,60,60,0.18)", width = 0.6),
                            showscale = FALSE,
                            colorbar = NULL,
                            opacity = 0.99
                        ),
                        text = hover_text[subset_indices],
                        hoverinfo = "text",
                        key = df$plotly_key[subset_indices]
                    )
                }
            }

            p <- p %>% layout(
                xaxis = list(title = paste("X-", x)),
                yaxis = list(title = paste("Y-", y)),
                legend = cat_legend_layout,
                font = list(size = 14),
                margin = list(l = 50, r = 50, t = 25, b = 50)
            )
        } else {
            # Original continuous plotting without symbol
            # Continuous color without symbol: ensure opaque selected colors
            safe_selected <- strip_hex_alpha(selected_color)
            # Increase default point size and outline for non-symbol continuous 2D
            p <- plot_ly(
                data = df,
                x = x,
                y = y,
                type = trace_type,
                mode = "markers",
                text = hover_text,
                hoverinfo = "text",
                width = 1000,
                height = 700,
                key = ~plotly_key,
                source = "embedding",
                marker = list(
                    size = max(3, ceiling(as.numeric(size) * 0.95) - 1),
                    color = safe_selected,
                    # use opaque endpoints for a more vivid but balanced colorbar
                    colorscale = list(list(0, "#0000FF"), list(1, "#FF0000")),
                    showscale = TRUE,
                    colorbar = legend_layout,
                    opacity = 0.99,
                    line = list(color = "rgba(60,60,60,0.18)", width = 0.9)
                )
            ) %>%
                layout(
                    xaxis = list(title = paste("X-", x)),
                    yaxis = list(title = paste("Y-", y)),
                    font = list(size = 14),
                    margin = list(l = 50, r = 50, t = 25, b = 50)
                )
        }
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
    coords,
    symbol = NULL) {
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

    hover_text <- build_hover_text_string(df, legend_title, display_color, x_vals, y_vals, z_vals)

    legend_layout <- get_legend_layout(legend_title)
    # Fixed legend icon size for 3D (independent of plotted point size)
    legend_symbol_size <- 18

    # Treat explicit 'unselected' or empty symbol choices as no symbol
    if (!is.null(symbol) && tolower(symbol) %in% c("unselected", "", "none")) {
        symbol <- NULL
    }

    if (is_categorical) {
        if (!is.null(symbol) && symbol %in% names(df)) {
            # Categorical color with symbols - create separate traces
            df$symbol_factor <- factor(df[[symbol]])
            symbol_levels <- levels(df$symbol_factor)
            color_levels <- levels(factor(df[[color]]))

            # Define a broader set of visually-distinct symbol shapes for 3D (use named plotly symbols)
            # Note: 3D marker symbol support is more limited in some environments, but named symbols below
            # provide better shape variety where supported.
            symbol_shapes <- c(
                "circle",
                "x",
                "x",
                "cross",
                "triangle-up",
                "triangle-down",
                "triangle-left",
                "triangle-right",
                "star"
            )

            # Use plotly's native mapping for 3D: color and symbol mapped directly.
            n_sym <- length(symbol_shapes)
            mapped_shapes <- symbol_shapes[((seq_len(length(symbol_levels)) - 1) %% n_sym) + 1]
            symbols_map <- setNames(mapped_shapes, symbol_levels)

            # ensure colors have no alpha and strong opacity
            clean_colors <- strip_hex_alpha(unname(colors))
            p <- plot_ly(
                data = df,
                x = ~ .data[[x_col]],
                y = ~ .data[[y_col]],
                z = ~ .data[[z_col]],
                type = "scatter3d",
                mode = "markers",
                color = ~ factor(df[[color]]),
                legendgroup = ~ factor(df[[color]]),
                colors = clean_colors,
                symbol = ~ factor(df[[symbol]]),
                symbols = symbols_map,
                # Slightly smaller markers and near-opaque fills with light outlines for 3D
                # Match 2D appearance: make 3D markers fully opaque and use subtle outline
                # Use same opacity and outline settings as 2D (no 3D lighting)
                # Remove outlines in 3D so overlapping points don't show white rings
                marker = list(size = max(3, ceiling(as.numeric(size) * 0.8) - 1), opacity = 1, line = list(width = 0)),
                text = hover_text,
                hoverinfo = "text",
                key = ~ df$id,
                width = 1000,
                height = 700,
                source = "embedding"
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
            # Original categorical 3D plotting without symbol
            # Map categories to per-point hex colors and pass explicit color vector
            clean_colors <- strip_hex_alpha(unname(colors))
            # Build per-point color vector using the named 'colors' mapping from prepare_categorical_colors
            # robust per-point color vector (handle colors as named vector or simple df)
            if (!is.null(colors)) {
                if (is.data.frame(colors)) {
                    tmp_colors <- as.character(unlist(colors))
                    names(tmp_colors) <- names(colors)
                    per_point_colors <- strip_hex_alpha(unname(tmp_colors[as.character(df[[color]])]))
                } else {
                    per_point_colors <- strip_hex_alpha(unname(colors[as.character(df[[color]])]))
                }
            } else {
                per_point_colors <- NULL
            }

            # Create one trace per category so the legend appears and toggling works
            p <- plot_ly(width = 1000, height = 700, source = "embedding")
            cat_levels <- unique(as.character(df[[color]]))
            # preserve order if factor
            if (is.factor(df[[color]])) cat_levels <- levels(df[[color]])
            for (lvl in cat_levels) {
                idx <- which(as.character(df[[color]]) == lvl)
                if (length(idx) == 0) next
                col_hex <- if (!is.null(colors) && lvl %in% names(colors)) strip_hex_alpha(colors[[lvl]]) else "#7F7F7F"
                p <- p %>% add_trace(
                    x = x_vals[idx],
                    y = y_vals[idx],
                    z = z_vals[idx],
                    type = "scatter3d",
                    mode = "markers",
                    name = lvl,
                    legendgroup = lvl,
                    showlegend = TRUE,
                    marker = list(
                        size = max(3, ceiling(as.numeric(size) * 0.8) - 1),
                        color = col_hex,
                        opacity = 1,
                        line = list(width = 0)
                    ),
                    text = hover_text[idx],
                    hoverinfo = "text",
                    key = df$id[idx]
                )
            }

            p <- p %>% layout(
                scene = list(
                    xaxis = list(title = paste("X-", x_col)),
                    yaxis = list(title = paste("Y-", y_col)),
                    zaxis = list(title = paste("Z-", z_col))
                ),
                legend = legend_layout,
                font = list(size = 14),
                margin = list(l = 50, r = 50, t = 25, b = 50)
            )
        }
    } else {
        if (!is.null(symbol) && symbol %in% names(df)) {
            # Continuous color with symbols
            df$symbol_factor <- factor(df[[symbol]])
            symbol_levels <- levels(df$symbol_factor)

            # Define a broader set of visually-distinct symbol shapes for 3D (continuous color case)
            symbol_shapes <- c(
                "circle",
                "x",
                "diamond",
                "cross",
                "x",
                "triangle-up",
                "triangle-down",
                "triangle-left",
                "triangle-right",
                "star"
            )

            # Lower categorical legend when color is continuous so it doesn't overlap colorbar
            # Place the categorical legend below the middle of the plot for easier separate control
            cat_legend_layout <- if (!is_categorical) get_legend_layout(symbol, y_pos = 0.45) else legend_layout

            p <- plot_ly(width = 1000, height = 700, source = "embedding")
            # If color is continuous, add a persistent invisible 3D trace to own the colorbar
            if (!is_categorical) {
                safe_sel_3d <- strip_hex_alpha(selected_color)
                p <- p %>% add_trace(
                    x = x_vals,
                    y = y_vals,
                    z = z_vals,
                    type = "scatter3d",
                    mode = "markers",
                    marker = list(
                        size = 1,
                        color = safe_sel_3d,
                        colorscale = list(c(0, "#0000FF"), c(1, "red")),
                        showscale = TRUE,
                        colorbar = modifyList(legend_layout, list(x = 1.18, xanchor = "left", thickness = 15)),
                        opacity = 0
                    ),
                    showlegend = FALSE,
                    hoverinfo = "none"
                )
            }

            # Create a trace for each symbol level
            for (j in seq_along(symbol_levels)) {
                symbol_val <- symbol_levels[j]
                subset_indices <- df$symbol_factor == symbol_val

                if (sum(subset_indices) > 0) {
                    # Ensure per-symbol continuous 3D traces use opaque colors
                    if (is.numeric(selected_color)) {
                        trace_colors_3d <- numeric_to_hex(selected_color[subset_indices], colors = c("#0000FF", "#FF0000"))
                    } else {
                        trace_colors_3d <- strip_hex_alpha(selected_color[subset_indices])
                    }
                    # slightly larger markers for better visibility in 3D
                    lg3 <- paste0("sym3d_", j)
                    p <- p %>% add_trace(
                        x = x_vals[subset_indices],
                        y = y_vals[subset_indices],
                        z = z_vals[subset_indices],
                        type = "scatter3d",
                        mode = "markers",
                        name = symbol_val,
                        legendgroup = lg3,
                        showlegend = TRUE,
                        marker = list(
                            size = max(3, ceiling(as.numeric(size) * 0.95) - 1),
                            # Use opaque hex colors (like 2D) and remove outlines in 3D
                            color = strip_hex_alpha(trace_colors_3d),
                            symbol = symbol_shapes[((j - 1) %% length(symbol_shapes)) + 1],
                            line = list(width = 0),
                            showscale = FALSE,
                            colorbar = NULL,
                            opacity = 1
                        ),
                        text = hover_text[subset_indices],
                        hoverinfo = "text",
                        key = df$id[subset_indices]
                    )
                }
            }

            p <- p %>% layout(
                scene = list(
                    xaxis = list(title = paste("X-", x_col)),
                    yaxis = list(title = paste("Y-", y_col)),
                    zaxis = list(title = paste("Z-", z_col))
                ),
                legend = cat_legend_layout,
                font = list(size = 14),
                margin = list(l = 50, r = 50, t = 25, b = 50)
            )
        } else {
            # Continuous 3D without symbol: keep colorbar but render visible per-point
            # opaque hex colors so the visual weight matches 2D.
            # Build visible per-point hex colors (interpolate numeric to hex if needed)
            if (is.numeric(selected_color)) {
                visible_colors_3d <- numeric_to_hex(selected_color, colors = c("#0000FF", "#FF0000"))
            } else {
                visible_colors_3d <- strip_hex_alpha(as.character(selected_color))
            }

            # Single numeric-colored 3D trace with explicit cmin/cmax and colorscale
            p <- plot_ly(
                x = x_vals,
                y = y_vals,
                z = z_vals,
                type = "scatter3d",
                mode = "markers",
                marker = list(
                    size = max(4, ceiling(as.numeric(size) * 1.0) - 1),
                    color = if (is.numeric(selected_color)) selected_color else strip_hex_alpha(visible_colors_3d),
                    colorscale = list(list(0, "#0000FF"), list(1, "#FF0000")),
                    cmin = if (is.numeric(selected_color)) min(selected_color, na.rm = TRUE) else NULL,
                    cmax = if (is.numeric(selected_color)) max(selected_color, na.rm = TRUE) else NULL,
                    showscale = TRUE,
                    colorbar = legend_layout,
                    opacity = 1,
                    line = list(width = 0)
                ),
                text = hover_text,
                hoverinfo = "text",
                width = NULL,
                height = NULL,
                source = "embedding",
                key = ~ df$id
            ) %>%
                layout(
                    scene = list(
                        xaxis = list(title = paste("X-", x_col)),
                        yaxis = list(title = paste("Y-", y_col)),
                        zaxis = list(title = paste("Z-", z_col)),
                        aspectratio = list(x = 1.5, y = 1.5, z = 1)
                    ),
                    font = list(size = 14),
                    margin = list(l = 50, r = 50, t = 25, b = 50)
                )
        }
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
    plot_dim = "2d",
    symbol = NULL) {
    # Validate inputs
    if (is.null(color) || is.na(color) || color == "" || !color %in% names(df)) {
        color <- "sample_type" # Safe fallback
        warning("Invalid color column, using 'sample_type' as fallback")
    }

    # Normalize the symbol parameter: treat empty, whitespace or 'Unselected' as no symbol
    if (!is.null(symbol)) {
        sym_val <- as.character(symbol)
        if (!is.na(sym_val)) {
            sym_val <- trimws(sym_val)
            if (!nzchar(sym_val) || tolower(sym_val) == "unselected" || tolower(sym_val) == "none") {
                symbol <- NULL
            } else {
                symbol <- sym_val
            }
        } else {
            symbol <- NULL
        }
    }

    x <- df[[xaxis_input]]
    y <- df[[yaxis_input]]

    # ensure stable per-row idenfitifier for plotly key usage
    if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))
    if (!"plotly_key" %in% names(df)) df$plotly_key <- seq_len(nrow(df))

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

    # After prepare_categorical_colors the df may have been modified (factor levels, NA handling).
    # Recompute selected_color to ensure it aligns with the possibly-updated df rows.
    selected_color <- df[[color]]

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
            colors,
            symbol
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
            coords,
            symbol
        )
    }

    return(p)
}
