# Colours of "Each gene separately" for 2 or 3 genes (up to
# MAX_GENES_RGB_MIX): each gene is a colour channel, gene 1 red, gene 2 green,
# gene 3 blue, and the colour of a cell mixes them. More genes use the colour
# wheel (gene_colour_wheel.R).

# Colour channel of each gene: gene 1 red, gene 2 green, gene 3 blue.
GENE_BLEND_CHANNELS <- diag(3)

# Brightness of the mixed colour when 1, 2 or 3 genes are fully expressed:
# lower for several genes, so that their mixtures (yellow, cyan, magenta)
# stay readable on white and the three genes together give a dark grey.
GENE_BLEND_BRIGHTNESS <- c(0.9, 0.8, 0.25)

# Colour (hex) of each row of `scaled` (cells x 1 to 3 genes, values 0..1).
# The strongest gene sets the intensity (light grey at 0, the full colour at
# 1) and the ratios of the genes set the hue: red and green mix to yellow,
# red and blue to magenta, green and blue to cyan, all three to dark grey.
blend_gene_colours <- function(scaled) {
    scaled <- as.matrix(scaled)
    k <- ncol(scaled)
    if (k < 1 || k > MAX_GENES_RGB_MIX) {
        stop("blend_gene_colours(): 1 to ", MAX_GENES_RGB_MIX, " genes, got ", k, call. = FALSE)
    }
    strength <- scaled[cbind(seq_len(nrow(scaled)), max.col(scaled, ties.method = "first"))]
    hue <- scaled / ifelse(strength > 0, strength, 1)
    brightness <- stats::approx(seq_along(GENE_BLEND_BRIGHTNESS), GENE_BLEND_BRIGHTNESS,
                                xout = pmax(1, rowSums(hue)), rule = 2)$y
    full <- (hue %*% GENE_BLEND_CHANNELS[seq_len(k), , drop = FALSE]) * brightness
    none <- matrix(GENE_COLOUR_NONE, nrow(scaled), 3, byrow = TRUE)
    mixed <- none * (1 - strength) + full * strength
    grDevices::rgb(mixed[, 1], mixed[, 2], mixed[, 3])
}

# Colour key of 2 genes: a grid of expression of gene 1 (x) by gene 2 (y),
# from 0 to the value shown at full colour (`tops`). Returns the x and y
# values, the colours of the grid (rows = y) and its hover text.
gene_colour_grid <- function(genes, tops, steps = 9) {
    levels <- seq(0, 1, length.out = steps)
    grid <- expand.grid(g1 = levels, g2 = levels)
    colours <- matrix(blend_gene_colours(as.matrix(grid)), steps, steps, byrow = TRUE)
    text <- matrix(sprintf("%s: %.2f<br>%s: %.2f", genes[[1]], grid$g1 * tops[[1]],
                           genes[[2]], grid$g2 * tops[[2]]), steps, steps, byrow = TRUE)
    list(x = levels * tops[[1]], y = levels * tops[[2]], colours = colours, text = text)
}

# Legend entries of 3 genes: each gene alone, each pair, all three, none.
gene_colour_legend <- function(genes) {
    sets <- c(lapply(seq_along(genes), function(i) i), utils::combn(seq_along(genes), 2, simplify = FALSE),
              list(seq_along(genes)))
    labels <- vapply(sets, function(s) paste(genes[s], collapse = " + "), character(1))
    labels[length(labels)] <- "All three"
    colours <- vapply(sets, function(s) {
        x <- numeric(length(genes))
        x[s] <- 1
        blend_gene_colours(matrix(x, nrow = 1))
    }, character(1))
    data.frame(label = c(labels, "None"),
               colour = c(colours, blend_gene_colours(matrix(0, 1, length(genes)))),
               stringsAsFactors = FALSE)
}
