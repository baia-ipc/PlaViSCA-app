# Colours of "Each gene separately" for 4 genes or more (above
# MAX_GENES_RGB_MIX; 2 or 3 genes are mixed as red, green and blue in
# gene_colour_mix.R): a colour wheel. Each of the k genes gets a hue, evenly
# spaced around the wheel in gene order, and the colour of a cell mixes the
# hues of the genes it expresses:
#   hue          the genes the cell expresses (the vector sum of their hues,
#                weighted by their share of the cell's scaled expression);
#   saturation   how specific the cell is: one gene gives the full colour,
#                genes evenly spread around the wheel cancel out to grey;
#   darkness     the total expression of the cell (the sum of its scaled
#                genes, scaled again to its 99th percentile among the
#                cells), from light grey (none) to a dark, saturated colour.
# Colours are computed in HCL (grDevices::hcl()), so that hues of equal
# chroma and luminance look equally strong. A colour that sRGB cannot show
# keeps its hue and luminance and loses chroma (wheel_hcl()).

# Hue (degrees) of the first gene: 12 makes it red, as in the mixing of 2 or
# 3 genes.
GENE_WHEEL_HUE_OFFSET <- 12

# Chroma of a cell expressing one gene at full intensity; luminance at zero
# and at full intensity. The cells get darker with expression, because the
# plots have a white background; 86 is the luminance of GENE_COLOUR_NONE.
GENE_WHEEL_CHROMA <- 100
GENE_WHEEL_LUMINANCE <- c(none = 86, full = 50)

# Largest chroma that sRGB can show, by hue (rows, 0..359 degrees) and
# luminance (columns, 0..100), found by bisection; computed on first use.
wheel_chroma_limits <- local({
    limits <- NULL
    function() {
        if (is.null(limits)) {
            h <- rep(0:359, times = 101)
            l <- rep(0:100, each = 360)
            lo <- rep(0, length(h))
            hi <- rep(200, length(h))
            for (i in 1:16) {
                mid <- (lo + hi) / 2
                ok <- !is.na(grDevices::hcl(h, mid, l, fixup = FALSE))
                lo[ok] <- mid[ok]
                hi[!ok] <- mid[!ok]
            }
            limits <<- matrix(lo, 360, 101)
        }
        limits
    }
})

# Hex colours of hue `h`, chroma `c` and luminance `l` (vectors), with the
# chroma lowered to what sRGB can show at that hue and luminance; a small
# margin covers the rounding of the table, and `fixup` any rest.
wheel_hcl <- function(h, c, l) {
    limit <- wheel_chroma_limits()[cbind(round(h) %% 360 + 1, pmin(pmax(round(l), 0), 100) + 1)]
    grDevices::hcl(h, pmin(c, 0.97 * limit), l, fixup = TRUE)
}

# Hue (degrees) of each of `k` genes, in gene order.
wheel_gene_hues <- function(k) {
    (GENE_WHEEL_HUE_OFFSET + 360 * (seq_len(k) - 1) / k) %% 360
}

# Colour of cells from their scaled expression (`scaled`: cells x genes,
# values 0..1, columns in gene order). Returns a list of `colour` (hex, one
# per cell), `intensity` (the scaled total expression of each cell, 0..1,
# which sets its darkness), `dominant`
# (the column of the gene with the largest share; NA without expression),
# `share` (its share of the cell's expression) and `hues` (one per gene).
wheel_gene_colours <- function(scaled) {
    scaled <- as.matrix(scaled)
    scaled[is.na(scaled)] <- 0
    k <- ncol(scaled)
    if (k < 1) {
        stop("wheel_gene_colours(): no gene", call. = FALSE)
    }
    hues <- wheel_gene_hues(k)
    total <- rowSums(scaled)
    expressed <- total > 0
    shares <- scaled / ifelse(expressed, total, 1)
    a <- as.numeric(shares %*% cos(hues * pi / 180))
    b <- as.numeric(shares %*% sin(hues * pi / 180))
    specificity <- pmin(sqrt(a^2 + b^2), 1)
    cells <- seq_len(nrow(scaled))
    dominant <- max.col(scaled, ties.method = "first")
    intensity <- as.numeric(scale_gene_expression(total))
    luminance <- GENE_WHEEL_LUMINANCE[["none"]] +
        (GENE_WHEEL_LUMINANCE[["full"]] - GENE_WHEEL_LUMINANCE[["none"]]) * intensity
    colour <- wheel_hcl((atan2(b, a) * 180 / pi) %% 360, GENE_WHEEL_CHROMA * specificity * intensity,
                        luminance)
    colour[!expressed] <- grDevices::rgb(GENE_COLOUR_NONE[1], GENE_COLOUR_NONE[2], GENE_COLOUR_NONE[3])
    share <- shares[cbind(cells, dominant)]
    dominant[!expressed] <- NA_integer_
    share[!expressed] <- NA_real_
    list(colour = colour, intensity = intensity, dominant = dominant, share = share, hues = hues)
}

# Angle (radians, counterclockwise from the x axis) at which the legend draws
# `hue`: gene 1 at the top, the others clockwise.
wheel_display_angle <- function(hue) {
    (90 - (hue - GENE_WHEEL_HUE_OFFSET)) * pi / 180
}

# Colour disc of the legend: a square image of `pixels` x `pixels` covering
# -1..1 on both axes, as an array of RGBA values (rows from y = -1 up, 0..255;
# transparent outside the disc). Each point has the colour of a cell at full
# intensity whose genes add up to that point: the hue of its angle, and less
# chroma towards the centre, which is the grey of balanced genes.
wheel_disc_image <- function(pixels = 81) {
    at <- seq(-1, 1, length.out = pixels)
    x <- rep(at, times = pixels)
    y <- rep(at, each = pixels)
    radius <- sqrt(x^2 + y^2)
    hue <- (GENE_WHEEL_HUE_OFFSET + 90 - atan2(y, x) * 180 / pi) %% 360
    colour <- wheel_hcl(hue, GENE_WHEEL_CHROMA * pmin(radius, 1), GENE_WHEEL_LUMINANCE[["full"]])
    rgba <- rbind(grDevices::col2rgb(colour), alpha = ifelse(radius <= 1, 255, 0))
    image <- array(0, c(pixels, pixels, 4))
    for (channel in 1:4) {
        image[, , channel] <- matrix(rgba[channel, ], pixels, pixels, byrow = TRUE)
    }
    image
}

# Colour wheel of the legend, on axes from -1 to 1: the colour disc
# (wheel_disc_image()), one point per gene on its edge at the gene's hue,
# with its name (`genes`, with the text position and hover text of each), a
# spoke from the centre to each gene (`spokes`: x, y, with NA between
# spokes) and the centre (`centre`). Gene 1 is at the top, the others follow
# clockwise.
gene_wheel_key <- function(genes) {
    hues <- wheel_gene_hues(length(genes))
    angle <- wheel_display_angle(hues)
    points <- data.frame(x = cos(angle), y = sin(angle))
    points$colour <- wheel_hcl(hues, GENE_WHEEL_CHROMA, GENE_WHEEL_LUMINANCE[["full"]])
    points$label <- genes
    points$position <- ifelse(points$x > 0.3, "middle right",
                       ifelse(points$x < -0.3, "middle left",
                       ifelse(points$y > 0, "top center", "bottom center")))
    points$text <- paste0(genes, " only")
    spokes <- data.frame(x = as.vector(rbind(0, points$x, NA)), y = as.vector(rbind(0, points$y, NA)))
    list(disc = wheel_disc_image(), genes = points, spokes = spokes,
         centre = data.frame(x = 0, y = 0, text = "Genes mixed in balance: grey"))
}
