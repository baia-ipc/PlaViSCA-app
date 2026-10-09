# Gene identifiers and names: building the gene list and converting between
# gene names and PVP01 identifiers.

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
        # Use base R instead of stringr to avoid dependency issues
        genes_df %>%
            filter(!grepl("^PVP01", name)) %>%
            pull(name)
    }
}

# Get gene id
get_gene_id <- function(gene_name, gene_names) {
    # Handle NULL or empty input
    if (is.null(gene_name) || length(gene_name) == 0) {
        return(NA)
    }

    # Ensure gene_names is a data.frame with 'name' and 'gene' columns
    if (!is.data.frame(gene_names) || !all(c("name", "gene") %in% names(gene_names))) {
        return(rep(NA, length(gene_name)))
    }

    # Create a lookup map for faster matching
    gene_map <- setNames(gene_names$gene, gene_names$name)

    # Match input gene names to the map
    sapply(gene_name, function(name) {
        if (is.na(name) || name == "") {
            return(NA)
        }

        # Return the gene ID from the map, or NA if not found
        unname(gene_map[name])
    }, USE.NAMES = FALSE)
}
