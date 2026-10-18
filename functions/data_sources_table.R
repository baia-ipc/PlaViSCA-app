# The Data sources table: data_source.csv arranged for display.

# Columns in display order: Study label, Number of cells, Authors, Title,
# Journal, Year, PMID, DOI. The year comes from Publication_date (e.g.
# "4-May-20"); the DOI link shows the bare DOI and points to doi.org.
format_data_sources <- function(pub) {
    # last two digits of e.g. "4-May-20" (no month parsing: it depends on the locale)
    year <- sub("^.*-(\\d{2})$", "20\\1", pub$Publication_date)
    doi_url <- sub(".*href='([^']*)'.*", "\\1", pub$DOI)
    doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi_url)
    data.frame(
        `Study label` = pub$Study_label,
        `Number of cells` = pub$Number_of_cells,
        Authors = pub$Authors,
        Title = pub$Title,
        Journal = pub$Journal,
        Year = year,
        PMID = pub$study_pmid,
        DOI = sprintf("<a href='%s' target='_blank'>%s</a>", doi_url, doi),
        check.names = FALSE
    )
}
