# Accessors for config/external_links.yml and config/team.yml.
# =============================================================================
# External web addresses and team information are kept in config/ so they can
# be changed without editing the app code.
# =============================================================================

.site_config_path <- function(file) {
    file.path(Sys.getenv("PLAVISCA_CONFIG_DIR", unset = "config"), file)
}

.external_links <- yaml::read_yaml(.site_config_path("external_links.yml"))
.team <- yaml::read_yaml(.site_config_path("team.yml"))

#' URL for a key of config/external_links.yml. When `id` is given, {id} in the
#' template is replaced by each element of `id` (one URL per element).
#'
#' external_link("source_code")
#' external_link("plasmodb_gene", c("PVP01_0102300", "PVP01_0102400"))
external_link <- function(key, id = NULL) {
    url <- .external_links[[key]]
    if (is.null(url)) {
        stop(sprintf(
            "Unknown external link '%s' - add it to %s",
            key, .site_config_path("external_links.yml")
        ), call. = FALSE)
    }
    if (is.null(id)) {
        return(url)
    }
    vapply(id, function(x) gsub("{id}", x, url, fixed = TRUE), character(1), USE.NAMES = FALSE)
}

#' Organizations (name, url, logo) and contact (name, role, affiliation,
#' email) from config/team.yml.
team_info <- function() {
    .team
}
