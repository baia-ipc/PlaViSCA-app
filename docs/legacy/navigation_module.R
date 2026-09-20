# =============================================================================
# HISTORICAL / LEGACY - NOT USED BY THE RUNNING APPLICATION
# =============================================================================
# This file was moved out of functions/ during the Phase-3 app repair
# (2026-09, branch plavisca-app-production-repair). It is retained only as a
# historical record.
#
# It is NOT sourced by global.R and is NOT executed by the app. It predates
# the Phase-2 atlas schema and may reference metadata columns that no longer
# exist (e.g. parasite_stages, development_phase, life_cycle_stage,
# hour_post_invasion, geographic_location).
#
# The ONLY authoritative preprocessing implementation is the separate
# pre_process_data repository. Do not update this file into a second
# pipeline, and do not re-source it from the app.
#
# See audit/phase3_app_repair/REPORT.md for the rationale.
# =============================================================================

# Navigation Module
# Functions for handling navigation between sections in the PlaViSCA app

#' Setup navigation observers
#' @param input Shiny input object
#' @param session Shiny session object
setup_navigation_observers <- function(input, session) {
    # Navigate to home section
    observeEvent(input$home, {
        shinyjs::hide("analysis_section")
        shinyjs::hide("about_section")
        shinyjs::show("home_section")

        shinyjs::runjs(
            "
            $('.menu-link').removeClass('active');
            $('#home').addClass('active');

            // Initialize particles for home page footer
            setTimeout(function() {
                if (typeof initHomeParticles === 'function') {
                    initHomeParticles();
                } else if (typeof initParticles === 'function') {
                    initParticles('particles-js-home');
                }
            }, 300);
            "
        )
    })

    # Navigate to about section
    observeEvent(
        list(input$about, input$contribute_link, input$about_link),
        {
            shinyjs::hide("home_section")
            shinyjs::hide("analysis_section")
            shinyjs::show("about_section")

            runjs("initParticles('colab-particles');")
            runjs("initParticles('particles-js-about');")

            shinyjs::runjs(
                "
                $('.menu-link').removeClass('active');
                $('#about').addClass('active');
                "
            )
        },
        ignoreInit = TRUE
    )

    # Navigate to analysis section
    observeEvent(input$analysis, {
        shinyjs::hide("home_section")
        shinyjs::hide("about_section")
        shinyjs::show("analysis_section")

        shinyjs::runjs(
            "
            $('.menu-link').removeClass('active');
            $('#analysis').addClass('active');
            "
        )
    })

    # Navigation between main sections
    observeEvent(input$metadata_link, {
        navigate_tabset(session, "analysis", "Metadata")
    })

    observeEvent(input$analysis_link, {
        navigate_tabset(session, "analysis", "Embeddings")
    })

    observeEvent(input$qc_link, {
        navigate_tabset(session, "analysis", "Quality control")
    })

    observeEvent(input$sources_link, {
        navigate_tabset(session, "analysis", "Data sources")
    })

    observeEvent(input$gene_exp_link, {
        navigate_tabset(session, "analysis", "Gene expression")
    })

    # User manual
    addResourcePath("manual", "manual/docs/build/html")
    observeEvent(input$usermanual_link, {
        runjs("window.open('manual/index.html', '_blank')")
    })
}
