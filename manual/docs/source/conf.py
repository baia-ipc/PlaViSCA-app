# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'PlaViSca'
copyright = 'BAIA 2025, Sopheap Oeng'
author = 'Sopheap Oeng'
release = 'V.01'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration
import sphinx_rtd_theme

extensions = ['myst_parser']
myst_heading_anchors = 2

templates_path = ['_templates']
exclude_patterns = []

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

#html_theme = "furo"
html_theme = "sphinx_rtd_theme"
html_title = "PlaViSca"
html_theme_options = {
    # Show navigation sidebar entries for deeper levels
    'navigation_depth': 3,
    # Keep the sidebar expanded instead of collapsing sections
    'collapse_navigation': False,
    # Display the version selector if available
    'display_version': True,
}
