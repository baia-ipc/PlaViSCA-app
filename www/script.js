// function to update label styles based on input checked status
function updateLabelStyles($input) {
  const $label = $input.closest('label');
  const name = $input.attr('name');
  const id = $input.attr('id');

  // Do not skip styling for the main control buttons
  const skipNames = ['stagelr'];
  const skipIds = ['check', 'check_exp'];

  if ($input.is(':checked') && !skipNames.includes(name) && !skipIds.includes(id)) {
    $label.addClass('checked-label');
  } else {
    $label.removeClass('checked-label');
  }
}

// Smooth sidebar transition optimization
function optimizeSidebarTransition() {
  const sidebar = document.getElementById('sidebar');
  const main = document.getElementById('main');

  if (sidebar && main) {
    sidebar.style.transform = 'translateZ(0)';
    sidebar.style.willChange = 'width, opacity';
    main.style.willChange = 'width';

    // Add transition end listener to clean up will-change
    sidebar.addEventListener('transitionend', function () {
      if (this.style.willChange) {
        this.style.willChange = 'auto';
        main.style.willChange = 'auto';
      }
    });
  }
}

// Immediate sidebar control - no Shiny delays
function setupImmediateSidebarControl() {
  // Function to show/hide sidebar immediately
  function toggleSidebar(tabName) {
    const sidebar = document.getElementById('sidebar');
    const main = document.getElementById('main');

    if (!sidebar || !main) return;

    // Tabs that should hide the sidebar
    const hideSidebarTabs = ['Metadata', 'Quality control', 'Data sources', 'Gene expression', 'Embeddings'];

    if (hideSidebarTabs.includes(tabName)) {
      // Hide sidebar immediately
      sidebar.classList.remove('visible');
      main.classList.add('col-sm-12');
    } else {
      // Show sidebar (if any other tab exists)
      sidebar.classList.add('visible');
      main.classList.remove('col-sm-12');
    }
  }

  // Direct click listener on tab navigation
  function setupTabClickListeners() {
    // Listen for clicks on tab links
    $(document).on('click', 'a[data-toggle="tab"]', function (e) {
      const tabName = $(this).text().trim();
      // Immediate response - no delay
      toggleSidebar(tabName);
    });

    // Also listen for any element that might trigger tab changes
    $(document).on('click', '[data-value]', function (e) {
      const tabName = $(this).attr('data-value');
      if (tabName) {
        toggleSidebar(tabName);
      }
    });

    // Listen for Bootstrap tab events
    $(document).on('show.bs.tab', 'a[data-toggle="tab"]', function (e) {
      const tabName = $(e.target).text().trim();
      toggleSidebar(tabName);
    });
  }

  // Set initial state
  function setInitialSidebarState() {
    // Default to hide sidebar for full screen experience
    const sidebar = document.getElementById('sidebar');
    const main = document.getElementById('main');

    if (sidebar && main) {
      sidebar.classList.remove('visible');
      main.classList.add('col-sm-12');
    }
  }

  // Initialize everything
  setupTabClickListeners();
  setInitialSidebarState();
}

// Initialize immediate sidebar control when DOM is ready
$(document).ready(function () {
  setupImmediateSidebarControl();
  optimizeSidebarTransition();
});

// Optimized tooltip management with caching and throttling
let tooltipUpdateTimeout;
const tooltipCache = new Map();

function updateTooltips() {
  // Throttle tooltip updates to prevent excessive DOM manipulation
  clearTimeout(tooltipUpdateTimeout);
  tooltipUpdateTimeout = setTimeout(() => {
    const $labels = $('label[title]');

    // Dispose existing tooltips only if they exist
    if ($labels.length > 0) {
      $labels.tooltip('dispose');
    }

    $('input[type="checkbox"], input[type="radio"]').each(function () {
      const $input = $(this);
      const $label = $input.closest('label');

      if ($label.length) {
        const txt = $label.text().trim();
        const cacheKey = `${this.id}_${this.checked}`;

        let tip = tooltipCache.get(cacheKey);
        if (!tip) {
          tip = $input.is(':checked')
            ? `${txt} is selected`
            : `Click to select ${txt}`;
          tooltipCache.set(cacheKey, tip);
        }

        $label.attr('title', tip);
      }
    });

    // Reinitialize tooltips with better performance options
    $('label[title]').tooltip({
      placement: 'right',
      trigger: 'hover',
      delay: { show: 300, hide: 100 }
    });
  }, 100);
}

// Function to add highlight on Plotly 2D points
(function () {
  const originalGetContext = HTMLCanvasElement.prototype.getContext;
  HTMLCanvasElement.prototype.getContext = function (type, options) {
    if (type === '2d') {
      options = options || {};
      options.willReadFrequently = true;
    }
    return originalGetContext.call(this, type, options);
  };
})();


// Enhanced Plotly highlight with performance optimizations
let plotlyHighlightSetup = false;
let plotlyElement = null;
let lastHighlightTime = 0;
const HIGHLIGHT_THROTTLE = 100;

function setupPlotlyHighlight() {
  plotlyElement = document.getElementById('all_plotly');
  if (!plotlyElement || plotlyHighlightSetup) return;

  // Use event delegation and throttling for better performance
  $(plotlyElement).off('plotly_click.highlight').on('plotly_click.highlight', function (event, data) {
    const now = Date.now();
    if (now - lastHighlightTime < HIGHLIGHT_THROTTLE) return;
    lastHighlightTime = now;

    if (!data || !data.points || !data.points[0]) return;

    const point = data.points[0];
    if (typeof point.z !== "undefined") return;

    // Cache DOM query
    const $sizeInput = $('input[name="size"]:checked');
    if (!$sizeInput.length) return;

    const baseSize = parseInt($sizeInput.val()) || 5;
    const highlightSize = baseSize * 5;

    // Use requestAnimationFrame for smooth animations
    requestAnimationFrame(() => {
      updateHighlight(point.x, point.y, highlightSize);
    });
  });

  plotlyHighlightSetup = true;
}

function updateHighlight(xVal, yVal, highlightSize) {
  if (!plotlyElement || !plotlyElement.data) return;

  const traces = plotlyElement.data;
  const highlightIndices = traces
    .map((trace, index) => trace.name === "highlight-point" ? index : -1)
    .filter(index => index !== -1);

  const highlightTrace = {
    x: [xVal],
    y: [yVal],
    mode: "markers",
    type: "scatter",
    name: "highlight-point",
    marker: {
      size: highlightSize,
      color: '#ffa629',
      line: { width: 2, color: 'white' }
    },
    showlegend: false,
    hoverinfo: 'skip'
  };

  if (highlightIndices.length > 0) {
    Plotly.deleteTraces(plotlyElement, highlightIndices)
      .then(() => Plotly.addTraces(plotlyElement, highlightTrace))
      .catch(err => console.warn('Plotly highlight error:', err));
  } else {
    Plotly.addTraces(plotlyElement, highlightTrace)
      .catch(err => console.warn('Plotly highlight error:', err));
  }
}


// Enhanced cleanup with better resource management
function cleanupResources() {
  try {
    if (tooltipUpdateTimeout) {
      clearTimeout(tooltipUpdateTimeout);
      tooltipUpdateTimeout = null;
    }

    // Clear caches
    tooltipCache.clear();

    // Cleanup tooltips with error handling
    const $tooltips = $('label[title]');
    if ($tooltips.length > 0) {
      $tooltips.each(function () {
        try {
          $(this).tooltip('dispose');
        } catch (e) {
          // Ignore disposal errors
        }
      });
    }

    // Cleanup plotly events
    if (plotlyElement) {
      $(plotlyElement).off('plotly_click.highlight');
      plotlyElement = null;
    }
    plotlyHighlightSetup = false;
    lastHighlightTime = 0;

    // Cleanup draggable instances
    $('.modal-dialog').each(function () {
      if ($(this).hasClass('ui-draggable')) {
        try {
          $(this).draggable('destroy');
        } catch (e) {
          // Ignore destruction errors
        }
      }
    });

    // Force garbage collection hint
    if (window.gc && typeof window.gc === 'function') {
      window.gc();
    }

  } catch (error) {
    console.warn('Cleanup error:', error);
  }
}


$(document).ready(function () {
  //  Cache frequently used selectors
  const $homeSection = $("#home_section");
  const $analysisSection = $("#analysis_section");
  const $aboutSection = $("#about_section");
  const $hamburger = $('.hamburger');
  const $menuItems = $('.menu-items');

  // Show/hide initial sections
  $homeSection.show();
  $analysisSection.hide();
  $aboutSection.hide();

  // Initialize particles for home page
  // The initParticles function is defined in particles-config.js
  $(document).ready(function () {
    if (typeof initParticles === 'function') {
      initParticles('particles-js-home');
    } else {
      console.error("initParticles function not found.");
    }
  });

  // Use cached selector and single event handler  
  $hamburger.on('click', function () {
    $menuItems.toggleClass('active');
  });

  // Cache input selectors for performance
  const $inputs = $('input[type="checkbox"], input[type="radio"]');

  // Initial update of label styles and tooltips
  $inputs.each(function () {
    updateLabelStyles($(this));
  });
  updateTooltips();

  // Debounced update for better performance
  let updateTimeout;
  $inputs.on('change', function () {
    const $i = $(this);
    if ($i.attr('type') === 'radio') {
      $('input[name="' + $i.attr('name') + '"]')
        .not('[name="stagelr"]')
        .closest('label')
        .removeClass('checked-label');
    }
    updateLabelStyles($i);

    // Debounce tooltip updates
    clearTimeout(updateTimeout);
    updateTimeout = setTimeout(() => {
      updateTooltips();
    }, 50);
  });

  // Optimized button handlers with event delegation
  $(document).on('click', '.btn_add', function () {
    Shiny.setInputValue('click_add_btn', this.id, { priority: 'event' });
  });

  // ID collection with caching
  $(document).on('click', '.btn_remove', function () {
    const btn_id = this.id;
    const $dynamicCols = $('[id^="dynamic_col_"]');
    const ids = $dynamicCols.map(function () {
      return parseInt(this.id.replace("dynamic_col_", ""));
    }).get();

    // Use setTimeout for non-blocking execution
    setTimeout(function () {
      Shiny.setInputValue('click_remove_btn', { btn_id: btn_id, ids: ids },
        { priority: 'event' });
    }, 50);
  });

  //  Optimized toggle and gene expression handlers
  $(document).on('click', '.gene-expression-toggle-container input', function () {
    Shiny.setInputValue('click_toggle_exp', { id: this.id, value: this.checked },
      { priority: 'event' });
  });

  $(document).on('change', '[id^="genes_exp_"]', function () {
    Shiny.setInputValue('change_gene_exp', this.id, { priority: 'event' });
  });

  // More efficient data type change handler with throttling
  let dataTypeChangeTimeout;
  $(document).on('change', 'input[name^="data_type_exp"]', function () {
    clearTimeout(dataTypeChangeTimeout);
    dataTypeChangeTimeout = setTimeout(() => {
      const $geneElements = $('[id^="genes_exp_"]');
      const ids = $geneElements.map(function () {
        const id = this.id;
        return (!id.endsWith('-label') && !id.endsWith('-selectized')) ? id : null;
      }).get().filter(Boolean);

      Shiny.setInputValue('data_type_exp_change', { ids: ids, nonce: Date.now() },
        { priority: 'event' });
    }, 100);
  });

  // Setup Plotly 2D highlight feature
  setupPlotlyHighlight();

  // Enhanced modal management with better error handling
  let modalInstances = new Set();

  $(document).on('shown.bs.modal', '.modal', function () {
    const $dialog = $(this).find('.modal-dialog');
    const modalId = this.id || `modal_${Date.now()}`;

    // Cleanup existing draggable instance if exists
    if ($dialog.hasClass('ui-draggable')) {
      try {
        $dialog.draggable('destroy');
      } catch (e) {
        console.warn('Failed to destroy existing draggable:', e);
      }
    }

    try {
      $dialog.draggable({
        handle: '.modal-header',
        containment: 'window',
        scroll: false
      });
      modalInstances.add(modalId);
    } catch (e) {
      console.warn('Failed to create draggable modal:', e);
    }
  });

  $(document).on('hidden.bs.modal', '.modal', function () {
    const $dialog = $(this).find('.modal-dialog');
    const modalId = this.id;

    if ($dialog.hasClass('ui-draggable')) {
      try {
        $dialog.draggable('destroy');
        modalInstances.delete(modalId);
      } catch (e) {
        console.warn('Failed to destroy draggable on hide:', e);
      }
    }
  });

  // Dropdown handler
  $(document).on('click', '.dropdown-arrow', function (e) {
    e.preventDefault();
    const $wrapper = $(this).closest('#apply_filter_wrapper');
    const $selectizeInput = $wrapper.find('.selectize-input');
    if ($selectizeInput.length) {
      $selectizeInput.trigger('mousedown');
    }
  });

  // Enhanced cleanup on page unload
  $(window).on('beforeunload', function () {
    cleanupResources();
    modalInstances.clear();
  });

  // Shiny disconnection handling
  $(document).on('shiny:disconnected shiny:sessionended', function () {
    cleanupResources();
    modalInstances.clear();
  });
});



// Foldable settings cards (Filters, Plot settings): clicking a card's title
// bar folds or unfolds every card of the same group, on all tabs at once.
$(document).on('click', '.foldable-card-header', function () {
  const group = $(this).attr('data-fold-group');
  const open = $(this).attr('aria-expanded') !== 'true';
  $('.foldable-card-header[data-fold-group="' + group + '"]').attr('aria-expanded', open);
  $('.foldable-card-body[data-fold-group="' + group + '"]')
    .toggleClass('show', open)
    // let Shiny recheck the visibility of outputs inside the cards
    .trigger(open ? 'shown' : 'hidden');
});

// Number of active filters in the title of each Filters card, e.g.
// "Filters (2)"; nothing when no filter is set.
function updateFilterCounts() {
  $('.foldable-card').has('.meta-filter').each(function () {
    let n = 0;
    $(this).find('.meta-filter select').each(function () {
      const v = $(this).val();
      if (Array.isArray(v) ? v.filter(x => x !== '').length > 0 : !!v) n++;
    });
    $(this).find('.foldable-card-count').text(n > 0 ? '(' + n + ')' : '');
  });
}

$(document).on('change', '.meta-filter select', updateFilterCounts);
$(document).on('shiny:inputchanged', function (event) {
  const el = document.getElementById(event.name);
  if (el && $(el).closest('.meta-filter').length) {
    setTimeout(updateFilterCounts, 0);
  }
});

// Folding a bslib sidebar changes the width of the plots next to it: let
// plotly and other widgets resize once the sidebar has finished moving.
$(document).on('bslib.sidebar', function () {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 400);
});

// The fold button of the plot settings sidebar says what it opens.
$(function () {
  $('.embedding-layout .collapse-toggle').attr('title', 'Plot settings');
});
