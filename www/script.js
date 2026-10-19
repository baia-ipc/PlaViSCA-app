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
    const hideSidebarTabs = ['Metadata plots', 'Metadata table', 'QC Plots', 'Data sources', 'Manual', 'Gene expression', 'Embeddings'];

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
  // Background particles of the page (particles-config.js)
  if (typeof initParticles === 'function') {
    initParticles('particles-js-analysis');
  }

  // Cache input selectors for performance
  const $inputs = $('input[type="checkbox"], input[type="radio"]');

  // Initial update of label styles
  $inputs.each(function () {
    updateLabelStyles($(this));
  });

  $inputs.on('change', function () {
    const $i = $(this);
    if ($i.attr('type') === 'radio') {
      $('input[name="' + $i.attr('name') + '"]')
        .not('[name="stagelr"]')
        .closest('label')
        .removeClass('checked-label');
    }
    updateLabelStyles($i);
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
  if (group === 'filters') {
    // folded, the Filters card is a button left of the content (style.css)
    $('.analysis-content').toggleClass('filters-folded', !open);
    window.dispatchEvent(new Event('resize'));
  }
});

// Number of active filters in the title of each Filters card, e.g.
// "Filters (2)", or on the folded Filters button; nothing when no filter is
// set.
function updateFilterCounts() {
  $('.foldable-card').has('.meta-filter').each(function () {
    let n = 0;
    $(this).find('.meta-filter select').each(function () {
      const v = $(this).val();
      if (Array.isArray(v) ? v.filter(x => x !== '').length > 0 : !!v) n++;
    });
    // brackets are added in the CSS, around the number in the unfolded title
    $(this).find('.foldable-card-count').text(n > 0 ? n : '');
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
// (native listener: jQuery would read "bslib.sidebar" as a namespaced event)
document.addEventListener('bslib.sidebar', function () {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 400);
});

// The fold button of the plot settings sidebars says what it opens.
$(function () {
  $('.plot-settings-layout .collapse-toggle').attr('title', 'Plot settings');
});

// Plot settings sidebars are open or folded together on all tabs. A sidebar
// on a hidden tab cannot be animated, so each one is brought in line with
// the last choice when its tab is shown.
let plotSettingsOpen = false;

document.addEventListener('bslib.sidebar', function (event) {
  const layout = $(event.target).closest('.plot-settings-layout');
  if (layout.length && layout.is(':visible')) {
    plotSettingsOpen = event.detail.open;
  }
});

$(document).on('shown.bs.tab', function (event) {
  // jQuery also passes the plain "shown" events of Shiny and bslib here
  if (event.namespace !== 'bs.tab' || !window.bslib || !window.bslib.Sidebar) return;
  $('.plot-settings-layout .bslib-sidebar-layout:visible:not(.transitioning)').each(function () {
    const sidebar = window.bslib.Sidebar.getInstance(this);
    if (sidebar && sidebar.isClosed === plotSettingsOpen) {
      sidebar.toggle(plotSettingsOpen ? 'open' : 'close');
    }
  });
});

// Compare Plots: while the views are linked, rotating, zooming or panning one
// plot applies the same camera (3D) or axis ranges (2D) to the other.
const linkedPlotIds = ['first_plotly', 'second_plotly'];
let linkingViews = false;

function linkedViewUpdate(data) {
  const update = {};
  for (const key in data) {
    if (/^(scene\.camera|scene\.aspectratio|xaxis\.|yaxis\.)/.test(key)) {
      update[key] = data[key];
    }
  }
  return update;
}

// the Linked / Unlinked button in the Visualization bar
let viewsLinked = true;
$(document).on('click', '#link_views', function () {
  viewsLinked = !viewsLinked;
  $(this).toggleClass('linked', viewsLinked).attr('aria-pressed', viewsLinked);
});

$(document).on('plotly_relayout plotly_relayouting', '#first_plotly, #second_plotly', function (event, data) {
  if (linkingViews || !viewsLinked || !data) return;
  const update = linkedViewUpdate(data);
  if (Object.keys(update).length === 0) return;
  const otherId = linkedPlotIds.find(id => id !== this.id);
  const other = document.getElementById(otherId);
  if (!other || !other.layout) return;
  linkingViews = true;
  Plotly.relayout(other, update).finally(function () { linkingViews = false; });
});

// Embedding plots: a double-click or right-click on a cell opens its details
// (server.R, input$embedding_cell_open); a single click does not. Plotly has
// no right-click event, so the cell under the mouse is remembered from the
// hover events.
const embeddingPlots = '#all_plotly, #first_plotly, #second_plotly';
let hoveredCellKey = null;

$(document).on('plotly_hover', embeddingPlots, function (event, data) {
  const pt = data && data.points && data.points[0];
  if (!pt) return;
  let key = pt.key;
  if (key === undefined && pt.data && pt.data.key) key = pt.data.key[pt.pointNumber];
  hoveredCellKey = key === undefined ? null : key;
});

$(document).on('plotly_unhover', embeddingPlots, function () {
  hoveredCellKey = null;
});

function openCellDetails(key) {
  Shiny.setInputValue('embedding_cell_open', { key: key, at: Date.now() }, { priority: 'event' });
}

// Plotly handles the mouse itself, so the browser's dblclick event does not
// reach the page: a double click is two Plotly clicks on the same cell
let lastCellClick = { key: null, time: 0 };
$(document).on('plotly_click', embeddingPlots, function (event, data) {
  const pt = data && data.points && data.points[0];
  if (!pt) return;
  let key = pt.key;
  if (key === undefined && pt.data && pt.data.key) key = pt.data.key[pt.pointNumber];
  if (key === undefined) return;
  const now = Date.now();
  if (lastCellClick.key === key && now - lastCellClick.time < 400) {
    lastCellClick = { key: null, time: 0 };
    openCellDetails(key);
  } else {
    lastCellClick = { key: key, time: now };
  }
});

$(document).on('contextmenu', embeddingPlots, function (event) {
  if (hoveredCellKey === null) return;
  event.preventDefault();
  openCellDetails(hoveredCellKey);
});

// Every plot gets a "Download plot as SVG" button right next to the PNG one:
// plotly's own PNG button is replaced by an equivalent one, so that the two
// sit together in one group of the toolbar. The PNG keeps each plot's
// download settings (toImageButtonOptions: file name, size, scale). In WebGL
// plots (the embeddings) the points stay a bitmap inside the SVG; the axes,
// labels and legend are vector graphics. The R plotly package sends its own
// list of extra buttons with each plot, so the buttons are added to that list
// whenever a plot is drawn.
function downloadOptions(gd, format) {
  const opts = Object.assign({ filename: gd.id || 'plot' }, gd._context.toImageButtonOptions);
  opts.format = format;
  if (format === 'svg') delete opts.scale;
  return opts;
}

const downloadButtons = [
  {
    name: 'downloadPng',
    title: 'Download plot as PNG',
    icon: null, // set once Plotly is loaded
    click: function (gd) { Plotly.downloadImage(gd, downloadOptions(gd, 'png')); }
  },
  {
    name: 'downloadSvg',
    title: 'Download plot as SVG',
    icon: null,
    click: function (gd) { Plotly.downloadImage(gd, downloadOptions(gd, 'svg')); }
  }
];

function withSvgButton(config) {
  const out = Object.assign({}, config);
  const added = (out.modeBarButtonsToAdd || []).slice();
  if (!added.some(b => b && b.name === 'downloadSvg')) added.push(...downloadButtons);
  out.modeBarButtonsToAdd = added;
  const removed = [].concat(out.modeBarButtonsToRemove || []);
  if (!removed.includes('toImage')) removed.push('toImage');
  out.modeBarButtonsToRemove = removed;
  return out;
}

function addSvgButtonToPlotly(plotly) {
  downloadButtons[0].icon = plotly.Icons.camera;
  downloadButtons[1].icon = plotly.Icons.disk;
  ['newPlot', 'react'].forEach(function (fn) {
    const original = plotly[fn];
    plotly[fn] = function (gd, data, layout, config) {
      if (data && typeof data === 'object' && !Array.isArray(data) && data.data) {
        // single-object form: Plotly.newPlot(gd, {data, layout, config})
        data = Object.assign({}, data, { config: withSvgButton(data.config) });
        return original.call(this, gd, data);
      }
      return original.call(this, gd, data, layout, withSvgButton(config));
    };
  });
}

// plotly.js is loaded with the first plot, after this script: wrap it as
// soon as it is defined
(function () {
  if (window.Plotly) {
    addSvgButtonToPlotly(window.Plotly);
    return;
  }
  let plotly;
  Object.defineProperty(window, 'Plotly', {
    configurable: true,
    get: function () { return plotly; },
    set: function (value) {
      plotly = value;
      if (value && value.newPlot) addSvgButtonToPlotly(value);
    }
  });
})();
