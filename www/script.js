// function to update label styles based on input checked status
function updateLabelStyles($input) {
  const $label = $input.closest('label');
  const name = $input.attr('name');
  const id = $input.attr('id');

  const skipNames = ['stagelr', 'data_type', 'data_type_exp'];
  const skipIds = ['check', 'check_exp', 'embedding'];

  if ($input.is(':checked') && !skipNames.includes(name) && !skipIds.includes(id)) {
    $label.addClass('checked-label');
  } else {
    $label.removeClass('checked-label');
  }
}

// function to update tooltips on all checkboxes and radios
function updateTooltips() {
  $('input[type="checkbox"], input[type="radio"]').each(function () {
    const $i = $(this);
    const txt = $i.parent().text().trim();
    const tip = $i.is(':checked')
      ? `${txt} is selected`
      : `Click to select ${txt}`;
    $i.parent().attr('title', tip);
  }).tooltip({ placement: 'right' });
}

// function to bind image map events
function bindImageMapEvents() {
  $('img[usemap]').rwdImageMaps();
  $("area").off('click').on('click', function () {
    Shiny.setInputValue('stage_click', $(this).attr('alt'), { priority: 'event' });
  });
}

// Function to add highlight on Plotly 2D points
function setupPlotlyHighlight() {
  const el = document.getElementById('all_plotly');
  if (!el) return;

  $(el).on('plotly_click', function (event, data) {
    if (!data || !data.points || !data.points[0]) {
      return;
    }
    const point = data.points[0];
    if (typeof point.z !== "undefined") {
      return;
    }

    const selectedSize = $('input[name="size"]:checked').val();
    const baseSize = parseInt(selectedSize);

    const xVal = point.x;
    const yVal = point.y;
    const highlightSize = baseSize * 5;

    const traces = el.data;
    const highlightIndices = [];

    for (let i = 0; i < traces.length; i++) {
      if (traces[i].name === "highlight-point") {
        highlightIndices.push(i);
      }
    }

    if (highlightIndices.length > 0) {
      Plotly.deleteTraces(el, highlightIndices).then(() => {
        addHighlightTrace();
      });
    } else {
      addHighlightTrace();
    }

    function addHighlightTrace() {
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
      Plotly.addTraces(el, highlightTrace);
    }
  });
}


$(document).ready(function () {
  // Show/hide initial sections
  $("#home_section").show();
  $("#data_sets_section").hide();
  $("#about_section").hide();

  // Bind image map events
  bindImageMapEvents();

  // Setup shiny:value event to rebind image map events when needed
  $(document).on('shiny:value', bindImageMapEvents);

  // Initial update of label styles and tooltips
  $('input[type="checkbox"], input[type="radio"]').each(function () {
    updateLabelStyles($(this));
  });
  updateTooltips();

  // Update label styles and tooltips on change
  $('input[type="checkbox"], input[type="radio"]').on('change', function () {
    const $i = $(this);
    if ($i.attr('type') === 'radio') {
      $('input[name="' + $i.attr('name') + '"]')
        .not('[name="stagelr"], [name="data_type"]')
        .closest('label')
        .removeClass('checked-label');
    }
    updateLabelStyles($i);
    updateTooltips();
  });

  // Click handlers for buttons
  $(document).on('click', '.btn_add', function () {
    Shiny.setInputValue('click_add_btn', this.id, { priority: 'event' });
  });

  $(document).on('click', '.btn_remove', function () {
    const btn_id = this.id;
    const ids = $('[id^="dynamic_col_"]').map(function () {
      return parseInt(this.id.replace("dynamic_col_", ""));
    }).get();
    setTimeout(function () {
      Shiny.setInputValue('click_remove_btn', { btn_id: btn_id, ids: ids },
      { priority: 'event' });
    }, 100);
  });

  // Toggle expression checkboxes change
  $(document).on('click', '.toggle_exp', function () {
    Shiny.setInputValue('click_toggle_exp', { id: this.id, value: this.checked },
    { priority: 'event' });
  });

  // Genes expression checkboxes change
  $(document).on('change', '[id^="genes_exp_"]', function () {
    Shiny.setInputValue('change_gene_exp', this.id, { priority: 'event' });
  });

  // Data type expression radio buttons change
  $(document).on('change', 'input[name^="data_type_exp"]', function () {
    const ids = [];
    $('[id^="genes_exp_"]').each(function () {
      const id = this.id;
      if (!id.endsWith('-label') && !id.endsWith('-selectized')) ids.push(id);
    });
    Shiny.setInputValue('data_type_exp_change', { ids: ids, nonce: Date.now() },
    { priority: 'event' });
  });

  // Setup Plotly 2D highlight feature
  setupPlotlyHighlight();
  
  // Movable modal
  $(document).on('shown.bs.modal', '.modal', function () {
    var $dialog = $(this).find('.modal-dialog');
    $dialog.draggable({
      handle: '.modal-header'
    });
  });
  
  
});


