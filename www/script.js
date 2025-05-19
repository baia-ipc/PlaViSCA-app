// www/custom.js

function bindImageMapEvents() {
  $('img[usemap]').rwdImageMaps();
  $("area").off('click').on('click', function () {
    Shiny.setInputValue('stage_click', $(this).attr('alt'), { priority: 'event' });
  });
}

$(document).ready(function () {
  // — your existing UI handlers —
  $("#home_section").show();
  $("#data_sets_section").hide();
  $("#about_section").hide();
  bindImageMapEvents();
  $(document).on('shiny:value', bindImageMapEvents);

  function updateLabelStyles($input) {
    var $label = $input.closest('label'),
        name   = $input.attr('name'),
        id     = $input.attr('id');
    if ($input.is(':checked') && name !== 'stagelr' && name !== 'data_type'
        && name !== 'data_type_exp' && id !== 'check'
        && id !== 'check_exp' && id !== 'embedding') {
      $label.addClass('checked-label');
    } else {
      $label.removeClass('checked-label');
    }
  }

  function updateTooltips() {
    $('input[type="checkbox"], input[type="radio"]').each(function () {
      var $i = $(this),
          txt = $i.parent().text().trim(),
          tip = $i.is(':checked')
                ? txt + " is selected"
                : "Click to select " + txt;
      $i.parent().attr('title', tip);
    }).tooltip({ placement: 'right' });
  }

  $('input[type="checkbox"], input[type="radio"]').each(function () {
    updateLabelStyles($(this));
  });
  updateTooltips();
  $('input[type="checkbox"], input[type="radio"]').on('change', function () {
    var $i = $(this);
    if ($i.attr('type') === 'radio') {
      $('input[name="' + $i.attr('name') + '"]')
        .not('[name="stagelr"], [name="data_type"]')
        .closest('label')
        .removeClass('checked-label');
    }
    updateLabelStyles($i);
    updateTooltips();
  });

  $(document).on('click', '.btn_add', function () {
    Shiny.setInputValue('click_add_btn', this.id, { priority: 'event' });
  });

  $(document).on('click', '.btn_remove', function () {
    var btn_id = this.id,
        ids    = $('[id^="dynamic_col_"]').map(function () {
                   return parseInt(this.id.replace("dynamic_col_", ""));
                 }).get();
    setTimeout(function () {
      Shiny.setInputValue('click_remove_btn', { btn_id: btn_id, ids: ids }, { priority: 'event' });
    }, 100);
  });

  $(document).on('click', '.toggle_exp', function () {
    Shiny.setInputValue('click_toggle_exp',
      { id: this.id, value: this.checked }, { priority: 'event' });
  });

  $(document).on('change', '[id^="genes_exp_"]', function () {
    Shiny.setInputValue('change_gene_exp', this.id, { priority: 'event' });
  });

  $(document).on('change', 'input[name^="data_type_exp"]', function () {
    var ids = [];
    $('[id^="genes_exp_"]').each(function () {
      var id = this.id;
      if (!id.endsWith('-label') && !id.endsWith('-selectized')) ids.push(id);
    });
    Shiny.setInputValue('data_type_exp_change', { ids: ids, nonce: Date.now() }, { priority: 'event' });
  });

});
