function bindImageMapEvents() {
  $('img[usemap]').rwdImageMaps();

  $("area").off('click').on('click', function () {
    var clickedArea = $(this).attr('alt');
    Shiny.setInputValue('stage_click', clickedArea, { priority: 'event' });
  });
}

$(document).ready(function () {
  $("#home_section").show();
  $("#data_sets_section").hide();
  $("#about_section").hide();

  bindImageMapEvents();

  $(document).on('shiny:value', function () {
    bindImageMapEvents();
  });


  function updateLabelStyles($input) {
    var $label = $input.closest('label');
    var inputName = $input.attr('name');
    var inputId = $input.attr('id');

    if ($input.is(':checked')) {
      if (inputName !== 'stagelr' && inputName !== 'data_type'
        && inputName !== 'data_type_exp' && inputId !== 'check'
        && inputId !== 'check_exp' && inputId !== 'embedding') {
        $label.addClass('checked-label');
      } else {
        $label.removeClass('checked-label');
      }
    } else {
      $label.removeClass('checked-label');
    }
  }

  function updateTooltips() {
    $('input[type="checkbox"], input[type="radio"]').each(function () {
      var $input = $(this);
      var labelText = $input.parent().text().trim();
      var isChecked = $input.is(':checked');
      var tooltipText = isChecked ? labelText + " is selected" :
        "Click to select " + labelText;

      $input.parent().attr('title', tooltipText);
    });

    $('input[type="checkbox"], input[type="radio"]').tooltip({ placement: 'right' });
  }

  $('input[type="checkbox"], input[type="radio"]').each(function () {
    var $input = $(this);
    updateLabelStyles($input);
  });

  updateTooltips();
  $('input[type="checkbox"], input[type="radio"]').on('change', function () {
    var $input = $(this);

    if ($input.attr('type') === 'radio') {
      $('input[name="' + $input.attr('name') + '"]')
        .not('[name="stagelr"], [name="data_type"]')
        .closest('label')
        .removeClass('checked-label');
    }

    updateLabelStyles($input);
    updateTooltips();
  });

  // Button and toggle interactions
  $(document).on('click', '.btn_add', function () {
    var btn_id = this.id;
    Shiny.setInputValue('click_add_btn', btn_id, { priority: 'event' });
  });

  $(document).on('click', '.btn_remove', function () {
    var btn_id = this.id;
    var ids = $('[id^="dynamic_col_"]').map(function () {
      return parseInt(this.id.replace("dynamic_col_", ""));
    }).get();

    setTimeout(function () {
      Shiny.setInputValue('click_remove_btn', {
        btn_id: btn_id,
        ids: ids
      }, { priority: 'event' });
    }, 100);
  });

  $(document).on('click', '.toggle_exp', function () {
    var btn_id = this.id;
    var is_checked = this.checked;
    Shiny.setInputValue('click_toggle_exp', {
      id: btn_id,
      value: is_checked
    }, { priority: 'event' });
  });

  $(document).on('change', '[id^="genes_exp_"]', function () {
    var id = this.id;
    Shiny.setInputValue('change_gene_exp', id, { priority: 'event' });
  });

  $(document).on('change', 'input[name^="data_type"]', function () {
    Shiny.onInputChange('last_type_input', this.name);
  });


 $(document).on('change', 'input[name^="data_type_exp"]', function () {
    var dataTypeExpId = this.value;
    var genesExpIdList = [];

  $('[id^="genes_exp_"]').each(function () {
    var id = this.id;
    if (id.endsWith('-label') || id.endsWith('-selectized')) return;
    if (!genesExpIdList.includes(id)) {
      genesExpIdList.push(id);
    }
  });

  Shiny.setInputValue('data_type_exp_change', genesExpIdList, { priority: 'event' });
  });


});
