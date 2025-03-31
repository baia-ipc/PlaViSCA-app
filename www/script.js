$(document).ready(function() {
  $("#home_section").show();
  $("#data_sets_section").hide();
  
  function updateLabelStyles($input) {
    var $label = $input.closest('label');
    var inputName = $input.attr('name');
    var inputId = $input.attr('id'); // Get the ID

    if ($input.is(':checked')) {
      if (inputName !== 'stagelr' && inputName !== 'data_type'
      && inputName !== 'data_type_exp' && inputId !== 'check' 
      && inputId !== 'check_exp'&& inputId !== 'check_pca'
      && inputId !== 'check_umap' && inputId !== 'check_tsne') {
        $label.addClass('checked-label');
      } else {
        $label.removeClass('checked-label');
      }
    } else {
      $label.removeClass('checked-label');
    }
  }


  function updateTooltips() {
    $('input[type="checkbox"], input[type="radio"]').each(function() {
      var $input = $(this);
      var labelText = $input.parent().text().trim();
      var isChecked = $input.is(':checked');
      var tooltipText = isChecked ? labelText + " is selected" :
      "Click to select " + labelText;

      $input.parent().attr('title', tooltipText);
    });

    $('input[type="checkbox"], input[type="radio"]').tooltip({ placement: 'right' });
  }

  // Initialize styles and tooltips
  $('input[type="checkbox"], input[type="radio"]').each(function() {
    var $input = $(this);
    updateLabelStyles($input);
  });

  updateTooltips();

  // Update styles and tooltips when changed
  $('input[type="checkbox"], input[type="radio"]').on('change', function() {
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
});
