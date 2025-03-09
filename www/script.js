$(document).ready(function() {
  $("#home_section").show();
  $("#data_sets_section").hide();

function updateLabelStyles($input) {
  var $label = $input.closest('label');

  if ($input.is(':checked')) {
    if ($input.attr('name') !== 'stagelr') {
      $label.addClass('checked-label');
    } else {
      $label.removeClass('checked-label');
    }
  } else {
    $label.removeClass('checked-label');
  }
}

$('input[type="checkbox"], input[type="radio"]').each(function() {
  var $input = $(this);

  $input.on('change', function() {
    if ($input.attr('type') === 'radio') {
      $('input[name="' + $input.attr('name') + '"]')
        .not('[name="stagelr"]')
        .closest('label')
        .removeClass('checked-label');
    }
    updateLabelStyles($input);
  });

  updateLabelStyles($input);
});

});
