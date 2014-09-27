$(document).ready(function () {
    $('.collapsible').click(function () {
        $(this).parent().find('.collapse').collapse('toggle');
    });
});
