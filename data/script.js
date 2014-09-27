$(document).ready(function () {
    $('.collapsible').click(function () {
        var $btn = $(this);
        $btn.parent().find('.collapse').collapse('toggle');
        $btn.find('span').toggleClass('glyphicon-folder-open glyphicon-folder-close');
    });
});
