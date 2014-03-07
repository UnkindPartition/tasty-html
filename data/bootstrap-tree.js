$(document).ready(function() {
        $('.tree > ul').attr('role', 'tree').find('ul').attr('role', 'group');
        $('.tree').find('li:has(ul)').addClass('parent_li').attr('role', 'treeitem').find(' > span').attr('title', 'Collapse this test group').on('click', function (e) {
        var children = $(this).parent('li.parent_li').find(' > ul > li');
        if (children.is(':visible')) {
                children.hide('fast');
                $(this).attr('title', 'Expand this test group').find(' > i').addClass('icon-folder-close').removeClass('icon-folder-open');
        }
        else {
                children.show('fast');
                $(this).attr('title', 'Collapse this test group').find(' > i').addClass('icon-folder-open').removeClass('icon-folder-close');
        }
        e.stopPropagation();
    });
});
