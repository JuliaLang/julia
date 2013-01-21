/*
 * sidebar.js
 * ~~~~~~~~~~
 *
 * This script manages the Sphinx sidebar position, scrolling and collapsing
 * for the Julia theme.
 *
 * Based on the sidebar script for the default sphinx theme.
 *
 * default sphinx theme:
 * :copyright: Copyright 2007-2011 by the Sphinx team, see AUTHORS.
 * :license: BSD, see LICENSE for details.
 *
 * julia theme:
 * :copyright: Copyright 2012 by the Julia authors, see AUTHORS.
 * :license: MIT, see  LICENSE for details.
 *
 */

$(function() {
  // global elements used by the functions.
  var body= $('div.body');
  var bodywrapper = $('.bodywrapper');
  var footerwrapper = $('.footerwrapper');
  var sidebar = $('.sphinxsidebar');
  var sidebarview = $('.sphinxsidebarview');
  var sidebarcontent = $('.sphinxsidebarcontent');
  var sidebarbuttondiv = $('.sphinxsidebarbutton');
  var sidebarbutton = $('#sidebarbutton');
  var searchq = $('.search [name=q]');

  var uncollapsed_height = 0;

  // if for some reason the document has no sidebar, do not run into errors
  if (!sidebar.length) return;

  // original margin-left of the bodywrapper and width of the sidebar
  // with the sidebar expanded
  var bw_margin_expanded = bodywrapper.css('margin-left');
  var ssb_width_expanded = sidebar.width();

  // margin-left of the bodywrapper and width of the sidebar
  // with the sidebar collapsed
  var bw_margin_collapsed = '40px';
  var ssb_width_collapsed = '30px';

  // sidebar paddings
  var barpadtop = parseFloat(sidebar.css('padding-top'));
  var barpadbottom = parseFloat(sidebar.css('padding-bottom'));
  var barpad = barpadtop + barpadbottom;

  var barscroll = 0;

  function viewOffsetTop(elem) {
    // NOTE: jQuery's offset() doesn't work (evaluates nodes
    //       as being disconnected and returns 0); plus we need
    //       the offset relative to the viewport, so this approach
    //       is more direct
    return elem[0].getBoundingClientRect().top;
  }

  function sidebar_is_collapsed() {
    return sidebarcontent.is(':not(:visible)');
  }

  function toggle_sidebar() {
    if (sidebar_is_collapsed())
      expand_sidebar();
    else
      collapse_sidebar();
  }

  function compute_uncollapsed_height() {
    var isc = sidebar_is_collapsed();
    if (isc)
      expand_sidebar();
    uncollapsed_height = Math.min(
            sidebarcontent.outerHeight(),
            sidebarcontent.parent().height() );
    if (isc)
      collapse_sidebar();
  }

  function get_collapsed_height() {
    var bartop = viewOffsetTop(sidebar);
    return Math.min(uncollapsed_height, $(window).height() - barpad - bartop);
  }

  function set_button_margin() {
    var bartop = viewOffsetTop(sidebar);
    var barheight = sidebarbuttondiv.height();
    var buttonheight = sidebarbutton.height();
    var buttonscroll = sidebarview.scrollTop();
    var newmargin = (Math.min(barheight, $(window).height() - barpad - bartop) - buttonheight) / 2 + buttonscroll;
    sidebarbutton.css('margin-top', newmargin);
    searchq.css('width', sidebarcontent.width() - 50);
  }

  function collapse_sidebar() {
    barscroll = sidebarview.scrollTop();
    sidebarview.scrollTop(0);

    var newheight = get_collapsed_height();

    sidebarcontent.hide();
    sidebar.css('width', ssb_width_collapsed);
    var newmargins = {
      'margin-left': bw_margin_collapsed,
      'margin-right': bw_margin_collapsed,
    };
    bodywrapper.css(newmargins);
    footerwrapper.css(newmargins);
    sidebarbuttondiv.css({
      'height': newheight,
      'border-radius': '5px',
      'float': '',
    });
    sidebarbutton.find('span').text('»');
    sidebarbuttondiv.attr('title', _('Expand sidebar'));
    set_button_margin();
    document.cookie = 'sidebar=collapsed';
  }

  function expand_sidebar() {
    var newmargins = {
      'margin-left': bw_margin_expanded,
      'margin-right': bw_margin_expanded,
    };
    bodywrapper.css(newmargins);
    footerwrapper.css(newmargins);
    sidebar.css('width', '');
    sidebarcontent.show();
    compute_uncollapsed_height();
    sidebarbuttondiv.css({
      'height': uncollapsed_height,
      'border-radius': '0 5px 5px 0',
      'float': 'right',
    });
    sidebarbutton.find('span').text('«');
    sidebarbuttondiv.attr('title', _('Collapse sidebar'));
    set_button_margin();
    sidebarview.scrollTop(barscroll);
    document.cookie = 'sidebar=expanded';
  }

  function prepare_sidebar_button() {
    sidebarbuttondiv.click(toggle_sidebar);
    sidebarbuttondiv.attr('title', _('Collapse sidebar'));
  }

  function set_state_from_cookie() {
    if (!document.cookie)
      return;
    var items = document.cookie.split(';');
    for(var k=0; k<items.length; k++) {
      var key_val = items[k].split('=');
      var key = key_val[0];
      if (key == 'sidebar') {
        var value = key_val[1];
        if ((value == 'collapsed') && (!sidebar_is_collapsed()))
          collapse_sidebar();
        else if ((value == 'expanded') && (sidebar_is_collapsed()))
          expand_sidebar();
      }
    }
  }

  function set_sidebar_pos() {
    var bodyoff = viewOffsetTop(body);
    sidebar.css('top', Math.max(bodyoff,0));
    if (!sidebar_is_collapsed()) {
      sidebar.css('width', '');
      compute_uncollapsed_height();
      sidebarbuttondiv.css('height', uncollapsed_height);
    } else {
      var newheight = get_collapsed_height();
      sidebarbuttondiv.css('height', newheight);
    }
    set_button_margin();
  }

  prepare_sidebar_button();
  set_state_from_cookie();
  compute_uncollapsed_height();
  set_sidebar_pos();
  sidebar.css('margin-left','auto');
  $(window).scroll(set_sidebar_pos);
  sidebarview.scroll(set_button_margin);
  $(window).resize(set_sidebar_pos);

  $("img[alt]").each(function(){
      $(this).attr('title', $(this).attr('alt'));
  });

});

