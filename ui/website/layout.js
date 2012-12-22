/*
    Some stuff to make the two columns extend to the bottom of the page.
*/

// set the outer height of an element or group of elements
function set_outer_height(selector, height) {
    $(selector).height(1);
    $(selector).height(height+1-$(selector).outerHeight(true));
}

// this function makes all of the columns the same height
function set_column_heights() {
    // determine the new height of the columns
    var new_columns_height = Math.max(350, $(window).height());

    // set the height of all the columns to the height of the window
    set_outer_height("#left-column", new_columns_height);
    set_outer_height("#right-column", new_columns_height);

    // make the terminal take up the full height of the right column
    set_outer_height("#terminal-form", new_columns_height);
}

// adjust the heights of the columns when the page loads or is resized
$(document).ready(set_column_heights);
$(window).resize(set_column_heights);
