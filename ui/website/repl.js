(function() {

/*
    Color Schemes
*/

// array of color schemes
var color_schemes = [
    ["Dark", {
        background_color: "#000000",
        text_color: "#dddddd",
        message_color: "#0000aa",
        error_color: "#ff0000",
        prompt_color: "#00bb00",
        plot_grid_color: "#333333",
        plot_axis_color: "#666666",
        plot_text_color: "#dddddd",
        plot_line_color: "#4d87c7",
        plot_rect_color: "#4d87c7",
        plot_rect_stroke_width: "0",
        plot_rect_stroke_color: "#FFFFFF",
    }],
    ["Light", {
        background_color: "#f8f8f8",
        text_color: "#444444",
        message_color: "#0000aa",
        error_color: "#ff0000",
        prompt_color: "#00aa00",
        plot_grid_color: "#dadada",
        plot_axis_color: "#aaaaaa",
        plot_text_color: "#444444",
        plot_line_color: "#4d87b7",
        plot_rect_color: "#4d87c7",
        plot_rect_stroke_width: "0",
        plot_rect_stroke_color: "#FFFFFF",
    }],
];

// the current color scheme
var current_color_scheme = 0;

// Fetch items out of local storage if they exist
if (Modernizr.localstorage) {
    if (localStorage.getItem("current_color_scheme")) {
        current_color_scheme = localStorage.getItem("current_color_scheme");
    }
}

// apply a particular color scheme -- call this every time the terminal content changes
function apply_color_scheme() {
    $("form#terminal-form").css("background-color", color_schemes[current_color_scheme][1].background_color);
    $("div#terminal").css("color", color_schemes[current_color_scheme][1].text_color);
    $("textarea#terminal-input").css("color", color_schemes[current_color_scheme][1].text_color);
    $("span.color-scheme-message").css("color", color_schemes[current_color_scheme][1].message_color);
    $("span.color-scheme-error").css("color", color_schemes[current_color_scheme][1].error_color);
    $("span.color-scheme-prompt").css("color", color_schemes[current_color_scheme][1].prompt_color);
    $("svg .hrule line, svg .vrule line").css("stroke", color_schemes[current_color_scheme][1].plot_grid_color);
    $("svg .hrule2 line, svg .vrule2 line").css("stroke", color_schemes[current_color_scheme][1].plot_axis_color);
    $("svg text").css("fill", color_schemes[current_color_scheme][1].plot_text_color);
    $("svg .line").css("stroke", color_schemes[current_color_scheme][1].plot_line_color);
    $("svg .rect").css("fill", color_schemes[current_color_scheme][1].plot_rect_color);
    $("svg .rect").css("stroke", color_schemes[current_color_scheme][1].plot_rect_stroke_width);
    
    if (Modernizr.localstorage) {
        localStorage.setItem("current_color_scheme", current_color_scheme);
    }
}

// when the DOM loads
$(document).ready(function() {
    // add the color scheme options to the picker
    var options_str = "";
    for (var i in color_schemes)
        options_str += "<option " + (current_color_scheme === i ? "selected" : "") + ">"+color_schemes[i][0]+"</option>";
    $("select#color-scheme-picker").html(options_str);

    // add a hook to the change event of the color picker
    $("select#color-scheme-picker").change(function() {
        // determine which color scheme was selected
        var scheme_name = $("select#color-scheme-picker option:selected").html();
        for (var i in color_schemes) {
            if (color_schemes[i][0] == scheme_name) {
                current_color_scheme = i;
                break;
            }
        }

        // apply the color scheme
        apply_color_scheme();
    });

    // apply the current color scheme
    apply_color_scheme();
});

/*
    Network Protol

    This needs to match the message
    types listed in ui/webserver/message_types.h.
*/

// input messages (to julia)
var MSG_INPUT_NULL              = 0;
var MSG_INPUT_START             = 1;
var MSG_INPUT_POLL              = 2;
var MSG_INPUT_EVAL              = 3;
var MSG_INPUT_REPLAY_HISTORY    = 4;
var MSG_INPUT_GET_USER          = 5;

// output messages (to the browser)
var MSG_OUTPUT_NULL             = 0;
var MSG_OUTPUT_WELCOME          = 1;
var MSG_OUTPUT_READY            = 2;
var MSG_OUTPUT_MESSAGE          = 3;
var MSG_OUTPUT_OTHER            = 4;
var MSG_OUTPUT_EVAL_INPUT       = 5;
var MSG_OUTPUT_FATAL_ERROR      = 6;
var MSG_OUTPUT_EVAL_INCOMPLETE  = 7;
var MSG_OUTPUT_EVAL_RESULT      = 8;
var MSG_OUTPUT_EVAL_ERROR       = 9;
var MSG_OUTPUT_PLOT             = 10;
var MSG_OUTPUT_GET_USER         = 11;
var MSG_OUTPUT_HTML             = 12;

/*
    REPL implementation.
*/

// the user name
var user_name = "julia";

// the user id
var user_id = "";

// indent string
var indent_str = "    ";

// how long we delay in ms before polling the server again
var poll_interval = 300;

// keep track of whether we are waiting for a message (and don't send more if we are)
var waiting_for_response = false;

// a queue of messages to be sent to the server
var outbox_queue = [];

// a queue of messages from the server to be processed
var inbox_queue = [];

// keep track of whether new terminal data will appear on a new line
var new_line = true;

// keep track of whether we have received a fatal message
var dead = false;

// keep track of terminal history
var input_history = [];
var input_history_current = [""];
var input_history_id = 0;
var input_history_size = 1000;

// Fetch items out of local storage if they exist
if (Modernizr.localstorage) {
    if (localStorage.getItem("input_history")) {
        input_history = JSON.parse(localStorage.getItem("input_history"));
        input_history_id = input_history.length - 1;
    }
    
    if (localStorage.getItem("input_history_current")) {
        input_history_current = JSON.parse(localStorage.getItem("input_history_current"));
    }
}

// reset the width of the terminal input
function set_input_width() {
    // resize the input box (the -1 is for Internet Explorer)
    $("#terminal-input").width($("#terminal").width()-$("#prompt").width()-1);
}

// set the width of the terminal
function set_terminal_width() {
    // set the width of the terminal
    var new_terminal_width = Math.max($(window).width()-229, 200);
    $("div#right-column").width(new_terminal_width);

    // resize the page container as well
    $("div#main").width($("div#left-column").outerWidth(true)+$("div#right-column").outerWidth(true));

    // reset the width of the terminal input
    set_input_width();
}

// set the width of the terminal when the window is resized
$(window).resize(set_terminal_width);

// jQuery extensions
jQuery.fn.extend({
    // inset some text into a textarea at the cursor position
    insert_at_caret: function(str) {
        // apply this function to all elements that match the selector
        return this.each(function(i) {
            // if text is selected, then just replace it
            if (document.selection) {
                // replace the selection with str
                this.focus();
                sel = document.selection.createRange();
                sel.text = str;
                this.focus();
            } else if (this.selectionStart || this.selectionStart == "0") {
                // replace the selection with str
                var start_pos = this.selectionStart;
                var end_pos = this.selectionEnd;
                var scroll_top = this.scrollTop;
                this.value = this.value.substring(0, start_pos)+str+this.value.substring(end_pos, this.value.length);
                this.focus();
                this.selectionStart = start_pos+str.length;
                this.selectionEnd = start_pos+str.length;
                this.scrollTop = scroll_top;
            } else {
                // just add str to the end
                this.value += str;
                this.focus();
            }
        })
    },

    // remove the character in a textarea before the cursor position or de-indent
    backspace_at_caret: function() {
        // apply this function to all elements that match the selector
        return this.each(function(i) {
            // if text is selected, then just delete it
            if (document.selection) {
                this.focus();
                sel = document.selection.createRange();
                sel.text = "";
                this.focus();
            } else if (this.selectionStart || this.selectionStart == "0") {
                // get the selection
                var start_pos = this.selectionStart;
                var end_pos = this.selectionEnd;
                var scroll_top = this.scrollTop;

                // check if nothing is selected
                if (start_pos == end_pos) {
                    // only backspace if we aren't at the beginning
                    if (start_pos > 0) {
                        // check if we are far enough that we might want to de-indent
                        if (start_pos > indent_str.length-1) {
                            // check if there is indentation right before the cursor
                            if (this.value.substring(start_pos-indent_str.length, start_pos) == indent_str) {
                                // delete the indentation
                                this.value = this.value.substring(0, start_pos-indent_str.length)+this.value.substring(end_pos, this.value.length);
                                this.selectionStart = start_pos-indent_str.length;
                                this.selectionEnd = start_pos-indent_str.length;
                            } else {
                                // just delete the character before the cursor
                                this.value = this.value.substring(0, start_pos-1)+this.value.substring(end_pos, this.value.length);
                                this.selectionStart = start_pos-1;
                                this.selectionEnd = start_pos-1;
                            }
                        } else {
                            // just delete the character before the cursor
                            this.value = this.value.substring(0, start_pos-1)+this.value.substring(end_pos, this.value.length);
                            this.selectionStart = start_pos-1;
                            this.selectionEnd = start_pos-1;
                        }
                    }
                } else {
                    // just delete the selection
                    this.value = this.value.substring(0, start_pos)+this.value.substring(end_pos, this.value.length);
                    this.selectionStart = start_pos;
                    this.selectionEnd = start_pos;
                }

                // focus the element and scroll it appropriately
                this.focus();
                this.scrollTop = scroll_top;
            }
        })
    },

    // insert a newline in a textarea at the cursor position and auto-indent
    newline_at_caret: function() {
        // apply this function to all elements that match the selector
        return this.each(function(i) {
            // determine the indentation for this line
            var indent = "";
            if (this.selectionStart || this.selectionStart == "0") {
                // determine the start of the indentation
                var start_pos = this.selectionStart;
                while (start_pos > 0) {
                    if (this.value[start_pos-1] == "\n")
                        break;
                    start_pos -= 1;
                }

                // determine the end of the indentation
                var end_pos = start_pos;
                while (end_pos < this.value.length) {
                    if (this.value[end_pos] != " ")
                        break;
                    end_pos += 1;
                }

                // get the indentation
                indent = this.value.substring(start_pos, end_pos);
            }

            // insert a newline and auto-indent
            $(this).insert_at_caret("\n"+indent);
        })
    },
});

// escape html
function escape_html(str) {
    // escape ampersands, angle brackets, tabs, and newlines
    return str.replace(/\t/g, "    ").replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\n/g, "<br />");
}

// indent and escape html
function indent_and_escape_html(str) {
    // indent newlines to line up with the end of the julia prompt
    return escape_html(str.replace(/\n/g, "\n       "));
}

function add_html_to_terminal(data) {
    // update the html
    $("#terminal").append(data);

    // apply the color scheme to the new content
    apply_color_scheme();

    // reset the size of the input box
    set_input_width();

    // scroll to the new data
    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));

    new_line = false;
}

// add html to the terminal (preserving whitespace)
function add_to_terminal(data) {
    // preserve whitespace with non-breaking spaces
    var new_data = "";
    var tag_depth = 0;
    for (var i = 0; i < data.length; i += 1) {
        if (data[i] == "<")
            tag_depth += 1;
        if (data[i] == ">")
            tag_depth -= 1;
        if (tag_depth == 0 && i < data.length-1) {
            if (data[i] == " " && data[i+1] == " ")
                new_data += "&nbsp;";
            else
                new_data += data[i];
        } else {
            new_data += data[i];
        }
    }

    add_html_to_terminal(new_data);

    // determine whether the last thing added was a newline
    if (new_data.length >= 6)
        new_line = (new_data.substr(new_data.length-6, 6) == "<br />");
}

// the first request
function init_session() {
    // send a start message
    outbox_queue.push([MSG_INPUT_GET_USER]);
    outbox_queue.push([MSG_INPUT_REPLAY_HISTORY]);
    process_outbox();
}

// check the server for data
function poll() {
    // send a poll message
    outbox_queue.push([MSG_INPUT_POLL]);
    process_outbox();
}

// send the messages in the outbox
function process_outbox() {
    // don't make new requests if we're waiting for old ones
    if (!waiting_for_response) {
        // don't send a request if there are no messages
        if (outbox_queue.length > 0) {
            // don't send any more requests while we're waiting for this one
            waiting_for_response = true;

            // send the messages
            $.post("/repl.scgi", {"request": $.toJSON(outbox_queue)}, callback, "json");
        }

        // we sent all the messages at once so clear the outbox
        outbox_queue = [];
    }
}

// an array of message handlers
var message_handlers = [];

message_handlers[MSG_OUTPUT_NULL] = function(msg) {}; // do nothing

message_handlers[MSG_OUTPUT_READY] = function(msg) {
    // remove the initializing message
    $("#terminal").html("");

    // enable input
    $("#prompt").show();
    $("#terminal-input").removeAttr("disabled");
    $("#terminal-input").show();
    $("#terminal-input").focus();

    // reset the size of the input box
    set_input_width();
};

message_handlers[MSG_OUTPUT_MESSAGE] = function(msg) {
    // print the message
    add_to_terminal("<span class=\"color-scheme-message\">"+escape_html(msg[0])+"</span><br /><br />");
};

message_handlers[MSG_OUTPUT_OTHER] = function(msg) {
    // just print the output
    add_to_terminal(escape_html(msg[0]));
};

message_handlers[MSG_OUTPUT_FATAL_ERROR] = function(msg) {
    // print the error message
    add_to_terminal("<span class=\"color-scheme-error\">"+escape_html(msg[0])+"</span><br /><br />");

    // stop processing new messages
    dead = true;
    inbox_queue = [];
    outbox_queue = [];
};

message_handlers[MSG_OUTPUT_EVAL_INPUT] = function(msg) {
    // check if this was from us
    if (msg[0] == user_id) {
        // get the input from form
        var input = $("#terminal-input").val();

        // input history
        if (input.replace(/^\s+|\s+$/g, '') != "")
            input_history.push(input);
        if (input_history.length > input_history_size)
            input_history = input_history.slice(input_history.length-input_history_size);
        input_history_current = input_history.slice(0);
        input_history_current.push("");
        input_history_id = input_history_current.length-1;
        
        // Save the changed values to localstorage
        if (Modernizr.localstorage) {
            localStorage.setItem("input_history", JSON.stringify(input_history));
            localStorage.setItem("input_history_current", JSON.stringify(input_history_current));
        }

        // clear the input field (it is disabled at this point)
        $("#terminal-input").val("");

        // hide the prompt until the result comes in
        $("#prompt").hide();
    }

    // add the prompt and the input to the log
    add_to_terminal("<span class=\"color-scheme-prompt\">"+indent_and_escape_html(msg[1])+"&gt;&nbsp;</span>"+indent_and_escape_html(msg[2])+"<br />");
}

message_handlers[MSG_OUTPUT_EVAL_INCOMPLETE] = function(msg) {
    // re-enable the input field
    $("#terminal-input").removeAttr("disabled");

    // focus the input field
    $("#terminal-input").focus();

    // add a newline for the user
    $("#terminal-input").newline_at_caret();
};

function enable_prompt() {
    // show the prompt
    $("#prompt").show();

    // re-enable the input field
    $("#terminal-input").removeAttr("disabled");

    // focus the input field
    $("#terminal-input").focus();
}

message_handlers[MSG_OUTPUT_EVAL_ERROR] = function(msg) {
    // print the error message
    add_to_terminal("<span class=\"color-scheme-error\">"+escape_html(msg[1])+"</span><br /><br />");

    // check if this was from us
    if (msg[0] == user_id) {
        enable_prompt();
    }
};

message_handlers[MSG_OUTPUT_EVAL_RESULT] = function(msg) {
    // print the result
    if ($.trim(msg[1]) == "")
        add_to_terminal("<br />");
    else
        add_to_terminal(escape_html(msg[1])+"<br /><br />");

    if (msg[0] == user_id) {
        enable_prompt();
    }
};

message_handlers[MSG_OUTPUT_HTML] = function(msg) {
    add_html_to_terminal(msg[1]);
    if (msg[0] == user_id) {
        enable_prompt();
    }
}

message_handlers[MSG_OUTPUT_GET_USER] = function(msg) {
    // set the user name
    user_name = indent_and_escape_html(msg[0]);
    user_id = indent_and_escape_html(msg[1]);
    $("#prompt").html("<span class=\"color-scheme-prompt\">"+user_name+"&gt;&nbsp;</span>");
    apply_color_scheme();
}

var plotters = {};

plotters["line"] = function(plot) {
    // local variables
    var xpad = 0,
        ypad = (plot.y_max-plot.y_min)*0.1,
        x = d3.scale.linear().domain([plot.x_min - xpad, plot.x_max + xpad]).range([0, plot.w]),
        y = d3.scale.linear().domain([plot.y_min - ypad, plot.y_max + ypad]).range([plot.h, 0]),
        xticks = x.ticks(8),
        yticks = y.ticks(8);

    // create an SVG canvas and a group to represent the plot area
    var vis = d3.select("#terminal")
      .append("svg")
        .data([d3.zip(plot.x_data, plot.y_data)]) // coordinate pairs
        .attr("width", plot.w+plot.p*2)
        .attr("height", plot.h+plot.p*2)
      .append("g")
        .attr("transform", "translate("+String(plot.p)+","+String(plot.p)+")");

    // vertical tics
    var vrules = vis.selectAll("g.vrule")
        .data(xticks)
      .enter().append("g")
        .attr("class", "vrule");

    // horizontal tics
    var hrules = vis.selectAll("g.hrule")
        .data(yticks)
      .enter().append("g")
        .attr("class", "hrule");

    // vertical lines
    vrules.filter(function(d) { return (d != 0); }).append("line")
        .attr("x1", x)
        .attr("x2", x)
        .attr("y1", 0)
        .attr("y2", plot.h - 1);

    // horizontal lines
    hrules.filter(function(d) { return (d != 0); }).append("line")
        .attr("y1", y)
        .attr("y2", y)
        .attr("x1", 0)
        .attr("x2", plot.w + 1);

    // x-axis labels
    vrules.append("text")
        .attr("x", x)
        .attr("y", plot.h + 10)
        .attr("dy", ".71em")
        .attr("text-anchor", "middle")
        .text(x.tickFormat(10));

    // y-axis labels
    hrules.append("text")
        .attr("y", y)
        .attr("x", -5)
        .attr("dy", ".35em")
        .attr("text-anchor", "end")
        .text(y.tickFormat(10));

    // y-axis
    var vrules2 = vis.selectAll("g.vrule2")
        .data(xticks)
      .enter().append("g")
        .attr("class", "vrule2");

    // x-axis
    var hrules2 = vis.selectAll("g.hrule2")
        .data(yticks)
      .enter().append("g")
        .attr("class", "hrule2");

    // y-axis line
    vrules2.filter(function(d) { return (d == 0); }).append("line")
        .attr("x1", x)
        .attr("x2", x)
        .attr("y1", 0)
        .attr("y2", plot.h - 1);

    // x-axis line
    hrules2.filter(function(d) { return (d == 0); }).append("line")
        .attr("y1", y)
        .attr("y2", y)
        .attr("x1", 0)
        .attr("x2", plot.w + 1);

    // actual plot curve
    vis.append("path")
        .attr("class", "line")
        .attr("d", d3.svg.line()
        .x(function(d) { return x(d[0]); })
        .y(function(d) { return y(d[1]); }));

    // newline
    add_to_terminal("<br />");

    // scroll to the new plot
    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));
};

plotters["bar"] = function(plot) {
    var data = d3.zip(plot.x_data, plot.y_data); // coordinate pairs

    // local variables
    var x = d3.scale.linear().domain(d3.extent(plot.x_data)).range([0, plot.w]),
        y = d3.scale.linear().domain([0, d3.max(plot.y_data)]).range([0, plot.h]),
        xticks = x.ticks(8),
        yticks = y.ticks(8);

    // create an SVG canvas and a group to represent the plot area
    var vis = d3.select("#terminal")
      .append("svg")
        .data([data])
        .attr("width", plot.w+plot.p*2)
        .attr("height", plot.h+plot.p*2)
      .append("g")
        .attr("transform", "translate("+String(plot.p)+","+String(plot.p)+")");

    // horizontal ticks
    var hrules = vis.selectAll("g.hrule")
        .data(yticks)
      .enter().append("g")
        .attr("class", "hrule")
        .attr("transform", function(d) { return "translate(0," + (plot.h-y(d)) + ")"; });

    // horizontal lines
    hrules.append("line")
        .attr("x1", 0)
        .attr("x2", plot.w);

    // y-axis labels
    hrules.append("text")
        .attr("x", -5)
        .attr("dy", ".35em")
        .attr("text-anchor", "end")
        .text(y.tickFormat(10));

    // x-axis rules container
    var vrules = vis.selectAll("g.vrule")
        .data(xticks)
        .enter().append("g")
        .attr("class", "vrule")
        .attr("transform", function(d) { return "translate(" + (x(d)) + ",0)"; });

    // x-axis labels
    vrules.append("text")
        .attr("y", plot.h + 20)
        .attr("dx", "0")
        .attr("text-anchor", "middle")
        .text(x.tickFormat(10));

    // Redfining domain/range to fit the bars within the width
    x.domain([0, 1]).range([0, plot.w/data.length]);

    // actual plot curve
    vis.selectAll("rect")
        .data(data)
        .enter().append("rect")
        .attr("class", "rect")
        .attr("x", function(d, i) { return x(i); })
        .attr("y", function(d) { return plot.h - y(d[1]); })
        .attr("width", (plot.w - plot.p*2) / data.length)
        .attr("height", function(d) { return y(d[1]); });

    // newline
    add_to_terminal("<br />");

    // scroll to the new plot
    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));
};

message_handlers[MSG_OUTPUT_PLOT] = function(msg) {
    var plottype = msg[0],
        plot = {
            "x_data": eval(msg[1]),
            "y_data": eval(msg[2]),
            "x_min": eval(msg[3]),
            "x_max": eval(msg[4]),
            "y_min": eval(msg[5]),
            "y_max": eval(msg[6])
        },
        plotter = plotters[plottype];

    // TODO:
    // * calculate dynamically based on window size
    // * update above calculation with window resize
    // * allow user to resize
    plot.w = 450;
    plot.h = 275;
    plot.p = 40;

    if (typeof plotter == "function")
        plotter(plot);
};

// process the messages in the inbox
function process_inbox() {
    // iterate through the messages
    for (var id in inbox_queue) {
        var msg = inbox_queue[id],
            type = msg[0], msg = msg.slice(1),
            handler = message_handlers[type];
        if (typeof handler == "function")
            handler(msg);
        if (dead)
            break;
    }

    // we handled all the messages so clear the inbox
    inbox_queue = [];
}

// called when the server has responded
function callback(data, textStatus, jqXHR) {
    // if we are dead, don't keep polling the server
    if (dead)
        return;

    // allow sending new messages
    waiting_for_response = false;

    // add the messages to the inbox
    inbox_queue = inbox_queue.concat(data);

    // process the inbox
    process_inbox();

    // send any new messages
    process_outbox();

    // poll the server again shortly
    setTimeout(poll, poll_interval);
}

// called on page load
$(document).ready(function() {
    // apply the autoresize plugin to the textarea
    $("#terminal-input").autoResize({ animate: false, maxHeight: 1000, onAfterResize: function() {
        setTimeout(function() { $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight")); }, 100);
        set_input_width();
    } });

    // clear the textarea in case the browser decides to pre-populate it
    $("#terminal-input").val("");

    // set the width of the terminal
    set_terminal_width();

    // record the cursor position when the user clicks anywhere
    var mouse_x, mouse_y;
    $(window).mousedown(function(evt) {
        mouse_x = evt.pageX;
        mouse_y = evt.pageY;
    });

    // focus the terminal input when the user clicks on the terminal (but not drags)
    $("#terminal-form").click(function(evt) {
        if ((mouse_x-evt.pageX)*(mouse_x-evt.pageX)+(mouse_y == evt.pageY)*(mouse_y == evt.pageY) < 4)
            $("#terminal-input").focus();
    });

    // hook keyboard events for the input field
    $("#terminal-input").keydown(function(evt) {
        // determine which key was pressed
        switch (evt.keyCode) {
            case 8:
                // user pressed the backspace key -- make sure the terminal input was enabled
                if (!$("#terminal-input").attr("disabled")) {
                    // backspace
                    $("#terminal-input").backspace_at_caret();
                    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));
                }
                return false;

            case 9:
                // user pressed the tab key -- make sure the terminal input was enabled
                if (!$("#terminal-input").attr("disabled")) {
                    // indent
                    $("#terminal-input").insert_at_caret(indent_str);
                    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));
                }
                return false;

            case 38:
                // user pressed the up key -- make sure the terminal input was enabled
                if (!$("#terminal-input").attr("disabled")) {
                    // terminal input history
                    input_history_current[input_history_id] = $("#terminal-input").val();
                    input_history_id -= 1;
                    if (input_history_id < 0)
                        input_history_id = 0;
                        
                    // Save values to localstorage    
                    if (Modernizr.localstorage) {
                        localStorage.setItem("input_history_current", JSON.stringify(input_history_current));
                    }
                    
                    $("#terminal-input").val(input_history_current[input_history_id]);
                    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));
                }
                return false;

            case 40:
                // user pressed the down key -- make sure the terminal input was enabled
                if (!$("#terminal-input").attr("disabled")) {
                    // terminal input history
                    input_history_current[input_history_id] = $("#terminal-input").val();
                    input_history_id += 1;
                    if (input_history_id > input_history_current.length-1)
                        input_history_id = input_history_current.length-1;
                        
                    // Save values to localstorage    
                    if (Modernizr.localstorage) {
                        localStorage.setItem("input_history_current", JSON.stringify(input_history_current));
                    }
                    
                    $("#terminal-input").val(input_history_current[input_history_id]);
                    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));
                }
                return false;

            case 13:
                // user pressed the enter key -- make sure the terminal input was enabled
                if (!$("#terminal-input").attr("disabled")) {
                    // disable the terminal input
                    $("#terminal-input").attr("disabled", "disabled");

                    // get the input
                    var input = $("#terminal-input").val();

                    // send the input to the server via AJAX
                    outbox_queue.push([MSG_INPUT_EVAL, user_name, user_id, input]);
                    process_outbox();
                }

                // prevent the form from actually submitting
                return false;
                
            case 67:
                // C key pressed
                if (evt.ctrlKey) {
                    // ctrl-c to cancel a command
                    
                    add_to_terminal("<span class=\"color-scheme-error\">Process Killed<span><br /><br />");
                    
                    // show the prompt
                    $("#prompt").show();

                    // re-enable the input field
                    $("#terminal-input").removeAttr("disabled");

                    // focus the input field
                    $("#terminal-input").focus();
                    
                    waiting_for_message = false;
                }
                
            //return false;
        }
    });

    // scroll to the input field
    $("#terminal-form").prop("scrollTop", $("#terminal-form").prop("scrollHeight"));

    // start polling the server
    init_session();
});

})();
