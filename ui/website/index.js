(function() {

// when the DOM loads
$(document).ready(function() {
    // click handler for "Try Julia Now" button
    $("a#submit_button").click(function() {
        // submit the form
        $("form#session_form").submit();
    });

    // submit the form when the user hits enter
    $("input#user_name").keydown(function(e) {
        if (e.keyCode == 13) {
            $("form#session_form").submit();
            return false;
        }
    });
    $("input#session_name").keydown(function(e) {
        if (e.keyCode == 13) {
            $("form#session_form").submit();
            return false;
        }
    });

    // form handler
    $("form#session_form").submit(function() {
        // send a start message
        outbox_queue.push([MSG_INPUT_START, $("input#user_name").val(), $("input#session_name").val()]);
        process_outbox();
        return false;
    });

    // focus the first input box
    $("input#user_name").focus();
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

// how long we delay in ms before polling the server again
var poll_interval = 200;

// keep track of whether we are waiting for a message (and don't send more if we are)
var waiting_for_response = false;

// a queue of messages to be sent to the server
var outbox_queue = [];

// a queue of messages from the server to be processed
var inbox_queue = [];

// keep track of whether we have received a fatal message
var dead = false;

// an array of message handlers
var message_handlers = [];

message_handlers[MSG_OUTPUT_WELCOME] = function(msg) {
    // redirect to the REPL page
    window.location = "repl.htm";
};

message_handlers[MSG_OUTPUT_MESSAGE] = function(msg) {
    // crappy way to show the user a message for now
    alert(msg[0]);
};

message_handlers[MSG_OUTPUT_FATAL_ERROR] = function(msg) {
    // crappy way to show the user a message for now
    alert(msg[0]);

    // stop processing new messages
    dead = true;
    inbox_queue = [];
    outbox_queue = [];
};

// check the server for data
function poll() {
    // send a poll message
    outbox_queue.push([MSG_INPUT_POLL]);
    process_outbox();
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

// process the messages in the inbox
function process_inbox() {
    // iterate through the messages
    for (var id in inbox_queue) {
        var msg = inbox_queue[id],
            type = msg[0], msg = msg.slice(1),
            handler = message_handlers[type];
        if (typeof handler == "function")
            handler(msg);
    }

    // we handled all the messages so clear the inbox
    inbox_queue = [];
}

})();
