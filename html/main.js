// This file is part of Mongoose project, http://code.google.com/p/mongoose

var chat = {
  // Backend URL, string.
  // 'http://backend.address.com' or '' if backend is the same as frontend
  backendUrl: '',
  maxVisibleMessages: 10,
  errorMessageFadeOutTimeoutMs: 2000,
  errorMessageFadeOutTimer: null,
  lastMessageId: 0,
  getMessagesIntervalMs: 1000,
};

chat.normalizeText = function(text) {
  return text.replace('<', '&lt;').replace('>', '&gt;');
};

chat.refresh = function(data) {
  $.each(data, function(index, entry) {
    var row = $('<div>').addClass('message-row').appendTo('#mml');
    var timestamp = (new Date(entry.timestamp * 1000)).toLocaleTimeString();
    $('<span>')
      .addClass('message-timestamp')
      .html('[' + timestamp + ']')
      .prependTo(row);
    $('<span>')
      .addClass('message-user')
      .addClass(entry.user ? '' : 'message-user-server')
      .html(chat.normalizeText((entry.user || '[server]') + ':'))
      .appendTo(row);
    $('<span>')
      .addClass('message-text')
      .addClass(entry.user ? '' : 'message-text-server')
      .html(chat.normalizeText(entry.text))
      .appendTo(row);
    chat.lastMessageId = Math.max(chat.lastMessageId, entry.id) + 1;
  });

  // Keep only chat.maxVisibleMessages, delete older ones.
  while ($('#mml').children().length > chat.maxVisibleMessages) {
    $('#mml div:first-child').remove();
  }
};

chat.getMessages = function() {
  $.ajax({
    dataType: 'jsonp',
    url: chat.backendUrl + '/ajax/get_messages',
    data: {last_id: chat.lastMessageId},
    success: chat.refresh,
    error: function() {
    },
  });
  window.setTimeout(chat.getMessages, chat.getMessagesIntervalMs);
};

chat.handleMenuItemClick = function(ev) {
  $('.menu-item').removeClass('menu-item-selected');  // Deselect menu buttons
  $(this).addClass('menu-item-selected');  // Select clicked button
  $('.main').addClass('hidden');  // Hide all main windows
  $('#' + $(this).attr('name')).removeClass('hidden');  // Show main window
};

chat.showError = function(message) {
  $('#error').html(message).fadeIn('fast');
  window.clearTimeout(chat.errorMessageFadeOutTimer);
  chat.errorMessageFadeOutTimer = window.setTimeout(function() {
      $('#error').fadeOut('slow');
  }, chat.errorMessageFadeOutTimeoutMs);
};

chat.handleMessageInput = function(ev) {
  var input = ev.target;
  if (ev.keyCode != 13 || !input.value)
    return;
  //input.disabled = true;
  $.ajax({
    dataType: 'jsonp',
    url: chat.backendUrl + '/ajax/send_message',
    data: {text: input.value},
    success: function(ev) {
      input.value = '';
      input.disabled = false;
      chat.getMessages();
    },
    error: function(ev) {
      chat.showError('Error sending message');
      input.disabled = false;
    },
  });
};

$(document).ready(function() {
  $('.menu-item').click(chat.handleMenuItemClick);
  $('.message-input').keypress(chat.handleMessageInput);
  chat.getMessages();
});

// vim:ts=2:sw=2:et
