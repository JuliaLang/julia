/*
 * jQuery.fn.autoResize 1.12
 * --
 * https://github.com/jamespadolsey/jQuery.fn.autoResize
 * --
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. */ 

(function($){

	var defaults = autoResize.defaults = {
		onResize: function(){},
		animate: {
			duration: 200,
			complete: function(){}
		},
		extraSpace: 50,
		minHeight: 'original',
		maxHeight: 500,
		minWidth: 'original',
		maxWidth: 500
	};

	autoResize.cloneCSSProperties = [
		'lineHeight', 'textDecoration', 'letterSpacing',
		'fontSize', 'fontFamily', 'fontStyle', 'fontWeight',
		'textTransform', 'textAlign', 'direction', 'wordSpacing', 'fontSizeAdjust',
		'padding', 'width'
	];

	autoResize.cloneCSSValues = {
		position: 'absolute',
		top: -9999,
		left: -9999,
		opacity: 0,
		overflow: 'hidden'
	};

	autoResize.resizableFilterSelector = 'textarea,input:not(input[type]),input[type=text],input[type=password]';

	autoResize.AutoResizer = AutoResizer;

	$.fn.autoResize = autoResize;

	function autoResize(config) {
		this.filter(autoResize.resizableFilterSelector).each(function(){
			new AutoResizer( $(this), config );
		});
		return this;
	}

	function AutoResizer(el, config) {
		
		config = this.config = $.extend({}, autoResize.defaults, config);
		this.el = el;

		this.nodeName = el[0].nodeName.toLowerCase();

		this.originalHeight = el.height();
		this.previousScrollTop = null;

		this.value = el.val();

		if (config.maxWidth === 'original') config.maxWidth = el.width();
		if (config.minWidth === 'original') config.minWidth = el.width();
		if (config.maxHeight === 'original') config.maxHeight = el.height();
		if (config.minHeight === 'original') config.minHeight = el.height();

		if (this.nodeName === 'textarea') {
			el.css({
				resize: 'none',
				overflowY: 'hidden'
			});
		}

		el.data('AutoResizer', this);

		this.createClone();
		this.injectClone();
		this.bind();

	}

	AutoResizer.prototype = {

		bind: function() {

			var check = $.proxy(function(){
				this.check();
				return true;
			}, this);

			this.unbind();

			this.el
				.bind('keyup.autoResize', check)
				//.bind('keydown.autoResize', check)
				.bind('change.autoResize', check);
			
			this.check(null, true);

		},

		unbind: function() {
			this.el.unbind('.autoResize');
		},

		createClone: function() {

			var el = this.el,
				clone = this.nodeName === 'textarea' ? el.clone() : $('<span/>');

			this.clone = clone;

			$.each(autoResize.cloneCSSProperties, function(i, p){
				clone[0].style[p] = el.css(p);
			});

			clone
				.removeAttr('name')
				.removeAttr('id')
				.attr('tabIndex', -1)
				.css(autoResize.cloneCSSValues);

			if (this.nodeName === 'textarea') {
				clone.height('auto');
			} else {
				clone.width('auto').css({
					whiteSpace: 'nowrap'
				});
			}

		},

		check: function(e, immediate) {

			var config = this.config,
				clone = this.clone,
				el = this.el,
				value = el.val();

			if (this.nodeName === 'input') {

				clone.text(value);

				// Calculate new width + whether to change
				var cloneWidth = clone.width(),
					newWidth = (cloneWidth + config.extraSpace) >= config.minWidth ?
						cloneWidth + config.extraSpace : config.minWidth,
					currentWidth = el.width();

				newWidth = Math.min(newWidth, config.maxWidth);

				if (
					(newWidth < currentWidth && newWidth >= config.minWidth) ||
					(newWidth >= config.minWidth && newWidth <= config.maxWidth)
				) {

					config.onResize.call(el);

					el.scrollLeft(0);

					config.animate && !immediate ?
						el.stop(1,1).animate({
							width: newWidth
						}, config.animate)
					: el.width(newWidth);

				}

				return;

			}

			// TEXTAREA
			
			clone.height(0).val(value).scrollTop(10000);
			
			var scrollTop = clone[0].scrollTop + config.extraSpace;
				
			// Don't do anything if scrollTop hasen't changed:
			if (this.previousScrollTop === scrollTop) {
				return;
			}

			this.previousScrollTop = scrollTop;
			
			if (scrollTop >= config.maxHeight) {
				el.css('overflowY', '');
				scrollTop = config.maxHeight;
			} else {
				el.css('overflowY', 'hidden');
			}

			if (scrollTop < config.minHeight) {
				scrollTop = config.minHeight;
			}

			config.onResize.call(el);
			
			// Either animate or directly apply height:
			config.animate && !immediate ?
				el.stop(1,1).animate({
					height: scrollTop
				}, config.animate)
				: el.height(scrollTop);
			
		},

		destroy: function() {
			this.unbind();
			this.el.removeData('AutoResizer');
			this.clone.remove();
			delete this.el;
			delete this.clone;
		},

		injectClone: function() {
			(
				autoResize.cloneContainer ||
				(autoResize.cloneContainer = $('<arclones/>').appendTo('body'))
			).append(this.clone);
		}

	};
	
})(jQuery);