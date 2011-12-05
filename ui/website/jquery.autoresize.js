/*
 * jQuery.fn.autoResize 1.14
 * --
 * https://github.com/jamespadolsey/jQuery.fn.autoResize
 * --
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. */ 

(function($){

	var uid = 'ar' + +new Date,

		defaults = autoResize.defaults = {
			onResize: function(){},
			onBeforeResize: function(){return 123},
			onAfterResize: function(){return 555},
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
		'paddingTop', 'paddingLeft', 'paddingBottom', 'paddingRight', 'width'
	];

	autoResize.cloneCSSValues = {
		position: 'absolute',
		top: -9999,
		left: -9999,
		opacity: 0,
		overflow: 'hidden'
	};

	autoResize.resizableFilterSelector = [
		'textarea:not(textarea.' + uid + ')',
		'input:not(input[type])',
		'input[type=text]',
		'input[type=password]',
		'input[type=email]',
		'input[type=url]'
	].join(',');

	autoResize.AutoResizer = AutoResizer;

	$.fn.autoResize = autoResize;

	function autoResize(config) {
		this.filter(autoResize.resizableFilterSelector).each(function(){
			new AutoResizer( $(this), config );
		});
		return this;
	}

	function AutoResizer(el, config) {

		if (el.data('AutoResizer')) {
			el.data('AutoResizer').destroy();
		}
		
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

		// Make sure onAfterResize is called upon animation completion
		config.animate.complete = (function(f){
			return function() {
				config.onAfterResize.call(el);
				return f.apply(this, arguments);
			};
		}(config.animate.complete));

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
				.bind('change.autoResize', check)
				.bind('paste.autoResize', function() {
					setTimeout(function() { check(); }, 0);
				});
			
			if (!this.el.is(':hidden')) {
				this.check(null, true);
			}

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
				.addClass(uid)
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

			if (!this.clone) {
		this.createClone();
		this.injectClone();
			}

			var config = this.config,
				clone = this.clone,
				el = this.el,
				value = el.val();

			// Do nothing if value hasn't changed
			if (value === this.prevValue) { return true; }
			this.prevValue = value;

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

					config.onBeforeResize.call(el);
					config.onResize.call(el);

					el.scrollLeft(0);

					if (config.animate && !immediate) {
						el.stop(1,1).animate({
							width: newWidth
						}, config.animate);
					} else {
						el.width(newWidth);
						config.onAfterResize.call(el);
					}

				}

				return;

			}

			// TEXTAREA
			
			clone.width(el.width()).height(0).val(value).scrollTop(10000);
			
			var scrollTop = clone[0].scrollTop;
				
			// Don't do anything if scrollTop hasen't changed:
			if (this.previousScrollTop === scrollTop) {
				return;
			}

			this.previousScrollTop = scrollTop;
			
			if (scrollTop + config.extraSpace >= config.maxHeight) {
				el.css('overflowY', '');
				scrollTop = config.maxHeight;
				immediate = true;
			} else if (scrollTop <= config.minHeight) {
				scrollTop = config.minHeight;
			} else {
				el.css('overflowY', 'hidden');
				scrollTop += config.extraSpace;
			}

			config.onBeforeResize.call(el);
			config.onResize.call(el);

			// Either animate or directly apply height:
			if (config.animate && !immediate) {
				el.stop(1,1).animate({
					height: scrollTop
				}, config.animate);
			} else {
				el.height(scrollTop);
				config.onAfterResize.call(el);
			}

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
