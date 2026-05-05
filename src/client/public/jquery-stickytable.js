(function (factory) {
	typeof define === 'function' && define.amd ? define(factory) :
	factory();
}(function () { 'use strict';

	/**
	 * jQuery throttle / debounce - v1.1 - 3/7/2010
	 * http://benalman.com/projects/jquery-throttle-debounce-plugin/
	 *
	 * Copyright (c) 2010 "Cowboy" Ben Alman
	 * Dual licensed under the MIT and GPL licenses.
	 * http://benalman.com/about/license/
	 * @preserve
	 */
	(function (b, c) { var $ = b.jQuery || b.Cowboy || (b.Cowboy = {}), a; $.throttle = a = function (e, f, j, i) { var h, d = 0; if (typeof f !== "boolean") { i = j; j = f; f = c; } function g() { var o = this, m = +new Date() - d, n = arguments; function l() { d = +new Date(); j.apply(o, n); } function k() { h = c; } if (i && !h) { l(); } h && clearTimeout(h); if (i === c && m > e) { l(); } else { if (f !== true) { h = setTimeout(i ? k : l, i === c ? e - m : e); } } } if ($.guid) { g.guid = j.guid = j.guid || $.guid++; } return g }; $.debounce = function (d, e, f) { return f === c ? a(d, e, false) : a(d, f, e !== false) }; })(window);

	/**
	 * jQuery Stick Table Headers - v3.0.0
	 * https://github.com/jpchip/stickytable
	 *
	 * Wrapped version of sticky table headers code here:
	 * http://tympanus.net/codrops/2014/01/09/sticky-table-headers-columns/
	 * http://tympanus.net/Tutorials/StickyTableHeaders/
	 *
	 * CSS located in jquery-stickytable.css
	 *
	 * Depends:
	 * jQuery
	 * jQuery throttle / debounce - v1.1 - 3/7/2010
	 * http://benalman.com/projects/jquery-throttle-debounce-plugin/
	 * @preserve
	 */

	(function ($, window, document, undefined$1) {

		var pluginName = 'stickyTable',
		defaults = {
			copyTableClass: true, //copies table classes to sticky tables
			copyEvents: false, //copy events on headers and cols to sticky tables
			overflowy: false //if true limits height of table to height of parent element
		};

		function StickyTable(element, options) {
			this.element = element;
			this._defaults = defaults;
			this._name = pluginName;

			this.options = $.extend({}, defaults, options);

			this._init();
		}

		StickyTable.prototype._init = function () {
			var $w = $(window),
				$t = $(this.element),
				classList = [],
				$thead,
				$col,
				$stickyHead,
				$stickyCol,
				$stickyInsct,
				$stickyWrap,
				setWidths,
				repositionStickyHead,
				repositionStickyCol,
				calcAllowance;

			if (!$(this.element).is("table")) {
				$.error('Target element must be a table');
			}

			//don't run this function again on same element!
			if ($(this.element).hasClass("sticky-enabled")) {
				return;
			}

			if ($t.find('thead').length > 0 && $t.find('th').length > 0) {
				// Clone <thead>
				$thead = $t.find('thead').clone(this.options.copyEvents);
				$col = $t.find('thead, tbody').clone(this.options.copyEvents);

				// Add class, remove margins, reset width and wrap table
				$t.css({
					margin: 0,
					width: '100%'
				}).wrap('<div class="sticky-wrap" />');

				if (this.options.overflowy) {
					$t.parent().addClass('overflow-y');
				}

				if ($t.attr('class')) {
					classList = $t.attr('class').split(/\s+/);
				}

				$t.addClass('sticky-enabled');

				// Create new sticky table head (basic)
				$t.after('<table class="sticky-thead" />');

				// If <tbody> contains <th>, then we create sticky column and intersect (advanced)
				if ($t.find('tbody th').length > 0) {
					$t.after('<table class="sticky-col" /><table class="sticky-intersect" />');
				}

				// Create shorthand for things
				$stickyHead = $t.siblings('.sticky-thead');
				$stickyCol = $t.siblings('.sticky-col');
				$stickyInsct = $t.siblings('.sticky-intersect');
				$stickyWrap = $t.parent('.sticky-wrap');

				if (this.options.copyTableClass) {
					$.each(classList, function (index, item) {
						$stickyHead.addClass(item);
						$stickyCol.addClass(item);
						$stickyInsct.addClass(item);
					});
				}

				$stickyHead.append($thead);

				$stickyCol.append($col)
					.find('thead th:gt(0)').remove()
					.end()
					.find('tbody td').remove();

				$stickyInsct.html('<thead><tr><th>' + $t.find('thead th:first-child').html() + '</th></tr></thead>');
				$stickyInsct.find('th').height($t.find('thead th:first-child').height());

				setWidths = function () {
					$t.find('thead th').each(function (i) {
						$stickyHead.find('th').eq(i).width($(this).width());
					})
						.end()
						.find('tr').each(function (i) {
							$stickyCol.find('tr').eq(i).height($(this).height());
						});

					// Set width of sticky table head
					$stickyHead.width($t.width());

					// Set width of sticky table col
					$stickyCol.find('th').add($stickyInsct.find('th')).width($t.find('thead th').width());
				};

				repositionStickyHead = function () {
					// Return value of calculated allowance
					var allowance = calcAllowance();

					// Check if wrapper parent is overflowing along the y-axis
					if ($t.height() > $stickyWrap.height()) {
						// If it is overflowing (advanced layout)
						// Position sticky header based on wrapper scrollTop()
						if ($stickyWrap.scrollTop() > 0) {
							// When top of wrapping parent is out of view
							$stickyHead.add($stickyInsct).css({
								opacity: 1,
								top: $stickyWrap.scrollTop()
							});
						} else {
							// When top of wrapping parent is in view
							$stickyHead.add($stickyInsct).css({
								opacity: 0,
								top: 0
							});
						}
					} else {
						// If it is not overflowing (basic layout)
						// Position sticky header based on viewport scrollTop
						if ($w.scrollTop() > $t.offset().top && $w.scrollTop() < $t.offset().top + $t.outerHeight() - allowance) {
							// When top of viewport is in the table itself
							$stickyHead.add($stickyInsct).css({
								opacity: 1,
								top: $w.scrollTop() - $t.offset().top
							});
						} else {
							// When top of viewport is above or below table
							$stickyHead.add($stickyInsct).css({
								opacity: 0,
								top: 0
							});
						}
					}
				};

				repositionStickyCol = function () {
					if ($stickyWrap.scrollLeft() > 0) {
						// When left of wrapping parent is out of view
						$stickyCol.add($stickyInsct).css({
							opacity: 1,
							left: $stickyWrap.scrollLeft()
						});
					} else {
						// When left of wrapping parent is in view
						$stickyCol.css({
							opacity: 0
						})
							.add($stickyInsct).css({
								left: 0
							});
					}
				};

				calcAllowance = function () {
					var a = 0;
					// Calculate allowance
					$t.find('tbody tr:lt(3)').each(function () {
						a += $t.height();
					});

					// Set fail safe limit (last three row might be too tall)
					// Set arbitrary limit at 0.25 of viewport height, or you can use an arbitrary pixel value
					if (a > $w.height() * 0.25) {
						a = $w.height() * 0.25;
					}

					// Add the height of sticky header
					a += $stickyHead.height();
					return a;
				};

				setWidths();

				$t.parent('.sticky-wrap').scroll(function () {
					repositionStickyHead();
					repositionStickyCol();
				});

				$w.on('load', setWidths)
					.resize($.debounce(500, function () {
						setWidths();
						repositionStickyHead();
						repositionStickyCol();
					}))
					.scroll(repositionStickyHead);
			}
		};

		StickyTable.prototype.destroy = function () {
			var $t = $(this.element),
				$stickyHead = $t.siblings('.sticky-thead'),
				$stickyCol = $t.siblings('.sticky-col'),
				$stickyInsct = $t.siblings('.sticky-intersect');

			this.options = $.extend({}, defaults);

			if ($t.hasClass('sticky-enabled')) {
				$stickyHead.remove();
				$stickyCol.remove();
				$stickyInsct.remove();

				$t.unwrap();
				$t.removeClass('sticky-enabled');
				//todo: clear events...
			}
			$.removeData(this.element, 'plugin_' + this._name);
		};

		$.fn[pluginName] = function (options) {
			return this.each(function () {
				var instance = $.data(this, 'plugin_' + pluginName);
				if (instance) {
					if (typeof options === 'string' || options instanceof String) {
						if (instance[options] !== undefined$1 && $.isFunction(instance[options])) {
							if (options.indexOf('_') !== -1) {
								$.error('Method ' + options + ' is private!');
							} else {
								instance[options](Array.prototype.slice.call(arguments, 1));
							}
						} else {
							$.error('Method ' + options + ' does not exist.');
						}
					}
				} else if (typeof options === 'object' || !options) {
					$.data(this, 'plugin_' + pluginName, new StickyTable(this, options));
				} else {
					$.error('Plugin must be initialised before using method: ' + options);
				}
			});
		};

	}(jQuery, window, document));

}));
