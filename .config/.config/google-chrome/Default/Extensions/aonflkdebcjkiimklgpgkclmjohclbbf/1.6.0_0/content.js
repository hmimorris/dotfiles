/*! shft.cl - 1.5.x
* https://shft.cl/
* https://deuxhuithuit.com/
* Copyright (c) 2015-2019 Deux Huit Huit (https://deuxhuithuit.com/);
* Licensed Private. All rights reserved.
* DO NOT COPY, NOR EDIT, NOR DISTRIBUTE, NOR GIVE, NOR SALE THIS FILE.
* Use at your own risk.
* Please enjoy responsibly.
*/
(function () {
	'use strict';

	const manifest = chrome.runtime.getManifest();
	const isDev = !('update_url' in manifest);
	const debug = isDev;
	const alwaysHttps = true;
	let server = !isDev ? 'https://shft.cl/' : ((alwaysHttps ? 'https:' : window.location.protocol) + '//localhost:3000/');
	const instanceId = ~~(Math.random() * 10000000);
	const storage = chrome.storage.local; // sync
	let user;

	/**
	 * Creates the loading UI
	 */
	const loading = (function (d) {
		const e = d.createElement('div');
		e.innerText = 'Shift Click found the image! Waiting for it...';
		if (!e.style) {
			return e;
		}
		e.style.display = 'block';
		e.style.backgroundColor = 'rgba(255, 255, 255, 0.6)';
		e.style.color = '#333333';
		e.style.fontFamily = 'Arial, Helvetica, sans-serif';
		e.style.position = 'fixed';
		e.style.top = e.style.right = e.style.left = 0;
		e.style.height = e.style.lineHeight = '40px';
		e.style.fontSize = '20px';
		e.style.verticalAlign = 'middle';
		e.style.textAlign = 'center';
		e.style.bottom = 'auto';
		e.style.cursor = 'wait';
		e.style.pointerEvents = 'none';
		e.style.margin = '0';
		e.style.padding = '0';
		e.style.textDecoration = 'none';
		e.style.border = 'none';
		e.style.opacity = 1;
		e.style.zIndex = (Math.pow(2, 31) - 1)|0;
		return e;
	})(document);

	/**
	 * Creates the canvas UI
	 */
	const canvas = (function (d) {
		const e = d.createElement('canvas');
		const init = function () {
			if (!e.style) {
				return;
			}
			e.style.display = 'block';
			e.style.backgroundColor = 'transparent';
			e.style.position = 'fixed';
			e.style.top = e.style.right = e.style.bottom = e.style.left = 0;
			e.style.border = 'none';
			e.style.cursor = 'crosshair';
			e.style.opacity = 1;
			e.style.zIndex = (Math.pow(2, 31) - 2)|0;
		};
		const ctx = e.getContext && e.getContext('2d');
		const pos = {
			x1: 0,
			x2: 0,
			y1: 0,
			y2: 0
		};
		const last = {
			x: 0,
			y: 0
		};
		init();
		return {
			elem: e,
			ctx: ctx,
			x: 0,
			y: 0,
			visible: false,
			mousedown: false,
			selection: false,
			move: false,
			supported: !!ctx,
			pos: function () {
				return extend(pos);
			},
			clear: function () {
				ctx.clearRect(0, 0, canvas.width, canvas.height);
			},
			update: function (x, y) {
				last.x = x;
				last.y = y;
				pos.x1 = Math.min(canvas.x, x);
				pos.x2 = Math.max(canvas.x, x);
				pos.y1 = Math.min(canvas.y, y);
				pos.y2 = Math.max(canvas.y, y);
			},
			translate: function (x, y) {
				const dx = x - last.x;
				const dy = y - last.y;
				last.x = x;
				last.y = y;
				if (pos.x1 + dx < 0) {
					return;
				} else if (pos.x2 + dx > canvas.width) {
					return;
				}
				if (pos.y1 + dy < 0) {
					return;
				} else if (pos.y2 + dy > canvas.height) {
					return;
				}
				pos.x1 += dx;
				pos.x2 += dx;
				pos.y1 += dy;
				pos.y2 += dy;
				canvas.x += dx;
				canvas.y += dy;
			},
			rect: function (options) {
				const textPad = 2;
				const minPix = 25;
				const fontHeight = 14;
				const x = pos.x2 - pos.x1;
				const y = pos.y2 - pos.y1;
				ctx.save();
				ctx.fillStyle = 'rgba(0, 255, 0, ' + options.opacity + ')';
				ctx.fillRect(pos.x1, pos.y1, x, y);
				ctx.strokeStyle = '#000000';
				ctx.strokeWidth = 2;
				ctx.strokeRect(pos.x1, pos.y1, x, y);
				ctx.font = fontHeight + 'px sans-serif';
				ctx.fillStyle = '#181818';
				ctx.textAlign = 'center';
				ctx.textBaseline = 'bottom';
				ctx.shadowColor = '#cccccc';
				ctx.shadowBlur = 10;
				if (x > minPix) {
					const xpx = x + ' px';
					ctx.fillText(
						xpx,
						pos.x1 + (x / 2),
						pos.y1 - textPad
					);
				}
				if (y > minPix) {
					const ypx = y + ' px';
					ctx.save();
					ctx.translate(pos.x1, pos.y1);
					ctx.rotate(-Math.PI / 2);
					ctx.fillText(
						ypx,
						-textPad - (y / 2),
						-textPad
					);
					ctx.restore();
				}
				ctx.restore();
			}
		};
	})(document);

	/**
	 * Global Utils
	 */
	const now = function () {
		return window.performance.now();
	};
	const z = function (z) {
		return parseInt(z, 10) || 0;
	};

	/**
	 * Style cache
	 */
	let styleCache;
	const initStyleCache = function () {
		if (!!styleCache && !!styleCache.clear) {
			styleCache.clear();
			styleCache = undefined;
		}
		styleCache = new WeakMap();
	};

	const getComputedStyleFor = function (elem) {
		elem = elem.elem || elem;
		if (styleCache.has(elem)) {
			return styleCache.get(elem);
		}
		const s = window.getComputedStyle(elem);
		styleCache.set(elem, s);
		return s;
	};

	const getBoundsFor = function (elem) {
		const bounds = elem.bounds || elem.getBoundingClientRect();
		return {
			top: bounds.top + window.scrollY,
			right: bounds.right + window.scrollX,
			bottom: bounds.bottom + window.scrollY,
			left: bounds.left + window.scrollX,
			width: bounds.width,
			height: bounds.height
		};
	};

	const log = function (msg) {
		if (debug) {
			console.log('[shft.cl]', '(' + instanceId.toString().padStart(7,0) + ')', msg);
		}
	};

	const extend = function(obj) {
		const c = {};
		Object.keys(obj).forEach(function (key) {
			c[key] = obj[key];
		});
		return c;
	};

	const urlIsValid = function (url) {
		return !!url && (
			/^https?:/i.test(url) || // http/https
			/^data:/i.test(url) || // data-uri
			/^s?ftpe?s?:/i.test(url) || // (s)ftp(e)(s)
			!/^[\/]{2}:.*$/i.test(url) // protocol less urls
		);
	};

	const pathIsRelative = function (url) {
		// does NOT starts with /
		return url !== undefined && !urlIsValid(url) && !/^[\/].*$/i.test(url);
	};

	const validateUrl = function (url) {
		const valid = urlIsValid(url);
		if (!valid) {
			const schema = url.split(':')[0];
			const issueUrl = server + 'issue';
			if (confirm('Image url schema (' + schema + ') is not valid.\n' +
				'Do you want to report it? \n')) {
				window.location = issueUrl;
			}
		}
		return valid;
	};
	const computeUrlFromPath = function (path) {
		let baseUrl;
		if (/^https?/i.test(path)) {
			return path;
		}
		else if ( !/^[\/]/.test(path)) {
			baseUrl = window.location.href;
			return baseUrl + '/' + path;
		} else {
			baseUrl = window.location.origin;
			return baseUrl + '/' + path;
		}
		return path;
	};
	const whitespaces = /[\s\n\r\t]{2,}/i;
	const splitSrcSet = function (srcset) {
		while (whitespaces.test(srcset)) {
			srcset = srcset.replace(whitespaces, ' ');
		}
		srcset = srcset.replace(/[\s\n\r\t]+$/, '');
		srcset = srcset.replace(/^[\s\n\r\t]+/, '');
		srcset = srcset.split(/,[\s\n\r\t]+/);

		return srcset;
	};
	const extractUrlFromOneSrcSet = function (srcset) {
		var paths = splitSrcSet(srcset);
		var l = paths.length - 1 || 0;

		const extractUrlAndSize = function (s) {
			s = s.split(' ');
			var size = s[1];
			var path = s[0];
			size = size.slice(0, -1);
			var url = computeUrlFromPath(path);
			return ({
				size: size,
				url: url
			});
		};

		if (!l) {
			if (/\s/g.test(paths)) {
				paths = paths[0].split(' ');
			}
			return computeUrlFromPath(paths[0]);
		}

		const urlsAndSizes = map(paths, extractUrlAndSize);
		while (l > 0) {
			if (!urlIsValid(urlsAndSizes[l].url)) {
				urlsAndSizes.splice(l, 1);
			}
			l--;
		}
		if (urlsAndSizes.length <= 0) {
			return;
		}
		return urlsAndSizes;
	};
	const findBestSrcFromUrlAndSizes = function (urlsAndSizes) {
		const sizes = map(urlsAndSizes, function (urlAndSize) {
			return urlAndSize.size;
		});
		const i = indexOfMax(sizes);
		return urlsAndSizes[i] && urlsAndSizes[i].url;
	};
	const chooseBestUrlFromDifferentSources = function (urls) {
		var minWidthSrcSet = [], maxWidthSrcSet = [];

		var extractNumberResolution = function (media, strat) {
			return media.match(new RegExp("^.*"+strat+".*?([0-9]+).*"))[1];
		};

		if (!urls || !urls.length) {
			return;
		}

		each(urls, function (url) {
			var media = url[1];
			if (/min/i.test(media)) {
				minWidthSrcSet.push([url[0], extractNumberResolution(media, 'min')]);
			} else if (/max/i.test(media)) {
				maxWidthSrcSet.push([url[0], extractNumberResolution(media, 'max')]);
			} else {
				// TODO also parse sizes...
			}
		});

		if (minWidthSrcSet.length > 0) {
			var sizes = map(minWidthSrcSet, function (srcset) {
				return srcset[1];
			});
			var i = indexOfMax(sizes);
			return minWidthSrcSet[i][0];
		} else if (maxWidthSrcSet.length > 0) {
			var sizes = map(maxWidthSrcSet, function (srcset) {
				return srcset[1];
			});
			var i = indexOfMin(sizes);
			return maxWidthSrcSet[i][0];
		}
		return urls[0][0];
	};
	const extractAllSourcesAndSizes = function (sources) {
		var urls = [];
		each(sources, function (source) {
			if (!!source.srcset) {
				urls.push([extractUrlFromOneSrcSet(source.srcset), source.media || source.sizes || source.width || 1]);
			} else if (!!source.src) {
				urls.push([source.src, source.media || source.sizes || source.width || 1]);
			}
		});
		return urls;
	};
	const identity = function (i) {
		return i;
	};
	const each = function (a, cb) {
		return Array.prototype.forEach.call(a, cb);
	};
	const map = function (a, cb) {
		var stack = [];
		each(a, function (i, j) {
			stack.push(cb(i, j));
		});
		return stack;
	};
	const indexOfMax = function(arr) {
		if (arr.length === 0) {
			return -1;
		}
		var max = arr[0];
		var maxIndex = 0;
		for (var i = 1; i < arr.length; i++) {
			if (z(arr[i]) > z(max)) {
				maxIndex = i;
				max = arr[i];
			}
		}
		return maxIndex;
	};
	const indexOfMin = function(arr) {
		if (arr.length === 0) {
			return -1;
		}
		var min = arr[0];
		var minIndex = 0;
		for (var i = 1; i < arr.length; i++) {
			if (z(arr[i]) < z(min)) {
				minIndex = i;
				min = arr[i];
			}
		}
		return minIndex;
	};
	const arrayize = function (a) {
		return map(a, identity);
	};
	const all = function (target, sel, cb) {
		if (!target) {
			target = document;
		}
		var stack = target.querySelectorAll(sel);
		if (!cb) {
			return stack;
		}
		return each(stack, cb);
	};
	const $ = function (sel, cb) {
		return all(document, sel, cb);
	};
	$.remove = function (elem) {
		return elem.parentNode.removeChild(elem);
	};

	const ping = function () {
		storage.get('ping', function (items) {
			var lastPing = items.ping || 0;
			if (isDev || lastPing < Date.now() - (24 * 60 * 60 * 1000)) {
				if (isDev) {
					log('ping in dev aborted!');
					return;
				}
				chrome.runtime.sendMessage({
					target: 'background',
					command: 'fetch',
					response: 'ping',
					instanceId: instanceId,
					params: {
						payload: {
							user: user
						},
						url: server + 'ping',
						keepalive: true
					}
				});
			}
		});
	};

	const posIsInElement = function (pos, elem) {
		var elemPos = getBoundsFor(elem);
		return elemPos.left <= pos.x && elemPos.right >= pos.x &&
			elemPos.top <= pos.y && elemPos.bottom >= pos.y;
	};

	const isVisible = function (elem) {
		var visible = true;
		var currentElement = elem;
		while (!!currentElement && currentElement !== document && !!visible) {
			var style = getComputedStyleFor(currentElement);
			if (style.display === 'none' || parseFloat(style.opacity) < 0.01 ||
				style.visibility === 'hidden' || style.visibility === 'collapse') {
				visible = false;
			}
			currentElement = currentElement.parentElement;
		}
		return visible;
	};

	let pendingImagesCount = 0;
	let virtualTabId = 0;
	const tabs = {};
	const pendingCloseTabs = {};
	const pendingRedirectionTabs = {};
	const formatSrcForConsole = function (src) {
		if (/^data:/.test(src)) {
			return src.substring(0, 64);
		}
		return src;
	};
	const imageFound = function (img, infos) {
		let src = img.src;
		let urls = img.urls;
		// We do have a source set but do not have "custom" list of urls
		// so we can extract them from the source set
		if (!!img.srcset && !urls) {
			/* TODO: fix this, it need unit tests ! */
			urls = extractUrlFromOneSrcSet(img.srcset);
			if (!!urls && !!urls.length) {
				src = findBestSrcFromUrlAndSizes(urls) || src;
			}
		}
		if (pathIsRelative(src)) {
			src = window.location.href + '/' + src;
		}
		if (!validateUrl(src)) {
			log('Url + ' + src + ' is not valid. Send aborted');
			return false;
		}
		if (window.navigator.onLine === false) {
			alert('You do not seem to be connected to internet...\nShift Click needs internet access');
			return false;
		}
		log('------------------------------');
		log('requesting ' + formatSrcForConsole(src));
		pendingImagesCount++;
		log('pendingImagesCount: ' + pendingImagesCount);
		const vtabid = (virtualTabId++);
		const bounds = getBoundsFor(img);
		const screen = extend(window.screen);
		screen.availHeight = window.screen.availHeight;
		screen.availLeft = window.screen.availLeft;
		screen.availTop = window.screen.availTop;
		screen.availWidth = window.screen.availWidth;
		screen.colorDepth = window.screen.colorDepth;
		screen.height = window.screen.height;
		screen.orientation = {
			angle: !window.screen || !window.screen.orientation ? 0 : window.screen.orientation.angle,
			type: !window.screen || !window.screen.orientation ? 'unknown' : window.screen.orientation.type
		};
		screen.pixelDepth = window.screen.pixelDepth;
		screen.width = window.screen.width;
		screen.position = {
			x: window.screenX,
			y: window.screenY
		};
		const plugins = [];
		each(window.navigator.plugins, function (p) {
			plugins.push({
				name: p.name,
				description: p.description,
				filename: p.filename
			});
		});
		const params = {
			url: src,
			urls: urls,
			srcset: img.srcset,
			sizes: img.sizes,
			referer: window.location,
			referrer: document.referrer,
			alt: img.alt || img.title,
			navigator: {
				userAgent: window.navigator.userAgent,
				platform: window.navigator.platform,
				language: window.navigator.language,
				languages: window.navigator.languages,
				plugins: plugins,
				platform: window.navigator.platform,
				cookieEnabled: window.navigator.cookieEnabled,
				vendor: window.navigator.vendor,
				doNotTrack: window.navigator.doNotTrack
			},
			scroll: {
				left: window.scrollX,
				top: window.scrollY
			},
			offset: {
				x: bounds.left,
				y: bounds.top,
				width: bounds.width,
				height: bounds.height
			},
			ext: {
				chrome: manifest.version
			},
			click: infos.clickPos,
			title: document.title,
			screen: screen,
			strategy: infos.strategy,
			timer: infos.timer
		};
		isDev && pendingImagesCount === 1 && document.body.appendChild(loading);
		
		log(params);
		log(infos.strategy);
		log(infos.timer.delta + ' ms');
		log('pendingImagesCount: ' + pendingImagesCount);
		log('------------------------------');

		log('------------------------------');
		log('opening new tab');
		chrome.runtime.sendMessage({
			target: 'background',
			command: 'open',
			url: server + 'loading',
			active: !infos.clickPos.meta && !infos.clickPos.ctrl,
			tabid: vtabid,
			instanceId: instanceId
		});
		log('new loading tab open request send');
		log('------------------------------');

		log('------------------------------');
		log('sending new request');
		chrome.runtime.sendMessage({
			target: 'background',
			command: 'fetch',
			response: 'newImage',
			active: !infos.clickPos.meta && !infos.clickPos.ctrl,
			tabid: vtabid,
			instanceId: instanceId,
			params: {
				url: server + 'new',
				payload: params
			}
		});
		log('new request sent');
		log('------------------------------');

		return true;
	};
	const hasPendingImages = function () {
		return pendingImagesCount > 0;
	};

	const hitTest = function (pos, collection) {
		var candidates = [];
		each(collection, function (elem) {
			if (posIsInElement(pos, elem)) {
				candidates.push(elem);
			}
		});
		return candidates;
	};

	const removeInvisibles = function (collection) {
		var candidates = [];
		each(collection, function (elem) {
			if (isVisible(elem)) {
				candidates.push(elem);
			}
		});
		return candidates;
	};

	const validateIsElement = function (elem, localName) {
		return elem !== window && elem !== document && elem.localName === localName;
	};

	const validateIsImg = function (elem) {
		return validateIsElement(elem, 'img') &&
			elem.width > 9 && elem.height > 9 &&
			elem.naturalWidth > 9 && elem.naturalHeight > 9;
	};

	const validateIsPicture = function (elem) {
		if (validateIsElement(elem, 'picture')) {
			let sources = all(elem, 'source');
			if (!!sources && !!sources.length) {
				sources = arrayize(sources);
				sources = sources.concat(arrayize(all(elem, 'img')));
				let urlsAndSizes;
				let url;
				if (sources.length > 1) {
					urlsAndSizes = extractAllSourcesAndSizes(sources);
					url = chooseBestUrlFromDifferentSources(urlsAndSizes);
				} else {
					urlsAndSizes = sources.srcset;
					url = sources.srcset || sources.src;
				}

				const title = map(sources, function (source) {
					return source.alt || source.title;
				}).filter(identity).join(' - ');

				return {
					src: url,
					urls: urlsAndSizes,
					alt: elem.title || title,
					bounds: elem.getBoundingClientRect(),
					elem: elem
				};
			}
		}
		return false;
	};

	const validateIsCanvas = function (elem) {
		if (validateIsElement(elem, 'canvas')) {
			var exportQuality = 1;
			var exportType = 'image/jpeg';
			var emptyC = document.createElement('canvas');
			emptyC.width = elem.width;
			emptyC.height = elem.height;
			var emptyUrl = emptyC.toDataURL(exportType);
			var src = elem.toDataURL(exportType);
			if (!src) {
				log('Failed to export canvas!');
			} else if (src !== emptyUrl) {
				return {
					src: elem.toDataURL(exportType, exportQuality),
					alt: elem.title,
					bounds: elem.getBoundingClientRect(),
					elem: elem
				};
			} else {
				log('Detected an empty canvas!');
			}
		}
		return false;
	};

	const urlRegExp = /(?:.+,\s*)?url\(["']?(.+?)["']?\)(?:\s*,.+)?/im;
	const imgSetRegExp = /^-webkit-image-set\(url\(["']?(.+?)["']?\)/im;
	const extractUrlFromSrc = function (style) {
		let bgImage;
		if (!!style.backgroundImage && style.backgroundImage !== 'none') {
			if (urlRegExp.test(style.backgroundImage)) {
				bgImage = style.backgroundImage.replace(urlRegExp, '$1');
			} else if (imgSetRegExp.test(style.backgroundImage)) {
				bgImage = style.backgroundImage.replace(imgSetRegExp, '$1');
			}
			return !!bgImage ? bgImage : false;
		}
		return false;
	};

	const validateHasBgImage = function (elem) {
		if (elem === window || elem === document) {
			return false;
		}
		const style = getComputedStyleFor(elem);
		const bgImage = extractUrlFromSrc(style);

		if (!!bgImage) {
			return {
				src: bgImage,
				alt: elem.title,
				bounds: elem.getBoundingClientRect(),
				elem: elem
			};
		}
		return false;
	};

	const validatePseudoElHasBgImage = function (elem) {
		if (elem === window || elem === document) {
			return false;
		}
		let bgImage;

		var extractBgImageFromPseudoEl = function (pseudoElement) {
			if (!bgImage) {
				const style = window.getComputedStyle(elem, pseudoElement);
				bgImage = extractUrlFromSrc(style);
			}
		};

		each([':after', ':before'], extractBgImageFromPseudoEl);

		if (!!bgImage) {
			return {
				src: bgImage,
				alt: elem.title,
				bounds: elem.getBoundingClientRect(),
				elem: elem
			};
		}
		return false;
	};

	const findAll = function (path, validator) {
		var all = [];
		each(path, function (elem, index) {
			if (!!elem) {
				var result = validator(elem, index, all);
				if (!!result) {
					all.push(result === true ? elem : result);
				}
			}
		});
		return all;
	};

	const findOne = function (path, validator) {
		return findAll(path, function (elem, index, all) {
			return all.length === 0 && validator(elem, index, all);
		})[0];
	};

	const findChildrenInPath = function (path, all) {
		var candidates = [];
		each(path, function (elem) {
			if (!candidates.length && elem !== document && elem !== window) {
				candidates = all(elem);
			}
		});
		return candidates;
	};

	const findElementsWithBackground = function (selector) {
		return function _findElementsWithBackground(elem) {
			return findAll(all(elem, selector), validateHasBgImage);
		};
	};

	const findPseudoElWithBackground = function (selector) {
		return function _findPseudoElWithBackground(elem) {
			return findAll(all(elem, selector), validatePseudoElHasBgImage);
		};
	};

	const isPositioned = function (style) {
		return style.position !== 'static';
	};

	// https://gist.github.com/benpickles/4059636
	const allParents = function (elem) {
		var parents = [];
		var e = elem.parentElement;
		while (!!e) {
			parents.unshift(e);
			e = e.parentElement;
		}
		return parents;
	};

	const findCommonAncestor = function (a, b) {
		var elemA = a.elem || a;
		var elemB = b.elem || b;

		// same ancestor
		if (elemA.parentElement === elemB.parentElement) {
			return {
				a: a,
				b: b,
				common: elemA.parentElement
			};
		}

		var parentsA = allParents(elemA);
		var parentsB = allParents(elemB);

		if (parentsA[0] !== parentsB[0]) {
			throw new Error('No common ancestor!');
		}

		var aLength = parentsA.length;
		var bLength = parentsB.length;

		// start from html and go down. Stop when the chain is broken.
		for (var i = 0; i < Math.min(aLength, bLength); i++) {
			if (parentsA[i] !== parentsB[i]) {
				return {
					a: parentsA[i],
					b: parentsB[i],
					common: parentsA[i - 1]
				};
			}
		}
		// Both trees are equal (checking shallowest length)
		// so return last elements
		return {
			a: parentsA[i] || parentsA[i - 1],
			b: parentsB[i] || parentsB[i - 1],
			common: parentsA[i - 1]
		};
	};

	const findFromRenderStack = function (candidates) {
		const matrix = [];
		candidates.forEach(function (rowC, rowI) {
			const row = [];
			matrix.push(row);
			candidates.forEach(function (colC, colI) {
				row[colI] = rowI === colI ? NaN : 0;
			});
		});
		printMatrix(matrix);
		candidates.forEach(function (rowC, rowI) {
			candidates.forEach(function (colC, colI) {
				if (colI <= rowI || rowC === colC) {
					return;
				}
				const c = findCommonAncestor(rowC, colC);
				if (!c) {
					return;
				}
				const styleA = getComputedStyleFor(c.a);
				const styleB = getComputedStyleFor(c.b);
				
				var positionedA = isPositioned(styleA);
				var positionedB = isPositioned(styleB);
				var setA = function () {
					matrix[rowI][colI] += 1;
				};
				var setB = function () {
					matrix[colI][rowI] += 1;
				};
				var normalStack = function () {
					var compare = c.a.compareDocumentPosition(c.b);
					if (compare === c.a.DOCUMENT_POSITION_PRECEDING) {
						// B precedes A
						setA();
					}
					else if (compare === c.a.DOCUMENT_POSITION_FOLLOWING) {
						// B follow A
						setB();
					}
					/*else if (compare === c.a.DOCUMENT_POSITION_CONTAINS) {
						// B contains A
						setA();
					}
					else if (compare === c.a.DOCUMENT_POSITION_CONTAINED_BY) {
						// B is contained by A
						setB();
					}*/
					else {
						throw new Error('Nodes must be siblings!');
					}
				};
				// https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/Adding_z-index
				if (positionedA && positionedB) {
					if (z(styleA.zIndex) === z(styleB.zIndex)) {
						// normal stacking
						normalStack();
					}
					else if (z(styleA.zIndex) > z(styleB.zIndex)) {
						// consider A on top
						setA();
					}
					else {
						// consider B on top
						setB();
					}
				}
				else if (!positionedA && positionedB) {
					// consider B on top
					setB();
				}
				else if (positionedA && !positionedB) {
					// consider A on top
					setA();
				}
				else {
					// normal stacking
					normalStack();
				}
			});
		});
		printMatrix(matrix);
		matrix.forEach(function (row, i) {
			var sum = 0;
			row.forEach(function (value) {
				sum += (value || 0);
			});
			matrix[i] = sum;
		});
		var max = 0, img;
		matrix.forEach(function (total, i) {
			if (max <= total) {
				max = total;
				img = candidates[i];
			}
		});
		return img;
	};

	const mustIgnoreEventKeys = function (e) {
		return !e.shiftKey || !!e.altKey;
	};

	const mustIgnoreEventTarget = function (e) {
		if (!!canvas.visible) {
			return true;
		}
		if (!e || !e.target) {
			return true;
		}
		var localName = e.target.localName;
		var contentEditable = e.target.getAttribute('contenteditable');
		return (!localName ||
			localName === 'input' || localName === 'textarea' ||
			localName === 'object' || localName === 'iframe' ||
			localName === 'select' ||
			contentEditable === '' || contentEditable === 'true'
		);
	};

	const copyEvent = function (e) {
		return {
			pageX: e.pageX,
			pageY: e.pageY,
			shiftKey: e.shiftKey,
			metaKey: e.metaKey,
			altKey: e.altKey,
			ctrlKey: e.ctrlKey,
			path: e.path,
			target: e.target
		};
	};

	const click = function (e, bypass) {
		const start = now();
		const pos = {
			x: e.pageX,
			y: e.pageY,
			shift: e.shiftKey,
			meta: e.metaKey,
			ctrl: e.ctrlKey,
			alt: e.altKey
		};
		var img;
		const found = function (strategy, group) {
			if (!img) {
				throw new Error('img cannot be falsy');
			}
			const end = now();
			imageFound(img, {
				strategy: (!!group ? group + '-' : '') + strategy,
				clickPos: pos,
				timer: {
					start: start,
					end: end,
					delta: end - start
				}
			});
			preventDefault(e);
			return true;
		};

		// ignore ?
		if (bypass !== true && mustIgnoreEventKeys(e)) {
			return true;
		}
		if (mustIgnoreEventTarget(e)) {
			return true;
		}

		// create a style cache
		initStyleCache();

		const findIt = function (options) {
			// Easiest routes (most often wrong...)
			// start with event target
			img = findOne([e.target], options.validator);
			if (!!img) {
				return found('eventTarget', options.group);
			}
			// then use mouse point
			img = document.elementFromPoint(pos.x, pos.y);
			img = findOne([img], options.validator);
			if (!!img) {
				return found('elementFromPoint', options.group);
			}

			// try to find a something in the event path
			img = findOne(e.path, options.validator);
			if (!!img) {
				return found('findInPath', options.group);
			}

			// if not found, find images in children of elements in event path
			var candidates = findChildrenInPath(e.path, options.all);
			candidates = removeInvisibles(candidates);
			candidates = hitTest(pos, candidates);
			if (candidates.length === 1) {
				img = candidates[0];
				return found('findChildrenInPath', options.group);
			}
			else if (candidates.length > 0) {
				img = findFromRenderStack(candidates);
				if (!!img) {
					return found('findChildrenInPath+findFromRenderStack', options.group);
				}
			}

			// if still not found in event path
			// we need to check the WHOLE document
			// for possible hit candidates.
			candidates = options.all();
			candidates = removeInvisibles(candidates);
			candidates = hitTest(pos, candidates);
			if (candidates.length === 1) {
				img = candidates[0];
				return found('querySelectorAll', options.group);
			}
			else if (candidates.length > 0) {
				img = findFromRenderStack(candidates);
				if (!!img) {
					return found('querySelectorAll+findFromRenderStack', options.group);
				}
			}
			return false;
		};

		// Try with all <picture />'s
		let options = {
			validator: validateIsPicture,
			group: 'picture',
			all: function (elem) {
				return all(elem, 'picture');
			}
		};
		if (findIt(options)) {
			return false;
		}

		// Try with all <img />'s
		options = {
			validator: validateIsImg,
			group: 'img',
			all: function (elem) {
				return all(elem, 'img');
			}
		};
		if (findIt(options)) {
			return false;
		}

		// try with <canvas />
		options = {
			validator: validateIsCanvas,
			group: 'canvas',
			all: function (elem) {
				return findAll(all(elem, 'canvas'), validateIsCanvas);
			}
		};
		if (findIt(options)) {
			return false;
		}

		// Do the same, but with all elements under html
		// that have a background image
		options = {
			validator: validateHasBgImage,
			group: 'bg',
			all: findElementsWithBackground('html *')
		};
		if (findIt(options)) {
			return false;
		}

		// Do the same, but with all pseudo elements under html
		// that have a background image
		options = {
			validator: validatePseudoElHasBgImage,
			group: 'bg',
			all: findPseudoElWithBackground('html *')
		};
		if (findIt(options)) {
			return false;
		}

		// try html node
		// keep it separate because it does not have any parent
		options = {
			validator: validateHasBgImage,
			group: 'html',
			all: findElementsWithBackground('html')
		};
		if (findIt(options)) {
			return false;
		}

		log('no Image found');
		log(pos);
		return true;
	};

	let lastKeyDownEvent = {};
	const lastKeyDown = function (e) {
		lastKeyDownEvent = copyEvent(e);
	};

	const paste = function (e) {
		var pasteStart = now();
		var lastCopy = copyEvent(lastKeyDownEvent);
		var processImageItem = function (item) {
			var file = item.getAsFile();
			var reader = new FileReader();
			var read = function () {
				var end = now();
				imageFound({
					title: document.title,
					src: reader.result,
					bounds: {
						top: window.scrollY,
						right: window.scrollX,
						bottom: window.innerHeight + window.scrollY,
						left: window.innerWidth + window.scrollX,
						width: window.innerWidth,
						height: window.innerHeight
					}
				}, {
					strategy: 'paste',
					clickPos: {
						x: lastCopy.pageX,
						y: lastCopy.pageY,
						shift: lastCopy.shiftKey,
						meta: lastCopy.metaKey,
						ctrl: lastCopy.ctrlKey,
						alt: lastCopy.altKey
					},
					timer: {
						start: pasteStart,
						end: end,
						delta: end - pasteStart
					}
				});
			};
			reader.addEventListener('load', read, false);
			reader.readAsDataURL(file);
		};
		each(e.clipboardData.items, function (item) {
			if (!!~item.type.indexOf('image')) {
				processImageItem(item);
			}
		});
	};

	const dragenter = function (e) {
		e.target.style.opacity = '0.4';
		document.body.classList.add('is-dragging');
	};
	const dragover = function (e) {
		e.dataTransfer.dropEffect = 'copy';
		return preventDefault(e);
	};
	const dragleave = function (e) {
		e.target.style.opacity = '';
		document.body.classList.remove('is-dragging');
	};
	const drop = function (e) {
		e.target.style.opacity = '';
		var files = e.dataTransfer.files;
		log('Dragged ' + files.length + ' files');

		var dropStart = now();
		var lastCopy = {}; // copyEvent(lastKeyDownEvent);
		var processImageItem = function (file) {
			var reader = new FileReader();
			var read = function () {
				var end = now();
				imageFound({
					title: document.title,
					src: reader.result,
					bounds: {
						top: window.scrollY,
						right: window.scrollX,
						bottom: window.innerHeight + window.scrollY,
						left: window.innerWidth + window.scrollX,
						width: window.innerWidth,
						height: window.innerHeight
					}
				}, {
					strategy: 'paste',
					clickPos: {
						x: lastCopy.pageX,
						y: lastCopy.pageY,
						shift: lastCopy.shiftKey,
						meta: lastCopy.metaKey,
						ctrl: lastCopy.ctrlKey,
						alt: lastCopy.altKey
					},
					timer: {
						start: dropStart,
						end: end,
						delta: end - dropStart
					}
				});
			};
			reader.addEventListener('load', read, false);
			reader.readAsDataURL(file);
		};

		for (var i = 0; i < files.length; i++) {
			var file = files[i];
			var imageType = /^image\//;

			if (!imageType.test(file.type)) {
				continue;
			}

			processImageItem(file);
		}
		return preventDefault(e);
	};

	const printMatrix = function (matrix) {
		log('--------');
		matrix.forEach(function (rowC, rowI) {
			var s = '';
			rowC.forEach(function (colC, colI) {
				s += isNaN(colC) ? '+' : colC;
				s += ' ';
			});
			log(s);
		});
		log('--------');
	};

	const preventDefault = function (e) {
		if (e.preventDefault) {
			e.preventDefault();
			e.stopPropagation();
			e.stopImmediatePropagation();
		}
		return false;
	};

	const noop = function (e) {
		// ignore ?
		if (mustIgnoreEventKeys(e) || mustIgnoreEventTarget(e)) {
			return true;
		}
		if (!hasPendingImages()) {
			return true;
		}
		return preventDefault(e);
	};

	let selectionStart = now();
	const selectionHotKey = 16; // shift
	const selectionAbortKey = 27; // escape
	const selectionMoveKey = 32; // space bar
	const selectionRectOpts = {
		opacity: 0.2
	};
	const drawSelection = (x, y) => {
		canvas.clear();
		if (canvas.move) {
			canvas.translate(x, y);
		} else {
			canvas.update(x, y);
		}
		canvas.rect(selectionRectOpts);
	};
	const initSelection = (e) => {
		log('selection mode on');
		canvas.width = window.innerWidth;
		canvas.height = window.innerHeight;
		canvas.elem.width = canvas.width;
		canvas.elem.height = canvas.height;
		canvas.clear();
		document.body.appendChild(canvas.elem);
		canvas.visible = true;
		canvas.mousedown = false;
		canvas.selection = false;
		canvas.move = false;
		selectionStart = now();
	};
	const teardownSelection = () => {
		log('selection mode off');
		document.body.removeChild(canvas.elem);
		canvas.y = 0;
		canvas.x = 0;
		canvas.visible = false;
		canvas.selection = false;
		canvas.move = false;
		canvas.mousedown = false;
	};

	const DOM_REFRESH_DELAY = 16;
	const sendSelection = function (e, strategy, canvas) {
		var end = now();
		var msg = {
			target: 'background',
			command: strategy === 'screenshotViewport' ? 'partialScreenshot' : 'screenshot',
			instanceId: instanceId,
			params:{
				image: {
					title: document.title,
					pos: canvas.pos(),
					bounds: getBoundsFor(canvas.elem)
				},
				infos: {
					strategy: strategy,
					clickPos: {
						x: canvas.x,
						y: canvas.y,
						shift: !!(e && e.shiftKey),
						meta: !!(e && e.metaKey),
						ctrl: !!(e && e.ctrlKey),
						alt: !!(e && e.altKey)
					},
					timer: {
						start: selectionStart,
						end: end,
						delta: end - selectionStart
					}
				},
				original: canvas.original,
				scrollTop: canvas.scrollTop
			}
		};
		setTimeout(function () {
			chrome.runtime.sendMessage(msg);
		}, DOM_REFRESH_DELAY);
	};
	let captureTimer = 0;
	const captureRectOpts = {
		opacity: 0.4
	};
	const CAPTURE_SELECTION_DELAY = 600;
	const CAPTURE_SCROLL_TOP_DELAY = 150;
	const captureSelection = (e, strategy) => {
		const selection = window.getSelection();
		if (selection.focusOffset === 0) {
			selection.collapse(document.body);
		}
		canvas.clear();
		canvas.rect(captureRectOpts);
		clearTimeout(captureTimer);
		captureTimer = setTimeout(function () {
			if (!canvas.mousedown && canvas.selection) {
				teardownSelection();
				sendSelection(e, strategy, canvas);
			}
			captureTimer = 0;
		}, CAPTURE_SELECTION_DELAY);
	};
	let keyupLastEvent = 0;
	let keyupHotKeyCount = 0;
	let keyupSelection = null;
	const keyupHotKeyTarget = 3;
	const keyupHotKeyTimeLimit = 2000;
	const keyupSelectionBounds = (pad) => {
		pad = pad || 0;
		const node = (elem) => {
			if (elem.nodeType === HTMLDocument.TEXT_NODE) {
				elem = elem.parentNode;
			}
			return elem;
		};
		const a = node(keyupSelection.anchorNode).getBoundingClientRect();
		const f = node(keyupSelection.focusOffset === 0 && keyupSelection.focusNode.previousSibling ?
			keyupSelection.focusNode.previousSibling :
			keyupSelection.focusNode
		).getBoundingClientRect();
		const b = {
			x1: Math.min(f.left, a.left),
			y1: Math.min(f.top, a.top),
			x2: Math.max(f.right, a.right),
			y2: Math.max(f.bottom, a.bottom)
		};
		if (pad !== 0) {
			b.x1 = Math.max(0, b.x1 - pad);
			b.y1 = Math.max(0, b.y1 - pad);
			b.x2 = Math.min(window.innerWidth, b.x2 + pad);
			b.y2 = Math.min(window.innerHeight, b.y2 + pad);
		}
		return b;
	};
	const keydown = function (e) {
		if (e.which === selectionAbortKey) {
			log('Hit abort');
			if (!!canvas.visible) {
				if (canvas.selection) {
					canvas.clear();
					canvas.selection = false;
					canvas.move = false;
					canvas.mousedown = false;
				}
				else {
					teardownSelection();
				}
				return preventDefault(e);
			}
		} else if (e.which === selectionHotKey) {
			log('Hit selection');
		} else if (!!canvas.visible && !!canvas.selection && e.which === selectionMoveKey) {
			log('Hit move');
			canvas.move = true;
			return preventDefault(e);
		}
	};
	const keyup = function (e) {
		if (!!canvas.visible) {
			if (!!canvas.selection && e.which === selectionMoveKey) {
				canvas.move = false;
				return preventDefault(e);
			}
			return;
		}
		if (e.which === selectionHotKey) {
			const n = now();
			if (keyupHotKeyCount > 0 && n - keyupLastEvent > keyupHotKeyTimeLimit) {
				if (keyupHotKeyCount > 0) {
					log('Selection time limit, bail out.');
				}
				keyupSelection = null;
				keyupHotKeyCount = 0;
				return;
			}
			const newSelection = window.getSelection();
			if (keyupHotKeyCount === 0) {
				keyupLastEvent = now();
				keyupSelection = {
					isCollapsed: newSelection.isCollapsed,
					anchorNode: newSelection.anchorNode,
					focusNode: newSelection.focusNode,
					anchorOffset: newSelection.anchorOffset,
					focusOffset: newSelection.focusOffset,
					type: newSelection.type,
					rangeCount: newSelection.rangeCount
				};
			}
			else if (newSelection.isCollapsed !== keyupSelection.isCollapsed ||
					newSelection.anchorNode !== keyupSelection.anchorNode ||
					newSelection.focusNode !== keyupSelection.focusNode ||
					newSelection.anchorOffset !== keyupSelection.anchorOffset ||
					newSelection.focusOffset !== keyupSelection.focusOffset ||
					newSelection.type !== keyupSelection.type ||
					newSelection.rangeCount !== keyupSelection.rangeCount
				) {
				log('Selection range change, bail out.');
				keyupSelection = null;
				keyupHotKeyCount = 0;
				return;
			}
			keyupHotKeyCount++;
			if (keyupHotKeyCount >= keyupHotKeyTarget) {
				initSelection(e);
				if (!!keyupSelection && !keyupSelection.isCollapsed) {
					log('Found valid text selection');
					var b = keyupSelectionBounds(8);
					canvas.selection = true;
					canvas.x = b.x1;
					canvas.y = b.y1;
					drawSelection(b.x2, b.y2);
					captureSelection(e, 'selection');
				}
				keyupSelection = null;
				keyupHotKeyCount = 0;
			}
		} else {
			keyupHotKeyCount = 0;
			keyupSelection = null;
		}
	};
	const mousemove = function (e) {
		if (!!canvas.mousedown) {
			canvas.selection = true;
			drawSelection(e.x, e.y);
			return preventDefault(e);
		}
	};
	const mouseup = function (e) {
		if (!!canvas.mousedown) {
			if (!!canvas.selection) {
				captureSelection(e, 'screenshot');
			}
			canvas.mousedown = false;
			return preventDefault(e);
		}
	};
	const mousedown = function (e) {
		if (!!canvas.visible) {
			if (!captureTimer) {
				canvas.clear();
				canvas.selection = false;
				canvas.move = false;
				canvas.mousedown = true;
				canvas.y = e.y;
				canvas.x = e.x;
			}
			return preventDefault(e);
		}
	};

	const cropToSelection = function (src, pos, cb) {
		if (!src) {
			log('Cannot crop empty source!');
			return;
		}
		const c = document.createElement('canvas');
		const pixelRatio = window.devicePixelRatio;

		c.width = (pos.x2 - pos.x1) * pixelRatio || 0;
		c.height = (pos.y2 - pos.y1) * pixelRatio || 0;
		if (c.width === 0 || c.height === 0) {
			log('Cannot create a ' + c.width + 'x' + c.height + ' image');
		} else {
			const img = new Image();
			img.onload = function () {
				const x = -pos.x1 * pixelRatio;
				const y = -pos.y1 * pixelRatio;
				c.getContext('2d').drawImage(img, x, y);
				const xport = validateIsCanvas(c);
				cb(xport.src);
			};
			img.src = src;
		}
	};

	const hostname = document.location.hostname;
	const localhost = hostname === 'localhost' || hostname === '127.0.0.1';
	const shftcl = hostname === 'shft.cl';
	const shftclDev = hostname === 'dev.shft.cl';
	let lastContextMenuEvent = null;

	/**
	 * Buckets UI
	 */
	const bucketText = function (bucket) {
		return bucket.bucket + (
			!bucket.count ? '' : ' (' + bucket.count + ')'
		);
	};
	const bucketLink = function (bucket) {
		const link = document.createElement('a');
		link.innerHTML = bucketText(bucket);
		link.setAttribute('data-hash', bucket.hash);
		link.setAttribute('href', '/bucket/' + bucket.hash);
		link.classList.add('bucket-link');
		return link;
	};

	/**
	 * Remote commands
	 */
	const commands = {
		click: function () {
			if (!lastContextMenuEvent) {
				log('right click ignored: no event found.');
				return;
			}
			click(lastContextMenuEvent, true);
			lastContextMenuEvent = null;
		},
		screenshot: function (params) {
			if (window.parent !== window) {
				log('Screenshot can only be taken from the top most window');
				return;
			}
			params.image.elem = canvas.elem;
			if (!!params.original) {
				imageFound(params.image, params.infos);
				return;
			}
			cropToSelection(params.image.src, params.image.pos, function (src) {
				if (!src) {
					log('screenshot failed :(');
				} else {
					params.image.src = src;
					imageFound(params.image, params.infos);
				}
			});
		},
		screenshotVisible: function (params) {
			if (window.parent !== window) {
				log('Screenshot can only be taken from the top most window');
				return;
			}
			sendSelection(undefined, 'screenshotVisible', {
				pos: function () {
					return {
						x1: window.scrollX,
						x2: window.scrollX + window.innerWidth,
						y1: window.scrollY,
						y2: window.scrollY + window.innerHeight
					};
				},
				x: window.scrollX,
				y: window.scrollY,
				elem: document.body,
				original: true
			});
		},
		screenshotViewport: function (params) {
			if (window.parent !== window) {
				log('Screenshot can only be taken from the top most window');
				return;
			}
			// Find max size
			const width = Math.max(
				document.documentElement.clientWidth,
				document.body.scrollWidth,
				document.documentElement.scrollWidth,
				document.body.offsetWidth,
				document.documentElement.offsetWidth
			);
			const height = Math.max(
				document.documentElement.clientHeight,
				document.body.scrollHeight,
				document.documentElement.scrollHeight,
				document.body.offsetHeight,
				document.documentElement.offsetHeight
			);
			// Copy original style
			const original = {
				overflow: document.body.style.overflow,
				scrollTop: window.scrollY
			};
			// Hide scroll
			document.body.style.overflow = 'hidden';
			window.scrollTo(0, 0);
			// Request screenshot
			log('Request full page capture of size ' + width + ' x ' + height);
			sendSelection(undefined, 'screenshotViewport', {
				pos: function () {
					return {
						x1: 0,
						x2: width,
						y1: 0,
						y2: height
					};
				},
				x: window.innerWidth,
				y: window.innerHeight,
				elem: document.body,
				original: original,
				scrollTop: 0
			});
		},
		scrollTop: function (params) {
			if (window.parent !== window) {
				log('scrollTop can only be called on the top most window');
				return;
			}
			// Scroll
			params.scrollTop += params.infos.clickPos.y;
			window.scrollTo(0, params.scrollTop);
			// Trigger scroll event
			window.dispatchEvent(new Event('scroll'));
			// Remove all position:fixed or position:sticky elements
			// after the first partial screen shot
			const hideElements = [];
			all(null, 'body *', function (elem) {
				const F = 'fixed';
				const S = 'sticky';
				if (!elem.hidden) {
					const isFixed = function (style) {
						return style.position === F || style.position === S;
					};
					const style = elem.style;
					const isFixedByStyle = isFixed(style);
					let isFixedByComputedStyle = false;
					if (!isFixedByStyle) {
						const computedStyle = window.getComputedStyle(elem);
						isFixedByComputedStyle = isFixed(computedStyle);
					}
					if (isFixedByStyle || isFixedByComputedStyle) {
						hideElements.push(function () {
							elem.data = elem.data || {};
							elem.data.originalOpacity = style.opacity;
							elem.style.opacity = 0;
							elem.classList.add('shift-click-hidden');
							elem.hidden = true;
						});
					}
				}
			});
			hideElements.forEach(function (hide) {
				hide();
			});
			// Let the browser do its job
			setTimeout(function () {
				// Request partial screenshot
				chrome.runtime.sendMessage({
					target: 'background',
					command: 'partialScreenshot',
					tabid: params.tabid,
					instanceId: instanceId,
					params: params
				});
			}, CAPTURE_SCROLL_TOP_DELAY);
		},
		screenshotViewportDone: function (params) {
			if (window.parent !== window) {
				log('screenshotViewportDone can only be called on the top most window');
				return;
			}
			// Restore original
			window.scrollTo(0, params.original.scrollTop);
			document.body.style.overflow = params.original.overflow;
			all(null, 'body .shift-click-hidden', function (elem) {
				elem.classList.remove('shift-click-hidden');
				elem.hidden = false;
				elem.style.opacity = elem.data.originalOpacity;
			});
			// Found image
			imageFound(params.image, params.infos);
		},
		open: function (params) {
			if (pendingCloseTabs[params.vtabid]) {
				chrome.runtime.sendMessage({
					target: 'background',
					command: 'close',
					tabid: params.tabid,
					instanceId: instanceId
				});
				delete pendingCloseTabs[params.vtabid];
			} else if (pendingRedirectionTabs[params.vtabid]) {
				chrome.runtime.sendMessage({
					target: 'background',
					command: 'change',
					url: pendingRedirectionTabs[params.vtabid],
					active: params.active,
					tabid: params.tabid,
					instanceId: instanceId
				});
				delete pendingRedirectionTabs[params.vtabid];
			} else {
				tabs[params.vtabid] = {
					tabid: params.tabid,
					active: params.active
				};
			}
			return true;
		},
		newImage: function (params, sender, response) {
			const changeTabUrl = function (computedUrl) {
				if (!tabs[params.vtabid]) {
					pendingRedirectionTabs[params.vtabid] = computedUrl;
					log('change tab pending set to ' + computedUrl);
					log('------------------------------');
				} else {
					chrome.runtime.sendMessage({
						target: 'background',
						command: 'change',
						url: computedUrl,
						active: tabs[params.vtabid].active,
						tabid: tabs[params.vtabid].tabid,
						instanceId: params.instanceId
					});
					log('tab changed to ' + computedUrl);
					log('------------------------------');
					delete tabs[params.vtabid];
				}
			};
			// Update state
			pendingImagesCount--;
			isDev && pendingImagesCount === 0 && document.body.removeChild(loading);
			// Check response for errors (network)
			if (!response.ok || !response.data) {
				const computedUrl = server + 'error/fatal';
				log('------------------------------');
				log('changing new tab to fatal error');
				changeTabUrl(computedUrl);
			} else {
				// Check for server errors
				if (!!response.data.error) {
					const errorCode = response.data.errorCode || response.data.error;
					const status = (!!~errorCode.indexOf('?') ? '&' : '?') + 's=' + response.status;
					const computedUrl = server + 'error/' + errorCode + status;
					log('------------------------------');
					log('changing new tab to error ' + errorCode);
					changeTabUrl(computedUrl);
				} else {
					// Should be ok!!
					let qs = [];
					if (!!response.data.token) {
						qs.push('t=' + response.data.token);
					}
					if (!qs.length) {
						qs = '';
					} else {
						qs = '?' + qs.join('&');
					}
					const computedUrl = server + response.data.hash + qs;
					log('------------------------------');
					log('changing new tab to ' + computedUrl);
					changeTabUrl(computedUrl);
				}
			}
		},
		ping: function (params, sender, response) {
			if (response.ok) {
				storage.set({ ping: Date.now() }, function () {
					if (chrome.runtime.lastError) {
						log('error while saving data: ' + chrome.runtime.lastError);
					} else if (user) {
						log('ping for user ' + user.id)
					} else {
						log('ping no user');
					}
				});
			} else {
				log('ping failed');
			}
		},
		myBuckets: function (params, sender, response) {
			if (!response.ok || !response.data || !!response.data.error) {
				log('Could not load remote buckets.');
			} else if (response.data.buckets) {
				all(document, '.js-chrome-ext-buckets-list', function (elem) {
					var node = elem.getAttribute('data-child-node');
					var link = elem.getAttribute('data-child-link');
					elem.innerHTML = '';
					each(response.data.buckets, function (bucket) {
						if (!!bucket.count) {
							return;
						}
						var n = document.createElement(node);
						if (!link) {
							n.setAttribute('value', bucket.bucket);
						} else {
							var l = bucketLink(bucket);
							n.appendChild(l);
						}
						elem.appendChild(n);
					});
				});
				all(document, '.js-chrome-ext-buckets-image', function (elem) {
					each(response.data.buckets, function (bucket) {
						if (!!bucket.count) {
							var l = bucketLink(bucket);
							elem.appendChild(l);
						}
					});
				});
			} else {
				log('No remote buckets found');
			}
		},
		newBucket: function (params, sender, response) {
			if (!response.ok || !response.data || !!response.data.error || !response.data.bucket) {
				log('Could not add to bucket');
			} else {
				const linkSel = '.js-chrome-ext-buckets-image a[data-hash="' + response.data.bucket.hash + '"]';
				const found = !!document.querySelector(linkSel);
				found || all(document, '.js-chrome-ext-buckets-image', function (elem) {
					const l = bucketLink(response.data.bucket);
					elem.appendChild(l);
				});
			}
		},
	};

	/**
	 * DEV MODE
	 * 
	 * enable with chrome.storage.local.set({'devMode': true});
	 * disable with chrome.storage.local.remove('devMode');
	 */
	storage.get('devMode', function (items) {
		if (!!items.devMode) {
			var devMode = items.devMode === true ? 'dev' : items.devMode;
			server = 'https://' + devMode + '.shft' + '.cl' + '/';
			chrome.runtime.sendMessage({
				target: 'background',
				command: 'enterDevMode'
			});
		}
	});

	/**
	 * Messaging with background script
	 */
	chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
		if (typeof request.instanceId !== 'undefined' && request.instanceId !== instanceId) {
			log('Ignored message `' + request.command + '` destined to ' + request.instanceId);
			return;
		}
		if (!!request.command) {
			if (!!commands[request.command]) {
				return commands[request.command](request.params, sender, request.response);
			} else {
				log('Command `' + request.command + '` ignored: not found.');
			}
		} else {
			console.info('No command found in request.');
		}
	});

	/**
	 * client ping
	 */
	const startPing = function () {
		if (window.parent !== window) {
			log('Ping disabled in child window');
			return;
		}
		if (window.navigator.doNotTrack) {
			log('Ping ignored for doNotTrack users');
			return;
		}
		var pingDelay = (~~(Math.random() * 100000) % 5000) + 2000;
		if (!(localhost && isDev)) {
			setTimeout(ping, pingDelay);
		}
		log('user ' + user.id + ' found. Init with delay ' + pingDelay);
	};

	/**
	 * User ID
	 */
	storage.get('user', function (items) {
		if (items.user && items.user.id) {
			user = items.user;
			startPing();
			return;
		}
		user = {
			id: (function () {
				var randomPool = new Uint8Array(32);
				crypto.getRandomValues(randomPool);
				var hex = '';
				for (var i = 0; i < randomPool.length; ++i) {
					hex += randomPool[i].toString(16);
				}
				return hex;
			})()
		};
		log('creating new user...');
		storage.set({ user: user }, function () {
			if (chrome.runtime.lastError) {
				log('error while saving user: ' + chrome.runtime.lastError);
			} else {
				startPing();
			}
		});
	});

	/**
	 * Extensions hooks
	 */
	if ((localhost && isDev) || shftcl || shftclDev) {
		if (shftcl || shftclDev) {
			$('.js-chrome-ext', $.remove);
		}
		$('.js-chrome-ext-paste', function (elem) {
			all(elem, '.js-chrome-ext-paste-target', function (elem) {
				elem.classList.remove('hidden');
				elem.addEventListener('dragenter', dragenter, false);
				elem.addEventListener('dragover', dragover, false);
				elem.addEventListener('dragleave', dragleave, false);
				elem.addEventListener('drop', drop, false);
			});
			all(elem, '.js-chrome-ext-paste-text', function (elem) {
				elem.classList.remove('hidden');
			});
			document.addEventListener('paste', paste, true);
			log('Paste and drag registered');
		});
		$('.js-chrome-ext-buckets', function (elem) {
			let hash;
			$('[data-hash]', function (c) {
				hash = c.getAttribute('data-hash');
			});
			elem.classList.remove('hidden');
			all(elem, 'input', function (input) {
				input.addEventListener('keyup', function (e) {
					if (e.which === 13 && input.value) {
						chrome.runtime.sendMessage({
							target: 'background',
							command: 'fetch',
							response: 'newBucket',
							instanceId: instanceId,
							params: {
								payload: {
									user: user,
									bucket: input.value,
									hash: hash
								},
								url: server + 'buckets/new'
							}
						});
						input.value = '';
						return preventDefault(e);
					}
				}, false);
			});
			setTimeout(function () {
				chrome.runtime.sendMessage({
					target: 'background',
					command: 'fetch',
					response: 'myBuckets',
					instanceId: instanceId,
					params: {
						payload: {
							user: user,
							hash: hash
						},
						url: server + 'buckets/my'
					}
				});
			}, 500);
		});
		return;
	}

	/**
	 * Black list
	 */
	const mouseBL = [
		/^https?:\/\/docs\.google\.com\//i,
		/^https?:\/\/.*invisionapp\.com\//i,
		/^https?:\/\/app\.uxpin\.com\//i,
		/^https?:\/\/.*figma\.com\//i,
		/^https?:\/\/.*gmail\.com\//i,
		/^https?:\/\/mail\.google\.com\//i,
		/^https?:\/\/web\.whatsapp\.com\//i,
		/^https?:\/\/connect\.emailsrvr\.com\//i,
		/^https?:\/\/draw\.io\//i,
		/^https?:\/\/.+\.celtra\.com\//i,
		/^https?:\/\/app\.smartsheet\.com\//i
	];
	const isMouseBLed = function () {
		var r = false;
		mouseBL.forEach(function (b, i) {
			if (r) {
				return;
			}
			r = b.test(window.location.href);
		});
		return r;
	};
	if (!isMouseBLed()) {
		document.addEventListener('mousedown', click, true);
		document.addEventListener('mouseup', noop, true);
		document.addEventListener('click', noop, true);
		if (isDev) {
			document.addEventListener('paste', paste, true);
		}
		document.addEventListener('keydown', lastKeyDown, true);
	} else {
		log('Mouse BL.');
	}

	/**
	 * Context Menu
	 */
	window.addEventListener('contextmenu', function (e) {
		lastContextMenuEvent = copyEvent(e);
	});

	/**
	 * Canvas support
	 */
	if (window.parent !== window) {
		log('Canvas disabled in child windows.');
	} else if (canvas.supported) {
		document.addEventListener('keydown', keydown, true);
		document.addEventListener('keyup', keyup, true);
		canvas.elem.addEventListener('mousemove', mousemove, true);
		canvas.elem.addEventListener('mousedown', mousedown, true);
		canvas.elem.addEventListener('mouseup', mouseup, true);
	} else {
		log('Canvas unsupported.');
	}

	/**
	 * Done.
	 */
	log('ext version ' + manifest.version);
	log('registered for ' + window.location);
})();
