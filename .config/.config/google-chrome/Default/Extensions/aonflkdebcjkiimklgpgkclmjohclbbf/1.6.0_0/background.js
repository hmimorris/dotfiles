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

	const ONLY_TABS = 'Only tabs can send this message.';
	const manifest = chrome.runtime.getManifest();
	const isDev = !('update_url' in manifest);
	const DEFAULT_IMAGE_OPTS = {
		format: 'jpeg',
		quality: 100
	};
	const JSON_MIME = 'application/json';

	const checkForError = function () {
		if (chrome.runtime.lastError) {
			console.error(chrome.runtime.lastError);
			return true;
		}
		return false;
	};

	const parseAndFormatData = function (response, text) {
		var data = {};
		if (response.status === 429) {
			data.error = 'too-many-requests?retry-after=' + xhr.getResponseHeader('retry-after');
		} else if (response.status === 503) {
			data.error = 'unavailable?retry-after=' + xhr.getResponseHeader('retry-after');
		} else if (!text || !response.status) {
			data.error = 'empty-response';
		} else {
			try {
				data = JSON.parse(text);
			}
			catch (ex) {
				data.error = 'invalid-json?msg=' + ex.message;
			}
		}
		return data;
	};

	const screenshotCanvas = document.createElement("canvas");
	const screenshotContext = screenshotCanvas.getContext("2d")

	const captureVisibleTab = function (request, sender, success) {
		const opts = request.opts || DEFAULT_IMAGE_OPTS;
		const callback = function (image) {
			if (checkForError()) {
				return;
			}
			success(image);
		};
		if (!request.params.image.pos && chrome.tabs.captureTab) {
			chrome.tabs.captureTab(sender.tab.windowId, opts, callback);
		} else {
			chrome.tabs.captureVisibleTab(sender.tab.windowId, opts, callback);
		}
	};

	/**
	 * Context Menu
	 */
	chrome.contextMenus.create({
		type: 'normal',
		title: 'Shift Click it',
		contexts: ['page', 'selection', 'image'],
		onclick: function (info, tab) {
			chrome.tabs.sendMessage(tab.id, {
				command: 'click'
			});
		}
	}, checkForError);

	let devModeIsRegistered = false;
	const registerDevModeContextMenus = function () {
		if (devModeIsRegistered) {
			return;
		}
		chrome.contextMenus.create({
			type: 'normal',
			title: 'Capture visible part (WIP)',
			contexts: ['all'],
			onclick: function (info, tab) {
				chrome.tabs.sendMessage(tab.id, {
					command: 'screenshotVisible'
				});
			}
		}, checkForError);

		chrome.contextMenus.create({
			type: 'normal',
			title: 'Capture all document (WIP)',
			contexts: ['all'],
			onclick: function (info, tab) {
				chrome.tabs.sendMessage(tab.id, {
					command: 'screenshotViewport'
				});
			}
		}, checkForError);

		devModeIsRegistered = true;
	};

	if (isDev) {
		registerDevModeContextMenus();
	}

	/**
	 * Remote commands
	 */
	var commands = {
		open: function (request, sender) {
			if (!sender.tab) {
				throw new Error(ONLY_TABS);
			}
			chrome.tabs.create({
				index: sender.tab.index + 1,
				url: request.url,
				active: request.active,
				windowId: sender.tab.windowId,
				openerTabId: sender.tab.id
			}, function (tab) {
				if (checkForError()) {
					return true;
				}
				chrome.tabs.sendMessage(sender.tab.id, {
					command: request.command,
					instanceId: request.instanceId,
					params: {
						tabid: tab.id,
						vtabid: request.tabid,
						windowId: sender.tab.windowId,
						active: request.active
					}
				});
			});
		},
		screenshot: function (request, sender) {
			if (!sender.tab) {
				throw new Error(ONLY_TABS);
			}
			captureVisibleTab(request, sender, function (image) {
				const params = request.params;
				params.image.src = image;
				chrome.tabs.sendMessage(sender.tab.id, {
					command: request.command,
					instanceId: request.instanceId,
					params: params
				});
			});
		},
		partialScreenshot: function (request, sender) {
			if (!sender.tab) {
				throw new Error(ONLY_TABS);
			}
			const pixelRatio = window.devicePixelRatio;
			if (request.params.scrollTop === 0) {
				screenshotCanvas.width = request.params.image.pos.x2 * pixelRatio;
				screenshotCanvas.height = request.params.image.pos.y2 * pixelRatio;
			}
			captureVisibleTab(request, sender, function (image) {
				const params = request.params;
				const img = new Image();
				img.onload = function () {
					// Are we past the end ?
					const maxY = request.params.image.pos.y2 - request.params.infos.clickPos.y;
					const isLast = request.params.scrollTop >= maxY;
					const yDraw = !isLast ? request.params.scrollTop : maxY;
					screenshotContext.drawImage(img, 0, yDraw * pixelRatio);
					
					if (isLast) {
						params.image.src = screenshotCanvas.toDataURL('image/jpeg');
						chrome.tabs.sendMessage(sender.tab.id, {
							command: 'screenshotViewportDone',
							instanceId: request.instanceId,
							params: params
						});
					} else {
						chrome.tabs.sendMessage(sender.tab.id, {
							command: 'scrollTop',
							instanceId: request.instanceId,
							params: params
						});
					}
				};
				img.src = image;
			});
		},
		change: function (request, sender) {
			if (!sender.tab) {
				throw new Error(ONLY_TABS);
			}
			if (!request.tabid) {
				chrome.tabs.create({
					index: sender.tab.index + 1,
					url: request.url,
					active: request.active,
					windowId: sender.tab.windowId,
					openerTabId: sender.tab.id
				}, checkForError);
			}
			else {
				chrome.tabs.update(request.tabid, {
					url: request.url,
					active: request.active
					//loadReplace: true // not working in Chrome, but works in FF
				}, checkForError);
			}
		},
		close: function (request, sender) {
			chrome.tabs.remove(request.tabid, checkForError);
		},
		fetch: function (request, sender) {
			const payload = (!!request.params.payload && typeof request.params.payload === 'object') ?
				JSON.stringify(request.params.payload) :
				request.params.payload;
			const headers = new Headers();
			const mime = request.params.mime || JSON_MIME;
			headers.append('Content-Type', mime);
			fetch(request.params.url, {
				method: request.params.verb || 'POST',
				headers: headers,
				body: payload,
				credentials: 'include',
				mode: 'cors',
				keepalive: request.params.keepalive,
				cache: 'no-cache'
			}).then(function (response) {
				return Promise.all([
					Promise.resolve(response),
					response.text()
				]);
			}).then(function (result) {
				const response = result[0];
				const text = result[1];
				const isJson = response.headers.get('content-type').indexOf(JSON_MIME) === 0;
				const data = isJson ? parseAndFormatData(response, text) : undefined;
				
				chrome.tabs.sendMessage(sender.tab.id, {
					command: request.response,
					instanceId: request.instanceId,
					params: {
						tabid: sender.tab.id,
						vtabid: request.tabid,
						windowId: sender.tab.windowId,
						active: request.active
					},
					response: {
						ok: response.ok,
						text: text,
						data: data,
						status: response.status,
						statusText: response.statusText
					}
				});
			}).catch(function (err) {
				chrome.tabs.sendMessage(sender.tab.id, {
					command: request.response,
					instanceId: request.instanceId,
					params: {
						tabid: sender.tab.id,
						vtabid: request.tabid,
						windowId: sender.tab.windowId,
						active: request.active
					},
					response: {
						ok: false,
						text: err.toString(),
						status: 0,
						statusText: 'Fatal error'
					}
				});
			});
			return true;
		},
		enterDevMode: function (request, sender) {
			registerDevModeContextMenus();
		}
	};
	chrome.runtime.onMessage.addListener(function(request, sender, sendResponse) {
		if (request.target !== 'background') {
			console.info('Received a command, `' + request.command + '`, not destined to background.');
			return;
		}
		if (!!request.command) {
			if (!!commands[request.command]) {
				return commands[request.command](request, sender);
			} else {
				console.info('Command `' + request.command + '` ignored: not found.');
			}
		} else {
			console.info('No command found in request.');
		}
	});
})();
