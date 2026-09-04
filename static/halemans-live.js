// Halemans live updates client (milestone_1.md §7).
// One websocket per tab; server sends pre-rendered HSX fragments which we
// swap into the DOM by id. HTTP render stays the source of truth: after a
// reconnect gap we reload the page.
(function () {
    if (!('WebSocket' in window)) return;

    var reconnectDelay = 500;
    var disconnectedAt = null;

    function scopesFromPage() {
        var el = document.querySelector('[data-live-scope]');
        if (!el) return [{ type: 'none' }];
        return el.getAttribute('data-live-scope').split(',').map(function (scope) {
            scope = scope.trim();
            if (scope === 'dashboard') return { type: 'dashboard' };
            if (scope === 'alerts') return { type: 'alerts' };
            if (scope.indexOf('env:') === 0) return { type: 'env', name: scope.slice(4) };
            if (scope.indexOf('alert:') === 0) return { type: 'alert', id: scope.slice(6) };
            if (scope.indexOf('group:') === 0) return { type: 'group', id: scope.slice(6) };
            return { type: 'none' };
        });
    }

    function applyUpdate(update) {
        var target = document.getElementById(update.id);
        if (target && window.morphdom) {
            window.morphdom(target, update.html);
        } else if (target) {
            target.outerHTML = update.html;
        } else if (update.mode === 'replaceOrPrepend' && update.parent) {
            var parent = document.getElementById(update.parent);
            if (parent) parent.insertAdjacentHTML('afterbegin', update.html);
        } else if (update.mode === 'prepend') {
            var container = document.getElementById(update.id);
            if (container) container.insertAdjacentHTML('afterbegin', update.html);
        }
    }

    function showBanner(banner) {
        var el = document.getElementById('push-banner');
        if (el) {
            el.textContent = (banner.severity || '').toUpperCase() + ': ' + (banner.title || 'new alert');
            el.classList.remove('d-none');
            el.onclick = function () { window.location = '/alerts/' + banner.alertId; };
        }
        if ('Notification' in window && Notification.permission === 'granted') {
            new Notification(banner.title || 'Halemans alert', {
                body: banner.severity,
                data: { url: '/alerts/' + banner.alertId }
            });
        }
    }

    function connect() {
        var proto = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        var socket = new WebSocket(proto + '//' + window.location.host + '/ws');

        socket.onopen = function () {
            reconnectDelay = 500;
            if (disconnectedAt && (Date.now() - disconnectedAt) > 3000) {
                window.location.reload();
                return;
            }
            disconnectedAt = null;
            scopesFromPage().forEach(function (scope) {
                socket.send(JSON.stringify(scope));
            });
        };

        socket.onmessage = function (event) {
            var message;
            try { message = JSON.parse(event.data); } catch (e) { return; }
            (message.updates || []).forEach(applyUpdate);
            if (message.banner) showBanner(message.banner);
        };

        socket.onclose = function () {
            if (!disconnectedAt) disconnectedAt = Date.now();
            setTimeout(connect, reconnectDelay);
            reconnectDelay = Math.min(reconnectDelay * 2, 10000);
        };
    }

    if (!window.Turbolinks) {
        connect();
    } else {
        connect();
    }
})();
