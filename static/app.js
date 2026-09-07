// Halemans app JS: push subscription management on the profile page
// (milestone_1.md §8). The in-page banner fallback lives in
// halemans-live.js.
(function () {
    function urlBase64ToUint8Array(base64String) {
        var padding = '='.repeat((4 - base64String.length % 4) % 4);
        var base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
        var rawData = window.atob(base64);
        var outputArray = new Uint8Array(rawData.length);
        for (var i = 0; i < rawData.length; ++i) outputArray[i] = rawData.charCodeAt(i);
        return outputArray;
    }

    function setStatus(text) {
        var el = document.getElementById('push-status');
        if (el) el.textContent = text;
    }

    document.addEventListener('DOMContentLoaded', function () {
        var container = document.querySelector('[data-testid="push-settings"]');
        if (!container) return;
        var vapidKey = container.getAttribute('data-vapid-key');

        var subscribeButton = document.getElementById('push-subscribe-button');
        var unsubscribeButton = document.getElementById('push-unsubscribe-button');

        if (!('serviceWorker' in navigator) || !('PushManager' in window)) {
            setStatus('push unavailable in this browser — in-page banners active');
            return;
        }

        if (subscribeButton) subscribeButton.addEventListener('click', function () {
            Notification.requestPermission().then(function (permission) {
                if (permission !== 'granted') {
                    setStatus('permission denied — in-page banners active');
                    return;
                }
                navigator.serviceWorker.register('/push-sw.js').then(function (registration) {
                    return registration.pushManager.subscribe({
                        userVisibleOnly: true,
                        applicationServerKey: urlBase64ToUint8Array(vapidKey)
                    }).then(function (subscription) {
                        return fetch('/api/push/subscribe', {
                            method: 'POST',
                            headers: { 'Content-Type': 'application/json' },
                            body: JSON.stringify(subscription)
                        });
                    });
                }).then(function (response) {
                    setStatus(response.ok ? 'subscribed' : 'subscribe failed');
                }).catch(function () { setStatus('subscribe failed'); });
            });
        });

        if (unsubscribeButton) unsubscribeButton.addEventListener('click', function () {
            navigator.serviceWorker.getRegistration('/push-sw.js').then(function (registration) {
                if (!registration) { setStatus('not subscribed'); return; }
                registration.pushManager.getSubscription().then(function (subscription) {
                    if (!subscription) { setStatus('not subscribed'); return; }
                    fetch('/api/push/subscribe', {
                        method: 'DELETE',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ endpoint: subscription.endpoint })
                    }).then(function () {
                        subscription.unsubscribe();
                        setStatus('unsubscribed');
                    });
                });
            });
        });
    });
})();

// Localized timestamps: views render <time data-utc datetime="…Z"> with a
// UTC fallback text; here we rewrite to browser-local time with an offset
// label like (UTC+4). MutationObserver catches WS/turbolinks DOM inserts.
(function () {
    function pad(n) { return (n < 10 ? '0' : '') + n; }

    function offsetLabel(d) {
        var mins = -d.getTimezoneOffset();
        var sign = mins >= 0 ? '+' : '-';
        var abs = Math.abs(mins);
        var hours = Math.floor(abs / 60);
        var rest = abs % 60;
        return 'UTC' + sign + hours + (rest ? ':' + pad(rest) : '');
    }

    function localize(el) {
        var d = new Date(el.getAttribute('datetime'));
        if (isNaN(d.getTime())) return;
        el.textContent = d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate())
            + ' ' + pad(d.getHours()) + ':' + pad(d.getMinutes()) + ':' + pad(d.getSeconds())
            + ' (' + offsetLabel(d) + ')';
    }

    function localizeAll(root) {
        if (root.nodeType !== 1) return;
        if (root.matches && root.matches('time.utc-time')) localize(root);
        var els = root.querySelectorAll ? root.querySelectorAll('time.utc-time') : [];
        for (var i = 0; i < els.length; ++i) localize(els[i]);
    }

    window.halemansLocalizeTimes = localizeAll;

    document.addEventListener('DOMContentLoaded', function () {
        localizeAll(document.body);
        new MutationObserver(function (mutations) {
            for (var i = 0; i < mutations.length; ++i) {
                var added = mutations[i].addedNodes;
                for (var j = 0; j < added.length; ++j) localizeAll(added[j]);
            }
        }).observe(document.body, { childList: true, subtree: true });
    });
})();

// Filter multi-select dropdowns (data-filter-dropdown, /alerts and
// /env/:name): the menu stays open while checkboxes are toggled
// (data-bs-auto-close="outside"); the enclosing GET form is submitted once
// when the dropdown closes after the selection changed.
(function () {
    document.addEventListener('change', function (event) {
        var wrapper = event.target.closest && event.target.closest('[data-filter-dropdown]');
        if (wrapper) wrapper.setAttribute('data-dirty', '1');
    });
    document.addEventListener('hidden.bs.dropdown', function (event) {
        var wrapper = event.target.closest && event.target.closest('[data-filter-dropdown]');
        if (!wrapper || !wrapper.getAttribute('data-dirty')) return;
        wrapper.removeAttribute('data-dirty');
        var form = wrapper.closest('form');
        if (form) form.submit();
    });
})();

// Theme packs (milestone_3.md §7): swap [data-theme] on <html>, persist to
// localStorage, POST to /profile/theme (fire-and-forget).
(function () {
    var THEMES = ['latte', 'frappe', 'macchiato', 'dracula', 'light', 'dark'];
    var LIGHT_THEMES = ['latte', 'light'];
    var STORAGE_KEY = 'halemans-theme';

    function isValidTheme(theme) {
        return THEMES.indexOf(theme) !== -1;
    }

    function applyLocal(theme) {
        document.documentElement.dataset.theme = theme;
        document.documentElement.dataset.bsTheme = LIGHT_THEMES.indexOf(theme) !== -1 ? 'light' : 'dark';
        try { window.localStorage.setItem(STORAGE_KEY, theme); } catch (e) {}
        var choices = document.querySelectorAll('[data-theme-choice]');
        for (var i = 0; i < choices.length; ++i) {
            choices[i].classList.toggle('active', choices[i].dataset.themeChoice === theme);
        }
    }

    window.halemansApplyTheme = function (theme) {
        if (!isValidTheme(theme)) return;
        applyLocal(theme);
        try {
            fetch('/profile/theme', {
                method: 'POST',
                credentials: 'same-origin',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ theme: theme })
            }).catch(function () {});
        } catch (e) {}
    };

    document.addEventListener('DOMContentLoaded', function () {
        var stored = null;
        try { stored = window.localStorage.getItem(STORAGE_KEY); } catch (e) {}
        if (stored && isValidTheme(stored) && stored !== document.documentElement.dataset.theme) {
            applyLocal(stored);
        }
        document.addEventListener('click', function (event) {
            var el = event.target.closest('[data-theme-choice]');
            if (el) window.halemansApplyTheme(el.dataset.themeChoice);
        });
    });
})();
