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

// Blackout form scope filtering: the scope-id select carries options for all
// three scope kinds with "type:" prefixed values; show only the selected
// kind and auto-select the first visible option on a type change.
(function () {
    document.addEventListener('DOMContentLoaded', function () {
        var typeSelect = document.querySelector('[data-testid="blackout-scope-type"]');
        var idSelect = document.querySelector('[data-testid="blackout-scope-id"]');
        if (!typeSelect || !idSelect) return;
        var applyFilter = function () {
            var prefix = typeSelect.value + ':';
            Array.prototype.forEach.call(idSelect.options, function (option) {
                var visible = option.value.indexOf(prefix) === 0;
                option.hidden = !visible;
                option.disabled = !visible;
            });
            var selected = idSelect.options[idSelect.selectedIndex];
            if (!selected || selected.disabled) {
                var first = Array.prototype.find.call(idSelect.options, function (o) { return !o.disabled; });
                if (first) idSelect.value = first.value;
            }
        };
        typeSelect.addEventListener('change', applyFilter);
        applyFilter();
    });
})();

// Team form pickers (moved from inline scripts in Web/View/Teams): a
// filterable host-group multi-select plus a member list showing only current
// members, with filter-to-add and a remove button. Keyed on the
// data-hg-filter / data-hg-select / data-member-* hooks.
(function () {
    var init = function () {
        var hgFilter = document.querySelector('[data-hg-filter]');
        var hgSelect = document.querySelector('[data-hg-select]');
        if (hgFilter && hgSelect && !hgFilter.dataset.init) {
            hgFilter.dataset.init = '1';
            hgFilter.addEventListener('input', function () {
                var q = hgFilter.value.toLowerCase();
                Array.prototype.forEach.call(hgSelect.options, function (option) {
                    option.hidden = option.text.toLowerCase().indexOf(q) === -1;
                });
            });
        }

        var membersFilter = document.querySelector('[data-member-filter]');
        if (!membersFilter || membersFilter.dataset.init) return;
        membersFilter.dataset.init = '1';
        var rows = document.querySelectorAll('[data-member-row]');
        var applyVisibility = function () {
            var q = membersFilter.value.toLowerCase();
            Array.prototype.forEach.call(rows, function (row) {
                var isMember = row.querySelector('select').value !== '';
                var matches = row.getAttribute('data-email').toLowerCase().indexOf(q) !== -1;
                row.style.display = (isMember || (q !== '' && matches)) ? '' : 'none';
            });
        };
        membersFilter.addEventListener('input', applyVisibility);
        Array.prototype.forEach.call(rows, function (row) {
            row.querySelector('select').addEventListener('change', applyVisibility);
            row.querySelector('[data-member-remove]').addEventListener('click', function () {
                row.querySelector('select').value = '';
                applyVisibility();
            });
        });
        applyVisibility();
    };
    document.addEventListener('DOMContentLoaded', init);
    document.addEventListener('turbolinks:load', init);
})();

// Local-datetime range inputs (/reports from/to): the visible input is a
// flatpickr calendar with free text allowed (relative expressions like
// "now() - 7d" pass through untouched). The canonical value lives in a
// hidden form field: local datetimes are converted to UTC ISO on edit, UTC
// ISO values from the server are shown in browser-local time — the same
// local-timezone convention as the <time data-utc> rendering above.
(function () {
    var ISO_Z = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2})?(\.\d+)?Z$/;
    var LOCAL = /^\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}(:\d{2})?$/;

    function pad(n) { return (n < 10 ? '0' : '') + n; }

    function toLocalText(d) {
        return d.getFullYear() + '-' + pad(d.getMonth() + 1) + '-' + pad(d.getDate())
            + ' ' + pad(d.getHours()) + ':' + pad(d.getMinutes());
    }

    function toUtcText(value) {
        var d = new Date(value.replace(' ', 'T'));
        if (isNaN(d.getTime())) return null;
        return d.toISOString().replace(/\.\d{3}Z$/, 'Z');
    }

    function syncHidden(input, hidden) {
        var v = input.value.trim();
        if (LOCAL.test(v)) {
            var utc = toUtcText(v);
            hidden.value = utc || v;
        } else {
            hidden.value = v;
        }
    }

    function wireWindowPreset(select) {
        var form = select.form;
        if (!form) return;
        var fromHidden = form.querySelector('input[type="hidden"][name="from"]');
        var toHidden = form.querySelector('input[type="hidden"][name="to"]');
        var fromInput = form.querySelector('input[data-local-datetime="from"]');
        var toInput = form.querySelector('input[data-local-datetime="to"]');
        if (!fromHidden || !toHidden || !fromInput || !toInput) return;

        // reflect the current from/to in the preset select (Custom when the
        // range doesn't exactly match a preset expression)
        var values = Array.prototype.map.call(select.options, function (o) { return o.value; });
        var toIsNow = toHidden.value === '' || toHidden.value === 'now()';
        select.value = toIsNow && values.indexOf(fromHidden.value) !== -1 ? fromHidden.value : '';

        var applyingPreset = false;
        select.onchange = function () {
            if (!select.value) return;
            applyingPreset = true;
            fromInput.value = select.value;
            toInput.value = 'now()';
            fromInput.dispatchEvent(new Event('input', { bubbles: true }));
            toInput.dispatchEvent(new Event('input', { bubbles: true }));
            applyingPreset = false;
        };
        // any manual from/to edit (typing or calendar pick) resets the
        // preset to Custom
        var resetOnEdit = function (event) {
            if (applyingPreset) return;
            if (event.target && event.target.hasAttribute && event.target.hasAttribute('data-local-datetime')) select.value = '';
        };
        form.oninput = resetOnEdit;
        form.onchange = resetOnEdit;
    }

    var init = function () {
        var inputs = document.querySelectorAll('input[data-local-datetime]');
        Array.prototype.forEach.call(inputs, function (input) {
            if (input.dataset.init) return;
            var form = input.form;
            if (!form) return;
            var hidden = form.querySelector('input[type="hidden"][name="' + input.getAttribute('data-local-datetime') + '"]');
            if (!hidden) return;
            input.dataset.init = '1';
            if (ISO_Z.test(hidden.value)) {
                var d = new Date(hidden.value);
                input.value = isNaN(d.getTime()) ? hidden.value : toLocalText(d);
            } else {
                input.value = hidden.value;
            }
            // property handlers (not addEventListener): morphdom keeps the
            // node across form submits and re-runs this init, so listeners
            // would otherwise accumulate
            input.onchange = function () { syncHidden(input, hidden); };
            input.oninput = function () { syncHidden(input, hidden); };
            if (typeof window.flatpickr === 'function') {
                if (input._flatpickr) input._flatpickr.destroy();
                window.flatpickr(input, { enableTime: true, time_24hr: true, allowInput: true, dateFormat: 'Y-m-d H:i' });
                // IHP's morphdom special-cases .flatpickr-input nodes and
                // overwrites the value after a form morph, racing the local-
                // time conversion above; dropping the marker class keeps the
                // default value sync (canonical value is rendered server-side
                // into the value attribute) and lets this init own display.
                input.classList.remove('flatpickr-input');
            }
        });
        var presets = document.querySelectorAll('select[data-window-preset]');
        Array.prototype.forEach.call(presets, wireWindowPreset);
    };
    document.addEventListener('DOMContentLoaded', init);
    document.addEventListener('turbolinks:load', init);
})();

// CSP-friendly replacements for inline handlers (milestone 12 §8):
// [data-autosubmit] inputs submit their form on change; forms with
// [data-confirm] ask before submitting.
(function () {
    document.addEventListener('change', function (event) {
        var el = event.target.closest && event.target.closest('[data-autosubmit]');
        if (el && el.form) el.form.submit();
    });
    document.addEventListener('submit', function (event) {
        var message = event.target.getAttribute && event.target.getAttribute('data-confirm');
        if (message && !window.confirm(message)) event.preventDefault();
    });
})();
