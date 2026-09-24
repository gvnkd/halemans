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
// UTC fallback text; here we rewrite to local time with an offset label like
// (UTC+4). The zone is the user's configured profile timezone
// (<html data-tz>, fixed offset "UTC±N") or the browser zone when unset.
// Elements with data-tz-format="hour" (dashboard hourly buckets) render as
// HH only. MutationObserver catches WS/turbolinks DOM inserts.
(function () {
    function pad(n) { return (n < 10 ? '0' : '') + n; }

    // "UTC+3" -> "Etc/GMT-3" (IANA Etc signs are inverted); null for
    // anything unexpected, falling back to the browser zone.
    function configuredTz() {
        var tz = document.documentElement.getAttribute('data-tz');
        if (!tz) return null;
        if (tz === 'UTC') return 'UTC';
        var m = tz.match(/^UTC([+-])(\d{1,2})$/);
        if (!m) return null;
        return 'Etc/GMT' + (m[1] === '+' ? '-' : '+') + parseInt(m[2], 10);
    }

    function offsetLabel(d) {
        var mins = -d.getTimezoneOffset();
        var sign = mins >= 0 ? '+' : '-';
        var abs = Math.abs(mins);
        var hours = Math.floor(abs / 60);
        var rest = abs % 60;
        return 'UTC' + sign + hours + (rest ? ':' + pad(rest) : '');
    }

    function tzParts(d, tz) {
        if (!tz) return null;
        try {
            var fmt = new Intl.DateTimeFormat('en-US', {
                timeZone: tz, hourCycle: 'h23',
                year: 'numeric', month: '2-digit', day: '2-digit',
                hour: '2-digit', minute: '2-digit', second: '2-digit'
            });
            var raw = fmt.formatToParts(d);
            var out = {};
            for (var i = 0; i < raw.length; ++i) out[raw[i].type] = raw[i].value;
            return out;
        } catch (e) { return null; }
    }

    function localize(el) {
        var d = new Date(el.getAttribute('datetime'));
        if (isNaN(d.getTime())) return;
        var label = document.documentElement.getAttribute('data-tz') || null;
        var p = tzParts(d, configuredTz());
        if (el.getAttribute('data-tz-format') === 'hour') {
            el.textContent = p ? p.hour : pad(d.getHours());
            return;
        }
        if (p) {
            el.textContent = p.year + '-' + p.month + '-' + p.day
                + ' ' + p.hour + ':' + p.minute + ':' + p.second
                + ' (' + (label || 'UTC') + ')';
            return;
        }
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

    // Turbolinks renders via morphdom (turbolinksMorphdom): when the new page
    // keeps rows in place, existing <time> elements are reused and only their
    // textContent reverts to the UTC fallback — invisible to the childList
    // observer below. Re-localize after every turbolinks render.
    document.addEventListener('turbolinks:render', function () {
        if (document.body) localizeAll(document.body);
    });

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
    var THEMES = ['latte', 'frappe', 'macchiato', 'dracula', 'light', 'dark', 'halemans-dark', 'halemans-light'];
    var LIGHT_THEMES = ['latte', 'light', 'halemans-light'];
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

// Local-datetime range inputs (/reports from/to): the visible input is free
// text (relative expressions like "now() - 7d" pass through untouched); the
// flatpickr calendar hangs off the icon button next to it and writes the
// picked date into the input. The canonical value lives in a hidden form
// field: local datetimes are converted to UTC ISO on edit, UTC ISO values
// from the server are shown in browser-local time — the same local-timezone
// convention as the <time data-utc> rendering above.
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
                // the calendar must NOT be attached to the text input:
                // flatpickr parses the input value on Enter/blur and clears
                // it when the text is an expression it cannot parse
                var toggle = form.querySelector('button[data-calendar-toggle="' + input.getAttribute('data-local-datetime') + '"]');
                if (toggle) {
                    if (toggle._flatpickr) toggle._flatpickr.destroy();
                    window.flatpickr(toggle, {
                        enableTime: true,
                        time_24hr: true,
                        dateFormat: 'Y-m-d H:i',
                        onChange: function (dates) {
                            if (!dates.length) return;
                            input.value = toLocalText(dates[0]);
                            input.dispatchEvent(new Event('input', { bubbles: true }));
                        }
                    });
                    // flatpickr init sets type="text" on its element, which
                    // turns a <button> into a form submit button — restore it
                    toggle.type = 'button';
                    // IHP's morphdom special-cases .flatpickr-input nodes and
                    // overwrites the value after a form morph; dropping the
                    // marker class lets this init own display
                    toggle.classList.remove('flatpickr-input');
                }
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

// [data-metric-chart-url] buttons lazy-load the alert metric chart into the
// target container and then toggle to a close button; the range/scale
// selects (#metric-chart-range, #metric-chart-scale) become query params and
// re-fetch the chart on change while it is open (window switches without a
// page reload). The chart SVG gets a cursor-tracking hover tooltip built
// from the data-chart-series / data-chart-tlo/thi payload the fragment
// embeds (points as [epochSeconds, value]).
(function () {
    function chartParams() {
        var p = [];
        var range = document.getElementById('metric-chart-range');
        var scale = document.getElementById('metric-chart-scale');
        if (range && range.value !== 'alert') p.push('range=' + encodeURIComponent(range.value));
        if (scale && scale.value !== 'auto') p.push('scale=' + encodeURIComponent(scale.value));
        return p.length ? '?' + p.join('&') : '';
    }
    function loadChart(url, target, btn) {
        if (btn) btn.disabled = true;
        target.textContent = '…';
        fetch(url + chartParams(), { headers: { 'X-Requested-With': 'fetch' } })
            .then(function (response) {
                if (!response.ok) throw new Error('http ' + response.status);
                return response.text();
            })
            .then(function (html) {
                target.innerHTML = html;
                target.setAttribute('data-metric-chart-url', url);
                attachChartTooltip(target);
                if (btn) {
                    btn.disabled = false;
                    btn.setAttribute('data-metric-chart-open', '1');
                    btn.textContent = btn.getAttribute('data-label-close') || 'Close metrics';
                }
            })
            .catch(function () {
                target.textContent = '';
                if (btn) btn.disabled = false;
            });
    }
    function closeChart(target, btn) {
        target.innerHTML = '';
        target.removeAttribute('data-metric-chart-url');
        btn.removeAttribute('data-metric-chart-open');
        btn.textContent = btn.getAttribute('data-label-open') || 'Show metrics';
    }
    function pad2(n) { return (n < 10 ? '0' : '') + n; }
    function formatTipTime(t, withDate) {
        var d = new Date(t * 1000);
        var hms = pad2(d.getHours()) + ':' + pad2(d.getMinutes()) + ':' + pad2(d.getSeconds());
        if (!withDate) return hms;
        return d.getFullYear() + '-' + pad2(d.getMonth() + 1) + '-' + pad2(d.getDate()) + ' ' + hms;
    }
    function attachChartTooltip(container) {
        var host = container.querySelector('[data-chart-series]') || container;
        var svg = host.querySelector('svg');
        var raw = host.getAttribute('data-chart-series');
        var tLo = parseFloat(host.getAttribute('data-chart-tlo'));
        var tHi = parseFloat(host.getAttribute('data-chart-thi'));
        var yLo = parseFloat(host.getAttribute('data-chart-ylo'));
        var yHi = parseFloat(host.getAttribute('data-chart-yhi'));
        var scale = host.getAttribute('data-chart-scale') || 'linear';
        var plotLeft = parseFloat(host.getAttribute('data-chart-plotleft')) || 60;
        var plotRight = parseFloat(host.getAttribute('data-chart-plotright')) || 870;
        if (!svg || !raw || isNaN(tLo) || isNaN(tHi) || isNaN(yLo) || isNaN(yHi)) return;
        var series;
        try { series = JSON.parse(raw); } catch (e) { return; }
        if (!series.length) return;
        // viewBox constants mirrored from Application/Service/Chart.hs
        var plotW = plotRight - plotLeft;
        var plotBottom = 34, plotTop = 218; // frameH 250 - legend 24 - 8
        var span = tHi - tLo;
        var ySpan = yHi - yLo;
        var withDate = span > 2 * 86400;
        function toDom(v) {
            if (scale === 'linear') return v;
            return Math.log(Math.max(v, 1e-12)) / Math.log(scale === 'log2' ? 2 : 10);
        }
        function pointY(v) {
            var y = plotBottom + (toDom(v) - yLo) / ySpan * (plotTop - plotBottom);
            return Math.min(plotTop, Math.max(plotBottom, y));
        }
        var tip = document.createElement('div');
        tip.className = 'metric-chart-tooltip';
        host.appendChild(tip);
        svg.addEventListener('mousemove', function (event) {
            var rect = svg.getBoundingClientRect();
            var vx = (event.clientX - rect.left) / rect.width * 900;
            // svg y grows downward, viewBox y is up: flip
            var vy = (rect.height - (event.clientY - rect.top)) / rect.height * 250;
            if (vx < plotLeft || vx > plotRight || vy < plotBottom - 20 || vy > plotTop + 20) {
                tip.style.display = 'none';
                return;
            }
            var t = tLo + (vx - plotLeft) / plotW * span;
            // tolerances: nearest sample within ~8 units horizontally AND
            // ~14 vertically — points close in x but far apart in y no
            // longer fight over the tooltip (no flicker)
            var maxGap = span / plotW * 8;
            var rows = [];
            for (var i = 0; i < series.length; i++) {
                var pts = series[i].points;
                if (!pts.length) continue;
                var lo = 0, hi = pts.length - 1;
                while (hi - lo > 1) {
                    var mid = (lo + hi) >> 1;
                    if (pts[mid][0] < t) lo = mid; else hi = mid;
                }
                var near = Math.abs(pts[lo][0] - t) <= Math.abs(pts[hi][0] - t) ? lo : hi;
                if (Math.abs(pts[near][0] - t) > maxGap) continue;
                if (Math.abs(pointY(pts[near][1]) - vy) > 14) continue;
                rows.push({ name: series[i].name, value: pts[near][2] });
            }
            if (!rows.length) { tip.style.display = 'none'; return; }
            var lines = [formatTipTime(t, withDate)];
            for (var j = 0; j < rows.length; j++) {
                lines.push(rows[j].name + ': ' + rows[j].value);
            }
            tip.textContent = lines.join('\n');
            tip.style.display = 'block';
            var x = event.clientX - rect.left + 14;
            var y = event.clientY - rect.top + 12;
            if (x + 220 > rect.width) x = event.clientX - rect.left - 220;
            tip.style.left = x + 'px';
            tip.style.top = y + 'px';
        });
        svg.addEventListener('mouseleave', function () { tip.style.display = 'none'; });
    }
    document.addEventListener('click', function (event) {
        var btn = event.target.closest && event.target.closest('button[data-metric-chart-url]');
        if (!btn) return;
        event.preventDefault();
        var target = document.getElementById(btn.getAttribute('data-metric-chart-target'));
        if (!target) return;
        if (btn.getAttribute('data-metric-chart-open')) closeChart(target, btn);
        else loadChart(btn.getAttribute('data-metric-chart-url'), target, btn);
    });
    document.addEventListener('change', function (event) {
        var sel = event.target;
        if (!sel.id || (sel.id !== 'metric-chart-range' && sel.id !== 'metric-chart-scale')) return;
        var container = document.getElementById('metric-chart-container');
        if (!container || !container.querySelector('svg')) return; // not open yet
        var url = container.getAttribute('data-metric-chart-url');
        if (url) loadChart(url, container, null);
    });
})();

// Floating agent chat widget (internal API milestone). Collapsed affordance
// on every page; the panel POSTs {message, session_id, page_context} to
// /agent/chat and renders the persisted assistant replies. Session id lives
// in localStorage so the conversation survives navigation.
//
// Events are DELEGATED at document level (same pattern as the
// data-metric-chart handlers above), NOT attached to the widget nodes:
// turbolinks-morphdom patches can replace inner nodes between page renders
// while keeping an ancestor alive, which silently drops listeners attached
// per-node.
//
// The input row is NOT a <form>: IHP's helpers.js intercepts every submit
// event document-wide and XHR-submits the form itself (missing action ->
// "null"), so a real form here can never be fully controlled by us. Send is
// a plain button + an Enter keydown (see sendMessage below).
(function () {
    var SESSION_KEY = 'halemans-agent-session';

    function sessionId() { return window.localStorage.getItem(SESSION_KEY) || null; }
    function setSession(id) { window.localStorage.setItem(SESSION_KEY, String(id)); }

    function append(messages, role, text) {
        var div = document.createElement('div');
        div.className = 'agent-msg agent-msg-' + role;
        div.textContent = text; // textContent: no HTML injection from the model
        messages.appendChild(div);
        messages.scrollTop = messages.scrollHeight;
        return div;
    }

    function pageContext() {
        // pathname + search: on /alerts the whole view state (sort, columns,
        // filters) lives in the query string, so it is the precise context.
        return { url: window.location.pathname + window.location.search, title: document.title };
    }

    // Per-message trace footer: duration, tokens, tool timings, errors —
    // click to expand the raw detail. Renders under assistant messages that
    // carry a trace (every message since the agent-traces milestone).
    function appendTrace(messages, trace) {
        if (!trace) return;
        var duration = trace.duration_ms != null ? trace.duration_ms + 'ms' : null;
        var tokens = trace.tokens_in != null ? ('tokens ' + trace.tokens_in + '/' + trace.tokens_out) : null;
        var calls = trace.tool_calls || [];
        var summary = [];
        if (duration) summary.push(duration);
        if (tokens) summary.push(tokens);
        if (calls.length) summary.push('tools: ' + calls.map(function (c) { return c.name; }).join(', '));
        if (trace.error) summary.push('ERROR: ' + trace.error);
        if (!summary.length) return;
        var line = document.createElement('div');
        line.className = 'agent-trace' + (trace.error ? ' agent-trace-error' : '');
        line.textContent = 'ⓘ ' + summary.join(' · ');
        var detail = null;
        line.addEventListener('click', function () {
            if (detail) {
                detail.parentNode.removeChild(detail);
                detail = null;
                return;
            }
            detail = document.createElement('pre');
            detail.className = 'agent-trace-detail';
            detail.textContent = JSON.stringify(trace, null, 2);
            messages.insertBefore(detail, line.nextSibling);
        });
        messages.appendChild(line);
    }

    // Numbered questions in the last assistant message get a structured
    // reply widget (one input per question), so the user answers each
    // question separately and the model gets labeled answers back.
    function maybeRenderQuestions(root, text) {
        var messages = root.querySelector('#agent-messages');
        if (!messages || messages.querySelector('.agent-questions')) return;
        var questions = [];
        String(text).split('\n').forEach(function (line) {
            var match = line.match(/^\s*(\d+)[.)]\s+(.+\?)\s*$/);
            if (match) questions.push({ n: match[1], q: match[2] });
        });
        if (!questions.length) return;
        var block = document.createElement('div');
        block.className = 'agent-questions';
        var inputs = questions.map(function (question) {
            var row = document.createElement('div');
            row.className = 'agent-question-row';
            var label = document.createElement('label');
            label.className = 'agent-question-label';
            label.textContent = question.n + '. ' + question.q;
            var input = document.createElement('textarea');
            input.className = 'agent-question-input';
            input.rows = 1;
            input.setAttribute('data-question', question.n);
            row.appendChild(label);
            row.appendChild(input);
            block.appendChild(row);
            return input;
        });
        var send = document.createElement('button');
        send.type = 'button';
        send.className = 'btn-brand agent-questions-send';
        send.textContent = 'Reply';
        send.addEventListener('click', function () {
            var parts = [];
            inputs.forEach(function (input) {
                var value = input.value.trim();
                if (value) parts.push(input.getAttribute('data-question') + '. ' + value);
            });
            block.parentNode.removeChild(block);
            if (parts.length) sendText(root, parts.join('\n'));
        });
        block.appendChild(send);
        messages.appendChild(block);
        messages.scrollTop = messages.scrollHeight;
        if (inputs[0]) inputs[0].focus();
    }

    function loadHistory(root) {
        var id = sessionId();
        if (!id) return;
        var messages = root.querySelector('#agent-messages');
        fetch(root.getAttribute('data-history-url') + '/' + encodeURIComponent(id), { headers: { 'X-Requested-With': 'fetch' } })
            .then(function (response) { return response.ok ? response.json() : null; })
            .then(function (data) {
                if (!data || !data.messages) return;
                messages.textContent = '';
                data.messages.forEach(function (row) {
                    if (row.content) append(messages, row.role, row.content);
                    appendTrace(messages, row.trace);
                });
            })
            .catch(function () {});
    }

    // Past sessions for the header selector (GET /agent/sessions). The empty
    // leading option is "New chat" — selecting it clears the conversation.
    function loadSessions(root) {
        var select = root.querySelector('#agent-sessions');
        if (!select) return;
        fetch(root.getAttribute('data-sessions-url'), { headers: { 'X-Requested-With': 'fetch' } })
            .then(function (response) { return response.ok ? response.json() : null; })
            .then(function (data) {
                if (!data || !data.sessions) return;
                var current = sessionId();
                select.textContent = '';
                var fresh = document.createElement('option');
                fresh.value = '';
                fresh.textContent = root.getAttribute('data-new-chat-label') || 'New chat';
                select.appendChild(fresh);
                data.sessions.forEach(function (session) {
                    var option = document.createElement('option');
                    option.value = session.id;
                    option.textContent = session.title || session.id;
                    if (session.id === current) option.selected = true;
                    select.appendChild(option);
                });
            })
            .catch(function () {});
    }

    function newChat(root) {
        window.localStorage.removeItem(SESSION_KEY);
        root.querySelector('#agent-messages').textContent = '';
        var select = root.querySelector('#agent-sessions');
        if (select) select.value = '';
    }

    function sendMessage(root) {
        var input = root.querySelector('#agent-input');
        var text = input.value.trim();
        if (!text) return;
        input.value = '';
        sendText(root, text);
    }

    // POST /agent/chat?stream=1 and consume the SSE response with a
    // ReadableStream reader: token events drive the "Thinking… N words · Xs"
    // label, tool events render as activity lines, done carries the final
    // replies. Falls back to a plain error label on stream failure.
    function sendText(root, text) {
        var messages = root.querySelector('#agent-messages');
        var input = root.querySelector('#agent-input');
        append(messages, 'user', text);
        if (input) input.focus();
        var thinking = append(messages, 'assistant', 'Thinking…');
        var startedAt = Date.now();
        var lastEventAt = Date.now();
        var reader = null;
        var aborted = false;
        var timer = window.setInterval(function () {
            if (aborted) return;
            var idle = Math.round((Date.now() - lastEventAt) / 1000);
            var base = thinking.getAttribute('data-progress') || ('Thinking… ' + Math.round((Date.now() - startedAt) / 1000) + 's');
            if (idle > 20) {
                // no SSE data for a while: say so, and give up after 2.5 min —
                // the server-side watchdog aborts stalled streams too, but a
                // dead connection must never leave the user staring at a
                // frozen label.
                thinking.textContent = base + ' (stalled ' + idle + 's — no data from the agent)';
                if (idle > 150) {
                    aborted = true;
                    if (reader) reader.cancel();
                    window.clearInterval(timer);
                    thinking.textContent = 'agent stalled — the request was aborted; press Send to retry';
                }
            } else {
                thinking.textContent = base;
            }
        }, 1000);

        function finishTurn(data) {
            window.clearInterval(timer);
            if (thinking.parentNode) thinking.parentNode.removeChild(thinking);
            setSession(data.session_id);
            var lastText = '';
            (data.replies || []).forEach(function (reply) {
                if (reply.content) {
                    lastText = reply.content;
                    append(messages, 'assistant', reply.content);
                }
                (reply.tool_calls || []).forEach(function (call) {
                    append(messages, 'tool', '⚙ ' + call.name);
                });
                appendTrace(messages, reply.trace);
            });
            loadSessions(root);
            maybeRenderQuestions(root, lastText);
        }

        function fail(message) {
            window.clearInterval(timer);
            thinking.setAttribute('data-progress', '');
            thinking.textContent = message;
        }

        fetch(root.getAttribute('data-chat-url'), {
            method: 'POST',
            headers: { 'Content-Type': 'application/json', 'X-Requested-With': 'fetch' },
            body: JSON.stringify({ message: text, session_id: sessionId(), page_context: pageContext(), stream: true })
        }).then(function (response) {
            if (!response.ok || !response.body) throw new Error('http ' + response.status);
            reader = response.body.getReader();
            var decoder = new TextDecoder();
            var buffer = '';
            function pump() {
                return reader.read().then(function (chunk) {
                    if (chunk.done) return;
                    lastEventAt = Date.now();
                    buffer += decoder.decode(chunk.value, { stream: true });
                    var sep;
                    while ((sep = buffer.indexOf('\n\n')) >= 0) {
                        var raw = buffer.slice(0, sep);
                        buffer = buffer.slice(sep + 2);
                        var eventName = null;
                        var dataLine = null;
                        raw.split('\n').forEach(function (line) {
                            if (line.indexOf('event: ') === 0) eventName = line.slice(7);
                            else if (line.indexOf('data: ') === 0) dataLine = line.slice(6);
                        });
                        if (!eventName || !dataLine) continue;
                        var data = {};
                        try { data = JSON.parse(dataLine); } catch (e) { continue; }
                        if (eventName === 'token') {
                            thinking.setAttribute('data-progress', 'Thinking… ' + data.words + ' words · ' + Math.round(data.elapsed_ms / 1000) + 's');
                        } else if (eventName === 'round') {
                            // round start: the model is working (possibly on a
                            // slow tool round) — reset the stall clock.
                            thinking.setAttribute('data-progress', 'Thinking… (round ' + data.round + ')');
                        } else if (eventName === 'tool') {
                            append(messages, 'tool', '⚙ ' + data.name + '…');
                        } else if (eventName === 'done') {
                            finishTurn(data);
                        }
                    }
                    return pump();
                });
            }
            return pump();
        }).catch(function () { if (!aborted) fail('agent request failed — retry'); });
    }

    document.addEventListener('click', function (event) {
        var root = document.getElementById('agent-widget');
        if (!root) return;
        var target = event.target;
        if (target.closest && target.closest('#agent-send')) {
            sendMessage(root);
            return;
        }
        if (target.closest && target.closest('#agent-toggle')) {
            var panel = root.querySelector('#agent-panel');
            var opening = panel.classList.contains('d-none');
            panel.classList.toggle('d-none');
            root.querySelector('#agent-toggle').setAttribute('aria-expanded', opening ? 'true' : 'false');
            var messages = root.querySelector('#agent-messages');
            if (opening) {
                loadSessions(root);
                if (!messages.childElementCount) loadHistory(root);
                root.querySelector('#agent-input').focus();
            }
            return;
        }
        if (target.closest && target.closest('#agent-new-chat')) {
            newChat(root);
            root.querySelector('#agent-input').focus();
            return;
        }
        if (target.closest && target.closest('#agent-close')) {
            root.querySelector('#agent-panel').classList.add('d-none');
            root.querySelector('#agent-toggle').setAttribute('aria-expanded', 'false');
        }
    });

    document.addEventListener('keydown', function (event) {
        // Enter sends, Shift+Enter starts a new line (textarea default).
        if (event.key !== 'Enter' || event.shiftKey || !event.target || event.target.id !== 'agent-input') return;
        event.preventDefault();
        var root = document.getElementById('agent-widget');
        if (root) sendMessage(root);
    });

    document.addEventListener('change', function (event) {
        var select = event.target;
        if (!select || select.id !== 'agent-sessions') return;
        var root = document.getElementById('agent-widget');
        if (!root) return;
        if (select.value) {
            setSession(select.value);
            root.querySelector('#agent-messages').textContent = '';
            loadHistory(root);
        } else {
            newChat(root);
        }
    });
})();
