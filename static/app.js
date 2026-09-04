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
