// Push service worker: shows a notification for each pushed message.
self.addEventListener('push', function (event) {
    var data = {};
    try { data = event.data ? event.data.json() : {}; } catch (e) { /* keep default */ }
    event.waitUntil(
        self.registration.showNotification(data.title || 'Halemans alert', {
            body: (data.severity || '') + (data.env ? ' in ' + data.env : ''),
            data: { url: data.url || '/' }
        })
    );
});

self.addEventListener('notificationclick', function (event) {
    event.notification.close();
    event.waitUntil(clients.openWindow(event.notification.data.url || '/'));
});
