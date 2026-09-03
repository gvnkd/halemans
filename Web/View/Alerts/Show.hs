module Web.View.Alerts.Show where
import Web.View.Prelude

data ShowView = ShowView { alert :: Alert }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <div data-testid="alert-card">
            <h1 data-testid="alert-title">{alert.title}</h1>
            <p>
                <span class="badge bg-secondary" data-testid="alert-status">{alert.status}</span>
                <span class="badge bg-secondary" data-testid="alert-severity">{alert.severity}</span>
            </p>
            <dl>
                <dt>Fingerprint</dt><dd>{alert.fingerprint}</dd>
                <dt>Env</dt><dd>{fromMaybe "-" alert.env}</dd>
                <dt>Host</dt><dd>{fromMaybe "-" alert.host}</dd>
                <dt>Check</dt><dd>{fromMaybe "-" alert.checkName}</dd>
                <dt>Occurrences</dt><dd>{alert.occurrences}</dd>
                <dt>Started at</dt><dd>{show alert.startedAt}</dd>
                <dt>Last seen</dt><dd>{show alert.lastSeenAt}</dd>
                <dt>Resolved at</dt><dd>{show alert.resolvedAt}</dd>
            </dl>
            <h2>Description</h2>
            <p>{alert.description}</p>
        </div>
    |]
