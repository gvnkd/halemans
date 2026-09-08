-- Server-side cache for Jira Assets icon/avatar images. The card renders
-- <img> against the app (Web.Controller.AssetsIcons) instead of the Jira
-- origin; rows are filled lazily on first request and keyed by (config, url).

CREATE TABLE assets_icon_cache (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    config_id UUID NOT NULL,
    url TEXT NOT NULL,
    content_type TEXT NOT NULL DEFAULT 'image/png',
    body BYTEA NOT NULL,
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE assets_icon_cache ADD CONSTRAINT assets_icon_cache_config_id_fkey FOREIGN KEY (config_id) REFERENCES assets_configs (id);
CREATE UNIQUE INDEX assets_icon_cache_config_url_idx ON assets_icon_cache(config_id, url);
