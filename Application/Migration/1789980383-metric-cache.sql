CREATE TABLE metric_cache (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    source_id UUID NOT NULL,
    series_key TEXT NOT NULL,
    bucket_start TIMESTAMP WITH TIME ZONE NOT NULL,
    points JSONB NOT NULL,
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE metric_cache ADD CONSTRAINT metric_cache_source_id_fkey FOREIGN KEY (source_id) REFERENCES sources (id);
CREATE UNIQUE INDEX metric_cache_key_idx ON metric_cache(source_id, series_key, bucket_start);
CREATE INDEX metric_cache_fetched_at_idx ON metric_cache(fetched_at);
