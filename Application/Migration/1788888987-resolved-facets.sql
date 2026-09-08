ALTER TABLE alerts ADD COLUMN facets JSONB NOT NULL DEFAULT '{}';
CREATE INDEX alerts_facets_idx ON alerts USING GIN (facets jsonb_path_ops);

CREATE TABLE field_mappings (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    facet TEXT NOT NULL,
    rank INT NOT NULL,
    kind TEXT NOT NULL,
    key TEXT NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (facet, rank)
);

CREATE TABLE facet_backfill_jobs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    cursor UUID DEFAULT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    status JOB_STATUS DEFAULT 'job_status_not_started' NOT NULL,
    last_error TEXT DEFAULT NULL,
    attempts_count INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    locked_by UUID DEFAULT NULL,
    run_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
