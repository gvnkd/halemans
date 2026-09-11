-- Milestone 10: DB-resident Jira/Confluence integration configs with
-- multiple search scopes (projects/spaces JSONB string arrays; empty = no
-- scope clause). token_env holds the env var NAME, never the token.
CREATE TABLE jira_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL,
    token_env TEXT NOT NULL,
    api_version TEXT NOT NULL DEFAULT '3',
    projects JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX jira_configs_name_idx ON jira_configs(name);

CREATE TABLE cmdb_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL,
    token_env TEXT NOT NULL,
    spaces JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX cmdb_configs_name_idx ON cmdb_configs(name);
