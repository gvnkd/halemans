-- Milestone 8 (enrichment phase 0) schema delta, per design_docs/milestone_8.md §2.

CREATE TABLE assets_configs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    base_url TEXT NOT NULL,
    token_env TEXT NOT NULL,
    auth_mode TEXT NOT NULL DEFAULT 'bearer',
    jira_email_env TEXT DEFAULT NULL,
    default_schema_name TEXT NOT NULL DEFAULT '',
    host_query_template TEXT NOT NULL DEFAULT '',
    attribute_names TEXT NOT NULL DEFAULT 'Owner,Cluster,Database,IP,Datacenter',
    enabled BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX assets_configs_name_idx ON assets_configs(name);

CREATE TABLE assets_objects (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    config_id UUID NOT NULL,
    object_id BIGINT NOT NULL,
    object_key TEXT NOT NULL DEFAULT '',
    label TEXT NOT NULL DEFAULT '',
    object_type_name TEXT NOT NULL DEFAULT '',
    attributes JSONB NOT NULL DEFAULT '{}',
    icon_url TEXT NOT NULL DEFAULT '',
    source_url TEXT NOT NULL DEFAULT '',
    fetched_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    UNIQUE (config_id, object_id)
);
ALTER TABLE assets_objects ADD CONSTRAINT assets_objects_config_id_fkey FOREIGN KEY (config_id) REFERENCES assets_configs (id);

CREATE TABLE asset_alert_links (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    alert_id UUID NOT NULL,
    assets_object_id UUID DEFAULT NULL,
    matched_by TEXT NOT NULL DEFAULT '',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE asset_alert_links ADD CONSTRAINT asset_alert_links_alert_id_fkey FOREIGN KEY (alert_id) REFERENCES alerts (id);
ALTER TABLE asset_alert_links ADD CONSTRAINT asset_alert_links_assets_object_id_fkey FOREIGN KEY (assets_object_id) REFERENCES assets_objects (id);
CREATE UNIQUE INDEX asset_alert_links_pair_idx ON asset_alert_links(alert_id, assets_object_id) WHERE assets_object_id IS NOT NULL;
CREATE INDEX asset_alert_links_alert_idx ON asset_alert_links(alert_id);

CREATE TABLE llm_agent_roles (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    description TEXT NOT NULL DEFAULT '',
    prompt_template_name TEXT NOT NULL DEFAULT 'alert_enrichment',
    tools JSONB NOT NULL DEFAULT '[]',
    enabled BOOLEAN NOT NULL DEFAULT true,
    is_default BOOLEAN NOT NULL DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE UNIQUE INDEX llm_agent_roles_name_idx ON llm_agent_roles(name);
CREATE UNIQUE INDEX llm_agent_roles_default_idx ON llm_agent_roles(is_default) WHERE is_default;

ALTER TABLE llm_analyses ADD COLUMN agent_role_id UUID DEFAULT NULL;
ALTER TABLE llm_analyses ADD CONSTRAINT llm_analyses_agent_role_id_fkey FOREIGN KEY (agent_role_id) REFERENCES llm_agent_roles (id);
