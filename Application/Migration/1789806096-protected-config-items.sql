-- Per-item protection flag for provision-managed configuration. Items whose
-- `protected` flag is true were provisioned from HALEMANS_PROVISION_CONFIG
-- (default for every item present in the file, opt-out via "protected":
-- false) and are read-only in the admin UI: edits/deletes are rejected and
-- the item is badged as protected. A present section whose key no longer
-- appears in the file clears the flag (item left the provisioned set);
-- global strict deletes the row outright.

ALTER TABLE users ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE sources ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE teams ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE roles ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE llm_configs ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE llm_prompt_templates ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE field_mappings ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE dashboards ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE jira_configs ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE cmdb_configs ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE assets_configs ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE grouping_rules ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE notification_rules ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE escalation_policies ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
ALTER TABLE llm_agent_roles ADD COLUMN protected BOOLEAN NOT NULL DEFAULT false;
