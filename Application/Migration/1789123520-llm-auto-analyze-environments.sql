-- Auto-analysis gate env scope (milestone 10 §5 extension): a non-empty
-- environments list restricts automatic LLM analysis to alerts whose
-- EFFECTIVE env (env facet override wins) is in the list; empty = all envs.
ALTER TABLE llm_auto_analyze_configs ADD COLUMN environments JSONB NOT NULL DEFAULT '[]';
