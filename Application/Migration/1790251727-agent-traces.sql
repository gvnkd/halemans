-- Agent turn traces (agent observability milestone): every assistant row
-- carries a trace of what happened in that round — duration, LLM token
-- counts, executed tool calls with their own timings and result excerpts,
-- and error states — so a stalled or misbehaving turn can be analyzed
-- after the fact (and the agent can read its own trace via the
-- explain_last_turn tool).
ALTER TABLE agent_messages ADD COLUMN trace JSONB;
