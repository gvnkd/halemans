-- Provision parity: blackouts become a provisionable section, so they need
-- the same read-only-in-admin-UI flag every other provisionable table has.
ALTER TABLE blackouts ADD COLUMN IF NOT EXISTS protected BOOLEAN NOT NULL DEFAULT false;
