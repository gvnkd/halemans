-- Provenance for the suppressed overlay: 'blackout' = local blackout window,
-- 'source' = muted at the source (zabbix suppress action). NULL on legacy
-- rows means blackout (the only mechanism before this column existed).
ALTER TABLE alerts ADD COLUMN suppressed_by TEXT DEFAULT NULL;
UPDATE alerts SET suppressed_by = 'blackout' WHERE suppressed;
