CREATE TABLE permanodes (
  sha224 TEXT PRIMARY KEY,
  attributes JSONB NOT NULL DEFAULT '{}'::jsonb,
  show_in_ui BOOLEAN NOT NULL DEFAULT false
);
