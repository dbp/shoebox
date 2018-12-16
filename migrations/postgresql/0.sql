CREATE TABLE items (
  blob_ref TEXT PRIMARY KEY,
  thumbnail BYTEA,
  preview TEXT,
  search_low TEXT,
  search_high TEXT,
  show_in_root BOOLEAN NOT NULL DEFAULT false
);

-- NOTE(dbp 2017-05-29): Workaround via http://rachbelaid.com/postgres-full-text-search-is-good-enough/
CREATE OR REPLACE FUNCTION gin_fts_fct(title text, content text)
  RETURNS tsvector
AS
  $BODY$
  SELECT setweight(to_tsvector($1), 'A') || setweight(to_tsvector($1), 'B');
  $BODY$
    LANGUAGE sql
    IMMUTABLE;

  CREATE INDEX idx_fts_items ON items  USING gin(gin_fts_fct(search_high, search_low));


CREATE TABLE redirs (
  id SERIAL,
  src TEXT NOT NULL,
  target TEXT NOT NULL
);

