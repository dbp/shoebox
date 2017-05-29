ALTER TABLE permanodes ADD COLUMN search_low text;
ALTER TABLE permanodes ADD COLUMN search_high text;

-- NOTE(dbp 2017-05-29): Workaround via http://rachbelaid.com/postgres-full-text-search-is-good-enough/
CREATE OR REPLACE FUNCTION gin_fts_fct(title text, content text)
RETURNS tsvector
AS
$BODY$
SELECT setweight(to_tsvector($1), 'A') || setweight(to_tsvector($1), 'B');
$BODY$
LANGUAGE sql
IMMUTABLE;

CREATE INDEX idx_fts_permanodes ON permanodes  USING gin(gin_fts_fct(search_high, search_low));
