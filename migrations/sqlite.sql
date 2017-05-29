CREATE TABLE permanodes (
sha1 TEXT PRIMARY KEY,
attributes TEXT NOT NULL DEFAULT '{}',
show_in_ui INTEGER NOT NULL DEFAULT false,
thumbnail BLOB,
preview TEXT,
search_low TEXT,
search_high TEXT,
content TEXT
);
