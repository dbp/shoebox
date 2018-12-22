CREATE TABLE items (
blob_ref TEXT PRIMARY KEY,
thumbnail BLOB,
medium BLOB,
preview TEXT,
search_low TEXT,
search_high TEXT,
show_in_root INT
);

CREATE TABLE redirs (
  id INTEGER PRIMARY KEY,
  src TEXT,
  target TEXT
);

CREATE TABLE urls (
  id INTEGER PRIMARY KEY,
  url TEXT,
  ref TEXT,
  url_blob_ref TEXT
);

CREATE TABLE notes (
  id INTEGER PRIMARY KEY,
  content TEXT,
  ref TEXT,
  note_blob_ref TEXT
);
