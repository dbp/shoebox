CREATE TABLE items (
blob_ref TEXT PRIMARY KEY,
thumbnail BLOB,
preview TEXT,
search_low TEXT,
search_high TEXT
);

CREATE TABLE redirs (
  id INTEGER PRIMARY KEY,
  src TEXT,
  target TEXT
);
