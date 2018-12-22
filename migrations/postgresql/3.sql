CREATE TABLE notes (
  id SERIAL,
  content TEXT NOT NULL,
  ref TEXT NOT NULL,
  note_blob_ref TEXT NOT NULL
);
