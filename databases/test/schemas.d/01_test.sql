
CREATE EXTENSION pgcrypto;

CREATE SEQUENCE users_id_seq;

CREATE TABLE users (
  id INTEGER PRIMARY KEY DEFAULT NEXTVAL('users_id_seq'),
  password TEXT NOT NULL,
  email TEXT NOT NULL
);

CREATE SEQUENCE messages_id_seq;

CREATE TABLE messages (
  id INTEGER PRIMARY KEY DEFAULT NEXTVAL('messages_id_seq'),
  target INTEGER NOT NULL REFERENCES users(id),
  time TIMESTAMP NOT NULL DEFAULT NOW(),
  message TEXT NOT NULL
);

CREATE INDEX ON messages (target);

