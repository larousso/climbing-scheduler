
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS "user" (
  id uuid primary key DEFAULT uuid_generate_v4(),
  login text not null unique,
  password text not null,
  timestamp timestamp DEFAULT current_timestamp
);
