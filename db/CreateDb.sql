
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS "user" (
  id uuid primary key DEFAULT uuid_generate_v4(),
  login text not null unique,
  name text,
  surname text,
  level text,
  password text not null,
  timestamp timestamp DEFAULT current_timestamp
);

CREATE UNIQUE INDEX "user_login" ON "user"(login) ;

CREATE TABLE IF NOT EXISTS "slot" (
  id uuid primary key DEFAULT uuid_generate_v4(),
  user_id uuid references "user"(id),
  day text not null,
  slot_start_hours int not null,
  slot_start_minutes int not null,
  slot_start_value int not null,
  slot_end_hours int not null,
  slot_end_minutes int not null,
  slot_end_value int not null,
  CONSTRAINT valid_times CHECK (slot_end_value > slot_start_value),
  UNIQUE (user_id, day, slot_start_value, slot_end_value)
);
