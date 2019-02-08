CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users (
  id            UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at    TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  name          TEXT NOT NULL UNIQUE,
  email         TEXT NOT NULL
);
