echo "CREATE ROLE \"climbing-scheduler\" WITH LOGIN ENCRYPTED PASSWORD 'climbing-scheduler' CREATEDB;CREATE DATABASE \"climbing-scheduler\" WITH OWNER \"climbing-scheduler\" TEMPLATE template0 ENCODING 'UTF8';GRANT ALL PRIVILEGES ON DATABASE \"climbing-scheduler\" TO \"climbing-scheduler\"; ALTER USER climbing-scheduler WITH SUPERUSER;" | docker run \
  --rm \
  --interactive \
  --link postgres:postgres \
  postgres:latest \
  bash -c 'exec psql -h "$POSTGRES_PORT_5432_TCP_ADDR" -p "$POSTGRES_PORT_5432_TCP_PORT" -U postgres'
