#!/bin/bash

set -e

psql -U "$POSTGRES_USER" --dbname "$POSTGRES_DB" -v ON_ERROR_STOP=1 <<- EOSQL
CREATE TABLE dockertest();
EOSQL
