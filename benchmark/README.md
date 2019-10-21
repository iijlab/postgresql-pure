# postgresql-benchmarck

## Libraries

- postgresql-libpq
  - https://hackage.haskell.org/package/postgresql-libpq
  - low-level binding to libpq https://github.com/phadej/postgresql-libpq

- postgresql-simple
  - https://hackage.haskell.org/package/postgresql-simple
  - Mid-Level PostgreSQL client library
  - depends on *postgresql-libpq*

- HDBC-postgresql
  - http://hackage.haskell.org/package/HDBC-postgresql
  - PostgreSQL driver for HDBC
  - uses libpq directly

- postgresql-typed
  - https://hackage.haskell.org/package/postgresql-typed
  - PostgreSQL interface with compile-time SQL type checking, optional HDBC backend
  - no libpq

- hasql
  - https://hackage.haskell.org/package/hasql
  - An efficient PostgreSQL driver and a flexible mapping API
  - depends on *postgresql-libpq*

- postgres-wire
  - https://github.com/postgres-haskell/postgres-wire
  - not yet released
  - no libpq
