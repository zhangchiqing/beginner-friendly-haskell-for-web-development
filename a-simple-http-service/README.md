# A simple RESTful HTTP Service in Haskell

## Feature
- [x] Simple: NO Monad Transformer. NO fancy type classes.
- [x] Simple: Only used 3 language extensions: OverloadedStrings, DeriveGeneric, ScopedTypeVariables
- [x] Connect to PostgreSQL, supports migrations
- [x] Example of a HTTP Client to external service (httpbin.org)
- [x] RESTful API for CRUD
- [x] Error Handling: Returning 4xx for any invalid request. Returning 500 for any internal service exception.
- [x] Logging to stdout. Logging every request and respond with request id. Logging any internal exception.
- [x] Environment configured by Envvar
- [x] Unit tests and Integation tests are included
- [ ] Docker Ready


