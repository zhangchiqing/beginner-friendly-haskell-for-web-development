# A simple RESTful HTTP Service in Haskell

## Feature
- [x] Simple: NO Monad Transformer. NO Advanced type classes. NO TemplateHaskell.
- [x] Simple: Only used 3 language extensions: OverloadedStrings, DeriveGeneric, ScopedTypeVariables
- [x] Connect to PostgreSQL, supports migrations
- [x] Example of a HTTP Client to external service (httpbin.org)
- [x] RESTful API for CRUD
- [x] Error Handling: Returning 4xx for any invalid request. Returning 500 for any internal service exception.
- [x] Logging to stdout. Logging every request and respond with request id. Logging any internal exception.
- [x] Environment configured by Envvar
- [x] Unit tests and Integation tests are included
- [x] Docker Ready

## How to install
```
make install
```

## How to launch PostgreSQL
```
make db
```

## How to run a build
```
make build
```

## How to run the service
```
make run
```

check: http://127.0.0.1:3000/ping

Example logs:
```
source .env && stack exec a-simple-http-service-exe
NOTICE:  relation "schema_migrations" already exists, skipping
[2019-02-10 17:57:11.464345 UTC] [INFO] HTTP.Server Starting up service with port 3000
Setting phasers to stun... (port 3000) (ctrl-c to quit)
[2019-02-10 17:57:17.863043 UTC] [INFO] [RequestId] 951b69d9-0139-406b-ba3b-9131da6b2076 GET /ping
[2019-02-10 17:57:17.863875 UTC] [INFO] [Response] 951b69d9-0139-406b-ba3b-9131da6b2076 ResponseCode: 200
[2019-02-10 17:57:24.233988 UTC] [INFO] [RequestId] 9d9c968b-e18c-4013-83c1-e70d8fa9ee83 POST /users
[2019-02-10 17:57:24.246 UTC] [INFO] [Response] 9d9c968b-e18c-4013-83c1-e70d8fa9ee83 ResponseCode: 200
[2019-02-10 17:57:30.271769 UTC] [INFO] [RequestId] 9c5f060b-1007-472a-a9e5-6228fc14f16b GET /user
[2019-02-10 17:57:30.272585 UTC] [INFO] [Response] 9c5f060b-1007-472a-a9e5-6228fc14f16b ResponseCode: 400
[2019-02-10 17:57:37.892525 UTC] [INFO] [RequestId] 6812c2ca-fe9d-4dfb-8fc3-829ac497f71e GET /user
[2019-02-10 17:57:37.901869 UTC] [INFO] [Response] 6812c2ca-fe9d-4dfb-8fc3-829ac497f71e ResponseCode: 200
[2019-02-10 17:57:45.059709 UTC] [INFO] [RequestId] 3b73f91f-f8c7-4dcb-b3a4-905a0afe4471 DELETE /user/alice
[2019-02-10 17:57:45.064397 UTC] [INFO] [Response] 3b73f91f-f8c7-4dcb-b3a4-905a0afe4471 ResponseCode: 204
[2019-02-10 17:57:56.571939 UTC] [INFO] [RequestId] cbc44b95-ef39-402b-b6f5-280ccf73d6de GET /users
[2019-02-10 17:57:56.572778 UTC] [INFO] [Response] cbc44b95-ef39-402b-b6f5-280ccf73d6de ResponseCode: 404
```

## How to run the tests
```
make db_test
make test
```

## How to make docker build
### 1. Build a builder image as a building environment, which includes stack for compiling and libpq for PostgreSQL
```
make builder-image
```
It builds a `zhangchiqing/stack-libpq` image as the builder.

#### Why do we need a builder image?
Since we don't need the compiler on production, it's a different image than the final deployed image so that it's just for building the executable binary.

And since we want to run the application on Linux, the code has to be compiled under linux

### 2. Build a builder image, which is the image for compiling the code on Linux
```
make builder-image
```
This compiles to executable binaries for Linux and place it in the mounted volume at `a-simple-http-service/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/bin`.

The initial build takes a long time, but since we cache the build (the `~/.stack` folder). The second time to build takes only seconds if there is no change. Thanks to stack!

### 3. Build a deployment base image
```
make deployment-base-image
```
It builds a `zhangchiqing/pid1-libpq` image as the base image for deployment.

### 4. Build a deployment image
```
make deployment-image
```
It builds a `zhangchiqing/a-simple-http-service` image

## How to run as docker container
```
make run-docker
```

check: http://127.0.0.1:3002/ping
