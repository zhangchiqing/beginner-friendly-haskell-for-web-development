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
