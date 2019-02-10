.PHONY: install test

install:
	cat howtoinstall.md

db:
	docker-compose up -d db

db_test:
	docker-compose up -d db_test

# depends on install
build:
	stack build

# depends on db, build
run:
	source .env && stack exec a-simple-http-service-exe

# depends on db_test
test:
	source .env_test && stack test

builder-image:
	docker build --tag zhangchiqing/stack-libpq docker/stack-libpq/

# depends on builder-base-image
docker-build:
	docker run --rm -v $(shell pwd):/opt/project/a-simple-http-service -w /opt/project/a-simple-http-service -v $(HOME)/.stack:/root/.stack zhangchiqing/stack-libpq stack build --allow-different-user

deployment-base-image:
	docker build --tag zhangchiqing/pid1-libpq docker/pid1-libpq/

# deployment image
deployment-image:
	docker build --tag zhangchiqing/a-simple-http-service .

# depends on db
run-docker:
	docker-compose up -d api
