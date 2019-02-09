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
	source .envrc && stack exec a-simple-http-service-exe

# depends on db_test
test:
	source .envrc.test && stack test

