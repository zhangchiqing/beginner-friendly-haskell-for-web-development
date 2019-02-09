.PHONY: install test

install:
	echo "install haskell and GHC via Homebrew: brew cask install haskell-for-mac"

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

