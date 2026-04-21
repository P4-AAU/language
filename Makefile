.PHONY: build run test shell clean

build:
	docker compose build

run:
	docker compose run --rm p4 opam exec -- dune exec bin/main.exe -- test.mylang

test:
	docker compose run --rm p4 opam exec -- dune test

shell:
	docker compose run --rm p4 bash

fmt:
	docker compose run --rm p4 opam exec -- dune fmt

clean:
	docker compose down --rmi all
	rm -rf _build