.PHONY: build run test shell clean

build:
	docker compose build

run:
	docker compose run --rm p4

test:
	docker compose run --rm p4 opam exec -- dune test -f

shell:
	docker compose run --rm p4 bash

fmt:
	docker compose run --rm p4 opam exec -- dune fmt

clean:
	docker compose down --rmi all
	rm -rf _build