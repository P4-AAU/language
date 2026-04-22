.PHONY: build run test shell clean

build:
	docker compose build

run:
	docker compose run --rm p4 opam exec -- dune exec bin/main.exe -- test.mylang

execute:
	docker compose run --rm p4 opam exec -- dune exec bin/executor.exe -- test.mylang

test:
	docker compose run --rm p4 opam exec -- dune exec test/test.exe

shell:
	docker compose run --rm p4 bash

fmt:
	docker compose run --rm p4 opam exec -- dune fmt

clean:
	docker compose down --rmi all
	rm -rf _build