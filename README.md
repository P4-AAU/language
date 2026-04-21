# P4 Compiler
A compiler for AUTOBounds, written in OCaml. Using menhir for parsing and ocamllex for lexing.


## Prerequisites

- Docker Desktop

## Getting started
```bash
git clone https://github.com/P4-AAU/language.git
cd language
make build
make run
```

## Commands

| Command | Description |
|---|---|
| `make build` | Build the Docker image |
| `make run` | Run the compiler | //currently doesnt work as intended
| `make test` | Run tests |
| `make shell` | Open an interactive shell |
| `make clean` | Remove Docker image and build artifacts |


## Troubleshooting
| Command | Description |
|---|---|
| `menhir --explain "FILEPATH"` | Verbose logging of insufficiently defined grammar |


## Contributing

All changes must be submitted via pull request to `main`. CI must pass before merging.
