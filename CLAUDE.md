# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test

All development runs inside Docker via `make`:

```bash
make build   # docker compose build (needed after Dockerfile/opam changes)
make test    # run test suite in container
make run     # run compiler on test.mylang in container
make fmt     # format code via container
make shell   # open a bash shell in the container
```

To run a single test or iterate quickly without Docker, you can use dune directly if OCaml/opam is available locally:

```bash
dune build
dune test
dune exec bin/main.exe -- test.mylang
```

The `docker-compose.yml` mounts `.:/app`, so local edits are reflected inside the container without rebuilding the image.

## Architecture

The compiler pipeline is: `.mll` → `.mly` → `ast.ml` → (future: `interp.ml` / `compile.ml`)

### Current pipeline (parse only)

```
lib/lexer.mll   → tokens (ocamllex)
lib/parser.mly  → AST (menhir)
lib/ast.ml      → AST type definitions
bin/main.ml     → reads a file, runs parser, prints "Parsing Successful"
```

The compiler currently only parses — it does not yet evaluate or compile. `bin/main.ml` calls `Parser.file Lexer.token lexbuf`, prints success, and ignores the resulting AST.

### Language syntax

The language uses Python-like syntax with curly-brace blocks:

- Statements: assignment (`x = expr`), `if expr { block } else { block }`, `while expr: stmt`, `print(args...)`
- Note: `if` uses curly braces `{ }` for its block but `while` uses a colon `:` — this inconsistency is reflected in the parser
- Expressions: integer/bool literals, identifiers, arithmetic (`+ - * / %`), comparison (`== != < <= > >=`), logical (`and or not`), unary minus
- Comments: `#` to end of line
- No string type, no function definitions yet

### AST key types (`lib/ast.ml`)

- `file = stmt` — a program is a single statement (usually `Sblock`)
- `stmt` — `Sif`, `Sassign`, `Sblock`, `Sprint`, `Swhile`
- `expr` — `Ecst`, `Eident`, `Eunop`, `Ebinop`
- `ident` carries a `loc` (source position) alongside the `id` string — use this for error reporting

### Tests (`test/test_p4_project.ml`)

Tests call `parse str` directly and check whether it succeeds or raises `Parser.Error`. Add cases as `test "name" "source string" should_pass`. The test binary is run by `dune test` / `make test`.

### Source file extension

`.mylang` is the source file extension for this language (see `test.mylang`).
