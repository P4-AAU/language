# Test Suite

This directory contains two complementary test setups for the p4-project compiler:

- **Unit tests** (`test_p4_project.ml`) using [Alcotest](https://github.com/mirage/alcotest) — fast, isolated tests of individual compiler components.
- **End-to-end tests** (`e2e/`) using [Dune Cram](https://dune.readthedocs.io/en/stable/dune-files.html#cram-tests) — black-box tests that run the compiled binary against real source files.

---

## Running the tests

Run everything at once:

```sh
dune test
```

Run only unit tests:

```sh
dune test test/
```

Run only end-to-end tests:

```sh
dune test test/e2e
```

---

## Unit Tests — Alcotest

### What they test

Unit tests target individual compiler modules in isolation. Currently they cover `Ast` node construction and structural equality across all node kinds:

| Suite | What is tested |
|---|---|
| `Ident` | Identifier record fields |
| `Constant` | `Cbool`, `Cint32`, and other literal constructors |
| `Expr` | `Ecst`, `Ebinop`, `Eunop` expression nodes |
| `Stmt` | `Sassign`, `Sblock` statement nodes |
| `Pattern` | `Pdefault`, `Pconst` pattern nodes |

As the lexer and parser are implemented, new suites should be added here for each compiler pass (lexing, parsing, type-checking, code generation).

### File layout

```
test/
  dune                  ← declares the test executable and its dependencies
  test_p4_project.ml    ← all unit tests live here
```

### How Alcotest works

Alcotest organises tests into **suites** (named groups) each containing one or more **test cases**. A test case is a plain OCaml function that calls `Alcotest.check` to assert a value, or `Alcotest.fail` to fail unconditionally.

```ocaml
(* A single test case *)
let test_ident_name () =
  let i = make_ident "x" in
  check string "ident name" "x" i.id

(* Registering it in a suite *)
let ident_tests =
  [ test_case "name" `Quick test_ident_name ]

(* Running all suites *)
let () =
  run "p4-project"
    [ ("Ident", ident_tests) ]
```

**`check` signature:** `check testable label expected actual`

- `testable` — an Alcotest module that knows how to print and compare values (`string`, `bool`, `int`, `Alcotest.of_pp`, etc.)
- `label` — shown in the failure message
- `expected` — what you expect
- `actual` — what the code produced

**Test speed tags:**
- `` `Quick `` — always run; use for fast, in-process tests (the default for everything here)
- `` `Slow `` — skipped unless `dune test --force` or `-e` is passed; use for tests that do I/O or take more than a few milliseconds

### The `dune` stanza

```dune
(test
 (name test_p4_project)
 (libraries p4_project alcotest))
```

`(test ...)` compiles `test_p4_project.ml` into an executable and runs it as part of `dune test`. Adding a new library dependency (e.g. `alcotest-lwt` for async tests) is done here.

### Adding a new test

1. Write a test function in `test_p4_project.ml`:

   ```ocaml
   let test_my_thing () =
     let result = My_module.do_thing 42 in
     check int "my thing" 42 result
   ```

2. Add it to a suite list:

   ```ocaml
   let my_tests =
     [ test_case "my thing" `Quick test_my_thing ]
   ```

3. Register the suite in the `run` call at the bottom of the file:

   ```ocaml
   let () =
     run "p4-project"
       [ ...
       ; ("My module", my_tests) ]
   ```

### Reading test output

A passing run looks like:

```
Testing `p4-project'.
  [OK]          Ident             0   name.
  [OK]          Constant          0   bool.
  ...
Test Successful in 0.001s. 10 tests run.
```

A failing run shows a diff between expected and actual:

```
  [FAIL]        Expr              0   const.
  ── Expr 0   const. ──────────────────────────────────────────────────────────
  ASSERT expr const
  expected: 1
  but got:  2
```

---

## End-to-End Tests — Dune Cram

### What they test

E2E tests run the compiled `p4-project` binary as a black box, feeding it real source files and asserting on stdout, stderr, and exit codes. They catch regressions that unit tests cannot: argument parsing, file I/O, pipeline integration, and error reporting to the user.

### File layout

```
test/e2e/
  dune              ← cram stanza; lists the binary and input files as deps
  basic.t           ← smoke test: binary with no arguments
  valid.t           ← binary accepts well-formed source files
  invalid.t         ← binary rejects malformed source files
  valid/
    hello.p4        ← minimal assignment and print
    arithmetic.p4   ← arithmetic expression
  invalid/
    syntax_error.p4 ← token the lexer cannot recognise
    empty.p4        ← empty file
```

### How Dune Cram works

A `.t` file is a script written in a shell-like notation. Every line starting with `  $ ` (two spaces, dollar sign, space) is a command. The lines immediately following — with no `$` — are the **expected output**. Dune runs the command and diffs actual output against expected; the test fails if they differ.

```
A description of what this test does.

  $ p4-project valid/hello.p4
  Hello

  $ p4-project invalid/syntax_error.p4
  Error: unexpected token '!!!'
  [1]
```

The trailing `[1]` asserts that the process exited with code 1. If a command is expected to succeed (exit 0), omit the exit code line entirely. Any other non-zero code can be written as `[N]`.

Cram tests run inside a **sandbox directory**. Only files declared as `(deps ...)` in the `dune` file are available inside the sandbox. This is why the `dune` stanza in `e2e/` explicitly lists the binary and the input folders:

```dune
(cram
 (deps
  %{bin:p4-project}
  (glob_files valid/*.p4)
  (glob_files invalid/*.p4)))
```

`%{bin:p4-project}` resolves to the locally built binary, so cram tests always test the current build, not any system-installed version.

### Input files

**`valid/`** contains syntactically and semantically correct programs. Each file should represent one distinct language feature so that a failure points directly at the broken feature.

**`invalid/`** contains programs that the compiler should reject. Each file should represent exactly one kind of error. When the binary is fully implemented, these tests should assert a non-zero exit code and a human-readable error message on stderr.

### Updating expected output

When the binary's output changes intentionally (e.g. the error format is improved, or execution output changes as a feature is implemented), run:

```sh
dune test test/e2e --auto-promote
```

This overwrites the `.t` files with actual output. Review the diff with `git diff`, then commit. Never promote without reviewing — a promotion that silences a real regression looks identical to one that updates a legitimate change.

### Adding a new end-to-end test

**Option A — add a case to an existing `.t` file:**

Open `valid.t` or `invalid.t` and append a new block:

```
  $ p4-project valid/loops.p4
  0
  1
  2
```

**Option B — add a new input file:**

1. Create the source file in `valid/` or `invalid/`.
2. Add a test case in the corresponding `.t` file.
3. The `(glob_files valid/*.p4)` dep in `dune` picks it up automatically — no `dune` edit needed.

**Option C — add a new `.t` file for a new category:**

1. Create `e2e/my_category.t`.
2. The cram stanza in `dune` applies to all `.t` files in the directory automatically.

---

## Dependency setup

Alcotest must be present in the active opam switch. Install it with:

```sh
opam install alcotest
```

It is declared as a test-only dependency in `p4-project.opam`:

```opam
"alcotest" {with-test}
```

Dune Cram is built into Dune 3.x and requires no additional installation.
