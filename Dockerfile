FROM ocaml/opam:ubuntu-24.04-ocaml-5.4

WORKDIR /app

COPY p4-project.opam .
RUN opam install . --deps-only --with-test -y
RUN opam install ocamlformat ocaml-lsp-server -y

COPY . .

RUN sudo chown -R opam:opam /app

RUN opam exec -- dune build

CMD ["opam", "exec", "--", "dune", "exec", "bin/main.exe"]