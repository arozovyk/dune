Test (preprocess) field on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (alias mel)
  >  (preprocess
  >   (action
  >    (run cat %{input-file}))))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @mel
  $ node _build/default/output/main.js
  hello
