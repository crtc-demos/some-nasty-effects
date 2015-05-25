ocamlfind ocamlopt -package Z3,graphics,camlimages.all_formats,bytes -linkpkg -ccopt "-L/home/jules/z3/lib -Wl,-rpath,/home/jules/z3/lib" palsearch.ml -o palsearch
