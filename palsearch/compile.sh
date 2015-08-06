if [ -e /home/jules/z3/lib ]; then
  Z3=/home/jules/z3/lib
elif [ -e /home/jules/stuff/z3/prefix/lib ]; then
  Z3=/home/jules/stuff/z3/prefix/lib
else
  echo "No Z3 directory found."
  exit 1
fi
ocamlfind ocamlopt -annot -package Z3,graphics,camlimages.all_formats,bytes -linkpkg -ccopt "-L$Z3 -Wl,-rpath,$Z3" palsearch.ml -o palsearch
