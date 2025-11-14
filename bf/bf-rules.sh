#!/bin/bash

for name in "$@"; do

out='$@' # prevent $@ from being replaced here; allowing jenga to replace it when interpreting the rule commands

cat << EOF

${name}.the_prog : progs/${name}.bf
  echo -n 'let the_prog = "' >> $out
  cat ${name}.bf >> $out
  echo '"' >> $out

orig_${name}.ml : prim.ml ${name}.the_prog interpreter.fun
  echo 'let the_mode = "ORIGINAL"' >> $out
  cat prim.ml >> $out
  cat ${name}.the_prog >> $out
  cat interpreter.fun >> $out
  echo 'let () = main()' >> $out

norm_${name}.ml : barefun.exe prim.ml shim.ml ${name}.fun
  ./barefun.exe ${name}.fun -2 -compile -mapp 1 -mlam 1 -ppu > ~/norm.ml
  cat prim.ml >> $out
  cat shim.ml >> $out
  echo 'let Unit =' >> $out
  cat ~/norm.ml >> $out

${name}.fun : ${name}.the_prog interpreter.fun
  echo 'let the_mode = "NORMALIZED"' >> $out
  cat ${name}.the_prog >> $out
  cat interpreter.fun >> $out

EOF
done
