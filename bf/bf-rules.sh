#!/bin/bash

for name in "$@"; do

cat << EOF

${name}.the_prog : progs/${name}.bf
  echo -n 'let the_prog = "' >> ${name}.the_prog
  cat ${name}.bf >> ${name}.the_prog
  echo '"' >> ${name}.the_prog

orig_${name}.ml : prim.ml ${name}.the_prog interpreter.fun
  echo 'let the_mode = "ORIGINAL"' >> orig_${name}.ml
  cat prim.ml >> orig_${name}.ml
  cat ${name}.the_prog >> orig_${name}.ml
  cat interpreter.fun >> orig_${name}.ml
  echo 'let () = main()' >> orig_${name}.ml

norm_${name}.ml : barefun.exe prim.ml shim.ml ${name}.fun
  ./barefun.exe ${name}.fun -2 -compile -mapp 1 -mlam 1 -ppu > ~/norm.ml
  cat prim.ml >> norm_${name}.ml
  cat shim.ml >> norm_${name}.ml
  echo 'let Unit =' >> norm_${name}.ml
  cat ~/norm.ml >> norm_${name}.ml

${name}.fun : ${name}.the_prog interpreter.fun
  echo 'let the_mode = "NORMALIZED"' >> ${name}.fun
  cat ${name}.the_prog >> ${name}.fun
  cat interpreter.fun >> ${name}.fun

EOF
done
