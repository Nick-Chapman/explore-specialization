#!/bin/bash

for name in "$@"; do

cat << EOF

orig_${name}.ml : prim.ml progs/${name}.bf interpreter.fun
  echo 'let the_mode = "ORIGINAL"' >> orig_${name}.ml
  cat prim.ml >> orig_${name}.ml
  echo -n 'let the_prog = "' >> orig_${name}.ml
  cat ${name}.bf >> orig_${name}.ml
  echo '"' >> orig_${name}.ml
  cat interpreter.fun >> orig_${name}.ml
  echo 'let () = main()' >> orig_${name}.ml

norm_${name}.ml : prim.ml shim.ml ${name}.fun
  barefun.exe ${name}.fun -2 -compile -mapp 1 -mlam 1 -ppu > ~/norm.ml
  cat prim.ml >> norm_${name}.ml
  cat shim.ml >> norm_${name}.ml
  echo 'let Unit =' >> norm_${name}.ml
  cat ~/norm.ml >> norm_${name}.ml

${name}.fun : progs/${name}.bf interpreter.fun
  echo 'let the_mode = "NORMALIZED"' >> ${name}.fun
  echo -n 'let the_prog = "' >> ${name}.fun
  cat ${name}.bf >> ${name}.fun
  echo '"' >> ${name}.fun
  cat interpreter.fun >> ${name}.fun

EOF
done
