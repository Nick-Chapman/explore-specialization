#!/bin/bash

for name in "$@"; do

cat << EOF

${name}.the_prog : progs/${name}.bf
  echo -n 'let the_prog = "' >> \$@
  cat ${name}.bf >> \$@
  echo '"' >> \$@

orig_${name}.ml : prim.ml ${name}.the_prog interpreter.fun
  echo 'let the_mode = "ORIGINAL"' >> \$@
  cat prim.ml >> \$@
  cat ${name}.the_prog >> \$@
  cat interpreter.fun >> \$@
  echo 'let () = main()' >> \$@

norm_${name}.ml : barefun.exe prim.ml shim.ml ${name}.fun
  ./barefun.exe ${name}.fun -2 -compile -mapp 1 -mlam 1 > ~/norm.ml
  cat prim.ml >> \$@
  cat shim.ml >> \$@
  echo 'let Unit =' >> \$@
  cat ~/norm.ml >> \$@

${name}.fun : ${name}.the_prog interpreter.fun
  echo 'let the_mode = "NORMALIZED"' >> \$@
  cat ${name}.the_prog >> \$@
  cat interpreter.fun >> \$@

EOF
done
