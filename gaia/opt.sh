
opt="opt-3.5"
optopt="-S -O3 -std-compile-opts -disable-loop-vectorization"

name=${1%.*}

${opt} ${optopt} $1 -o ${name}.opt.ll
${opt} ${optopt} ${name}.opt.ll -o ${name}.opt.ll  # optimize twice
