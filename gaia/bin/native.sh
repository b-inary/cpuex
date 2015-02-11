#!/bin/sh

opt='opt-3.5'
optopt='-S -O3 -std-compile-opts'
llc='llc-3.5'
clang='clang-3.5'

datalayout='e-m:e-i64:64-f80:128-n8:16:32:64-S128'
triple='x86_64-pc-linux-gnu'

name=${1%.*}
dir=`dirname $0`

$llc $dir/lib/llvmlib.ll -o llvmlib.s &&
sed -e "s/^target datalayout.*$/target datalayout = \"${datalayout}\"\ntarget triple = \"${triple}\"/" $1 > $name.native.ll &&
$opt $optopt $name.native.ll -o $name.native.ll &&
$opt $optopt $name.native.ll -o $name.native.ll &&  # optimize twice
$llc $name.native.ll &&
$clang $name.native.s llvmlib.s -o $name.out -lm
