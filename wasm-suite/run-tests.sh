#!/bin/bash

for i in wasm-suite/*.json
do
  cargo run --release -- wasm-suite $i
done

