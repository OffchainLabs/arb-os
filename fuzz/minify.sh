
COUNT=0
for file in ~/Documents/arb-os/fuzz/ready/*; do
    echo "Doing $COUNT"
    ((COUNT += 1))
    
    input=$(cargo fuzz tmin optimizer $file -D 2>&1)
    input=$(echo $input | grep "Test case minimization failed")
    
    if [[ -z "$input" ]]; then
        cargo fuzz tmin optimizer $file -D
        exit 0
    fi
    rm $file
done

for file in ~/Documents/arb-os/fuzz/hfuzz_workspace/hfuzz-optimizer/SIG*; do
    num=$(ls ready | wc -l)
    cp $file "ready/minify-$num"
done

for file in ~/Documents/arb-os/fuzz/ready/*; do
    echo "Doing $COUNT"
    ((COUNT += 1))
    
    input=$(cargo fuzz tmin optimizer $file -D 2>&1)
    input=$(echo $input | grep "Test case minimization failed")
    
    if [[ -z "$input" ]]; then
        cargo fuzz tmin optimizer $file -D
        exit 0
    fi
    rm $file
done

rm ~/Documents/arb-os/hfuzz_workspace/hfuzz-optimizer/SIG*
