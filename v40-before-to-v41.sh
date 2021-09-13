
cp arb_os/arbos_before.mexe.bkp arb_os/arbos_before.mexe

sed -i 's/"Func":\[true,/"Func":\[{"view":true,"write":true,"closure":false},/g' arb_os/arbos_before.mexe
sed -i 's/"Func":\[false,/"Func":\[{"view":false,"write":false,"closure":false},/g' arb_os/arbos_before.mexe

cat arb_os/arbos_before.mexe | jq . | grep -i -b2 -a12 --color=always func
