print_model () {
    model=$1
    echo "Printing model $1"
    td=$2
    ref=$(git rev-parse --short HEAD)
    build/tpplc-graph $model --output build/temp --print-model > $td/$(basename $1 .tppl).$ref.mc
}

model_dir=models/host-repertoire-evolution
target_dir=../miking-dppl
print_model $model_dir/flat-root-prior-HRM.tppl $target_dir/flat-root-prior-HRM
print_model $model_dir/subroot-HRM.tppl $target_dir/subroot-HRM
