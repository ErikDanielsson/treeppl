model_dir=models/host-repertoire-evolution
lib_dir=$model_dir/host-rep-lib
print_model () {
    model=$1
    echo "Printing model $1"
    td=$2
    ref=$(git rev-parse --short $(git rev-list -1 HEAD -- $model $lib_dir))
    target=$td/$(basename $1 .tppl).$ref.mc
    if [ -f $target ]; then
        echo "Target $target exists, skipping"
    else
        echo "Target $target does not exists, compiling"
        build/tpplc-graph $model --output build/temp --print-model > $target
    fi
}

target_dir=../miking-dppl
print_model $model_dir/flat-root-prior-HRM.tppl $target_dir/flat-root-prior-HRM
print_model $model_dir/subroot-HRM.tppl $target_dir/subroot-HRM
