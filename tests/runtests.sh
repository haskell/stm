#!/bin/bash

# Simple runner script used for TravisCI (where GHC's testsuite runner isn't available)

set -e

die () {
    echo "ERROR: $1" >&2;
    exit 1;
}

[ -f tests/runtests.sh ] && cd tests/

[ -f runtests.sh ] || die "must be called from inside tests folder"

for T in *.hs;do
    T=${T%.hs}

    echo "== running test '$T'"

    ghc --make -threaded -O2 --make ${T}.hs

    if ./${T} > ${T}.stdout.run 2> ${T}.stderr.run
    then
        echo "${T} exited with code $?"
    fi

    for FD in stdout stderr; do
        if [ -f "${T}.${FD}.ignore" ]; then
            echo "ignoring ${FD} output"
            continue
        fi
        echo "validate ${FD} output..."
        if [ -f "${T}.${FD}" ]; then REF="${T}.${FD}"; else REF=/dev/null; fi
        diff -w -u ${REF} ${T}.${FD}.run
    done

    echo "> '${T}' PASSED"

    rm ${T}.hi ${T}.o ${T} ${T}.stdout.run ${T}.stderr.run
done

echo "----------------------------------------------------------------------------"
echo "all tests PASSED!"
