#!/bin/sh

# do_tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

usage () {
    echo "$0 [lisp invocation] [scripts-regex]"
    echo " - read lisp forms one at a time from matching scripts"
    echo " - quit with exit status 0 on getting eof"
    echo " - quit with exit status >0 if an unhandled error occurs"
    echo " you need to supply the .script in the second argument"
    echo " lisps include sbcl, clisp, allegro and allegromodern"
    echo "OPTIONS:"
    echo "    -d -- debug mode"
    echo "    -u -h -- show this message."
}

unset DEBUG_ASDF_TEST

while getopts "duh" OPTION
do
    case $OPTION in
        d)
            export DEBUG_ASDF_TEST=t
            ;;
        u)
            usage
            exit 1
            ;;
        h)
            usage
            exit 1
            ;;
    esac
done
shift $(($OPTIND - 1))

if [ x"$1" = "xhelp" ]; then
    usage
    exit 1
fi

if [ -z "$2" ]; then
    scripts="*.script"
else
    scripts="$2"
fi

sok=1

DO () { ( set -x ; "$@" ); }

do_tests() {
  command="$1" eval="$2"
  rm -f ~/.cache/common-lisp/"`pwd`"/* || true
  ( cd .. && DO $command $eval '(load "test/compile-asdf.lisp")' )
  if [ $? -ne 0 ] ; then
    echo "Compilation FAILED" >&2
  else
    echo "Compiled OK" >&2
    test_count=0
    test_pass=0
    test_fail=0
    failed_list=""
    for i in $scripts ;
    do
      echo "Testing: $i" >&2
      test_count=`expr "$test_count" + 1`
      rm -f ~/.cache/common-lisp/"`pwd`"/* || true
      if DO $command $eval "(load \"$i\")" ; then
        echo "Using $command, $i passed" >&2
	test_pass=`expr "$test_pass" + 1`
      else
        echo "Using $command, $i failed" >&2
	test_fail=`expr "$test_fail" + 1`
	failed_list="$failed_list $i"
        sok=0
      fi
    done
    echo >&2
    echo "-#---------------------------------------" >&2
    echo "Using $command" >&2
    echo "Ran $test_count tests: " >&2
    echo "  $test_pass passing and $test_fail failing" >&2
    if [ $test_fail -eq 0 ] ; then
	echo "all tests apparently successful" >&2
        echo success > ../tmp/results/status
    else
	echo "failing test(s): $failed_list" >&2
    fi
    echo "-#---------------------------------------" >&2
    echo >&2
  fi
}

# terminate on error
set -e

lisp=$1
if [ -z $1 ] ; then
    lisp="sbcl"
fi

command= flags= nodebug= eval=
case "$lisp" in
  sbcl)
    command="${SBCL:-sbcl}"
    flags="--noinform --userinit /dev/null --sysinit /dev/null"
    nodebug="--disable-debugger"
    eval="--eval" ;;
  clisp)
    command="${CLISP:-clisp}"
    flags="-norc -ansi -I "
    nodebug="-on-error exit"
    eval="-x" ;;
  allegro)
    command="${ALLEGRO:-alisp}"
    flags="-q"
    nodebug="-batch"
    eval="-e" ;;
  allegromodern)
    command="${ALLEGROMODERN:-mlisp}"
    flags="-q"
    nodebug="-batch"
    eval="-e" ;;
  ccl)
    command="${CCL:-ccl}"
    flags="--no-init --quiet"
    nodebug="--batch"
    eval="--eval" ;;
  cmucl)
    command="${CMUCL:-lisp}"
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  ecl)
    command="${ECL:-ecl}"
    flags="-norc"
    eval="-eval" ;;
  lispworks)
    command="${LISPWORKS:-lispworks}"
    # If you have a licensed copy of lispworks,
    # you can obtain the "lispworks" binary with, e.g.
    # echo '(hcl:save-image "/lispworks" :environment nil)' > /tmp/build.lisp ;
    # ./lispworks-6-0-0-x86-linux -siteinit - -init - -build /tmp/build.lisp
    flags="-siteinit - -init -"
    eval="-eval" ;;
  gclcvs)
    export GCL_ANSI=t
    command="${GCL:-gclcvs}"
    flags="-batch"
    eval="-eval" ;;
  abcl)
    command="${ABCL:-abcl}"
    flags="--noinit --noinform"
    eval="--eval" ;;
  *)
    echo "Unsupported lisp: $1" >&2
    echo "Please add support to run-tests.sh" >&2
    exit 42 ;;
esac

if ! type "$command" ; then
    echo "lisp implementation not found: $command" >&2
    exit 43
fi

ASDFDIR="$(cd .. ; /bin/pwd)"
export CL_SOURCE_REGISTRY="${ASDFDIR}"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${ASDFDIR}\" (\"${ASDFDIR}/tmp/fasls\" :implementation)) :ignore-inherited-configuration)"
env | grep asdf

command="$command $flags"
if [ -z "${DEBUG_ASDF_TEST}" ] ; then
  command="$command $nodebug"
fi


create_config () {
    mkdir -p ../tmp/test-source-registry-conf.d ../tmp/test-asdf-output-translations-conf.d
}

clean_up () {
    rm -rf ../tmp/test-source-registry-conf.d ../tmp/test-asdf-output-translations-conf.d
}

if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
else
    create_config
    mkdir -p ../tmp/results
    echo failure > ../tmp/results/status
    thedate=`date "+%Y-%m-%d"`
    do_tests "$command" "$eval" 2>&1 | \
	tee "../tmp/results/${lisp}.text" "../tmp/results/${lisp}-${thedate}.save"
    read a < ../tmp/results/status
    clean_up
    [ success = "$a" ] ## exit code
fi
