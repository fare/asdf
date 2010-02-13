#!/bin/sh

# do_tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

export CL_SOURCE_REGISTRY="$PWD"
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

do_tests() {
  command=$1 eval=$2 fasl_ext=$3
  rm -f *.$fasl_ext ~/.cache/common-lisp/"`pwd`"/*.$fasl_ext || true
  ( cd .. && $command $eval '(load "test/compile-asdf.lisp")' )
  if [ $? -eq 0 ] ; then
    echo "Compiled OK"
    test_count=0
    test_pass=0
    test_fail=0
    failed_list=""
    for i in $scripts ;
    do
      echo "Testing: $i" >&2
      test_count=`expr "$test_count" + 1`
      rm -f *.$fasl_ext ~/.cache/common-lisp/"`pwd`"/*.$fasl_ext || true
      if $command $eval "(load \"$i\")" ; then
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

case "$lisp" in
  sbcl)
    if type sbcl ; then
      fasl_ext="fasl"
      command="sbcl --noinform --userinit /dev/null --sysinit /dev/null"
      nodebug="--disable-debugger"
      eval="--eval"
    fi ;;
  clisp)
    if type clisp ; then
	fasl_ext="fas"
	command=`which clisp`
	command="$command -norc -ansi -I "
        nodebug="-on-error exit"
        eval="-x"
    fi ;;
  allegro)
    if type alisp ; then
	fasl_ext="fasl"
	command="alisp -q "
        nodebug="-batch"
        eval="-e"
    fi ;;
  allegromodern)
    if type mlisp ; then
	fasl_ext="fasl"
	command="mlisp -q"
        nodebug="-batch"
        eval="-e"
    fi ;;
  ccl)
    if type ccl ; then
        case `uname -s` in
          Linux) fasl_os=lx ;;
          Darwin) fasl_os=dx ;;
        esac
        case `uname -m` in
          x86_64|ppc64) fasl_bits=64 ;;
          i?86|ppc) fasl_bits=32 ;;
        esac
        fasl_ext="${fasl_os}${fasl_bits}fsl"
	command="ccl --no-init --quiet"
        nodebug="--batch"
        eval="--eval"
    fi ;;
  cmucl)
    if type lisp ; then
	fasl_ext="x86f"
	command="lisp -noinit"
        nodebug="-batch"
        eval="-eval"
    fi ;;
  ecl)
    if type ecl ; then
	fasl_ext="fas"
	command=`which ecl`
	command="$command -norc"
        eval="-eval"
    fi ;;
  lispworks)
    if type lispworks ; then
	fasl_ext="ofasl"
	command=`which ecl`
	command="$command -siteinit - -init -"
        eval="-eval"
    fi ;;
esac

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
    mkdir -p results
    echo $command
    thedate=`date "+%Y-%m-%d"`
    do_tests "$command" "$eval" "$fasl_ext" 2>&1 | \
	tee "results/${lisp}.text" "results/${lisp}-${thedate}.save"
    clean_up
fi
