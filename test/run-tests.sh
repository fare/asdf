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
  command=$1 eval=$2
  rm -f ~/.cache/common-lisp/"`pwd`"/* || true
  ( cd .. && $command $eval '(load "test/compile-asdf.lisp")' )
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

command=
case "$lisp" in
  sbcl)
    command=sbcl
    flags="--noinform --userinit /dev/null --sysinit /dev/null"
    nodebug="--disable-debugger"
    eval="--eval" ;;
  clisp)
    command=clisp
    flags="-norc -ansi -I "
    nodebug="-on-error exit"
    eval="-x" ;;
  allegro)
    command=alisp
    flags="-q"
    nodebug="-batch"
    eval="-e" ;;
  allegromodern)
    command=mlisp
    flags="-q"
    nodebug="-batch"
    eval="-e" ;;
  ccl)
    command=ccl
    flags="--no-init --quiet"
    nodebug="--batch"
    eval="--eval" ;;
  cmucl)
    command=lisp
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  ecl)
    command=ecl
    flags="-norc"
    eval="-eval" ;;
  lispworks)
    command=lispworks
    flags="-siteinit - -init -"
    eval="-eval" ;;
  gclcvs)
    export GCL_ANSI=t
    command=gclcvs
    flags="-q"
    eval="-eval" ;;
  *)
    echo "Unsupported lisp: $1" >&2
    echo "Please add support to run-tests.sh" >&2
    exit 42 ;;
esac

if [ -z "$command" ] ; then
    echo "lisp implementation not recognized: $1" >&2
    exit 43
fi
if ! type "$command" ; then
    echo "lisp implementation not found: $command" >&2
    exit 43
fi

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
    do_tests "$command" "$eval" 2>&1 | \
	tee "results/${lisp}.text" "results/${lisp}-${thedate}.save"
    clean_up
fi
