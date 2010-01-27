#!/bin/sh

# do_tests {lisp invocation} {scripts-regex}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

export TEST_DIR="$PWD/tmp/"
export CL_SOURCE_REGISTRY="$TEST_DIR/dir1/:$TEST_DIR/dir2//"

if [ x"$1" = "xhelp" ]; then
    echo "$0 [lisp invocation] [scripts-regex]"
    echo " - read lisp forms one at a time from matching scripts"
    echo " - quit with exit status 0 on getting eof"
    echo " - quit with exit status >0 if an unhandled error occurs"
    echo " you need to supply the .script in the second argument"
    echo " lisps include sbcl, clisp, allegro and allegromodern"
    exit -1
fi

if [ -z "$2" ]; then
    scripts="*.script"
else
    scripts="$2"
fi

sok=1

do_tests() {
rm -f *.$2 || true
( cd .. && echo '(load "test/compile-asdf.lisp")' | $1  )
if [ $? -eq 0 ] ; then
    test_count=0
    test_pass=0
    test_fail=0
    failed_list=""
    for i in $scripts ; 
    do 
      echo "Testing: $i" >&2
      test_count=`expr "$test_count" + 1`
      rm -f *.$2 || true
      if  $1 < $i ; then
        echo "Using $1, $i passed" >&2
	test_pass=`expr "$test_pass" + 1`
      else
        echo "Using $1, $i failed" >&2
	test_fail=`expr "$test_fail" + 1`
	failed_list="$failed_list $i"
        sok=0
      fi
    done
    echo >&2
    echo "-#---------------------------------------" >&2
    echo "Using $1" >&2
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

if [ "$lisp" = "sbcl" ] ; then 
    if type sbcl ; then
      fasl_ext="fasl"
      command="sbcl --userinit /dev/null --sysinit /dev/null --noinform --noprogrammer"
    fi
elif [ "$lisp" = "clisp" ] ; then
    if type clisp ; then
	fasl_ext="fas"
	command=`which clisp`
	command="$command -norc -ansi -I - "
    fi
elif [ "$lisp" = "allegro" ] ; then
    if type alisp ; then
	fasl_ext="fasl"
	command="alisp -q -batch "
    fi
elif [ "$lisp" = "allegromodern" ] ; then
    if type mlisp ; then
	fasl_ext="fasl"
	command="mlisp -q -batch "
    fi
elif [ "$lisp" = "ccl" ] ; then
    if type ccl ; then
	fasl_ext="dx32fsl"
	command="ccl --no-init --quiet --batch "
    fi
fi


#if [ -x /usr/bin/lisp ]
#then 
#  do_tests "/usr/bin/lisp -batch -noinit" x86f
#fi

create_asds () {
    mkdir -p tmp/{conf.d,dir1,dir2/{dir3,dir4}}
    for i in `find tmp | sed -e '1d;/conf.d/d'`; do touch "$i"/test.asd; done
}

clean_up () {
    rm -rf tmp
}


if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
else
    mkdir -p results
    echo $command
    thedate=`date "+%Y-%m-%d"`
    do_tests "$command" $fasl_ext 2>&1 | tee "results/${lisp}.text" "results/${lisp}-${thedate}.save"
fi
 
