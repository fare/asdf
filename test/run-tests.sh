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
    echo " lisps include abcl, allegro, allegromodern, ccl (clozure),"
    echo "  clisp, cmucl, ecl, gcl, gclcvs, sbcl, scl and xcl."
    echo "OPTIONS:"
    echo "    -d -- debug mode"
    echo "    -h -- show this message."
    echo "    -u -- upgrade tests."
}

unset DEBUG_ASDF_TEST upgrade

while getopts "duh" OPTION
do
    case $OPTION in
        d)
            export DEBUG_ASDF_TEST=t
            ;;
        u)
            upgrade=t
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
lisp=${1:-sbcl} ; shift

if [ -z "$*" ]; then
    scripts="*.script"
else
    scripts="$*"
fi

sok=1

DO () { ( set -x ; "$@" ); }

do_tests() {
  command="$1" eval="$2"
  rm -f ~/.cache/common-lisp/"`pwd`"/* || true
  ( cd .. && DO $command $eval '(or #.(load "test/script-support.lisp") #.(asdf-test::compile-asdf-script))' )
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
      echo >&2
      echo >&2
    done
    echo >&2
    echo "-#---------------------------------------" >&2
    echo "Using $command" >&2
    echo "Ran $test_count tests: " >&2
    echo "  $test_pass passing and $test_fail failing" >&2
    if [ $test_fail -eq 0 ] ; then
	echo "all tests apparently successful" >&2
        echo success > ../build/results/status
    else
	echo "failing test(s): $failed_list" >&2
    fi
    echo "-#---------------------------------------" >&2
    echo >&2
  fi
}

# terminate on error
set -e

command= flags= nodebug= eval=
case "$lisp" in
  abcl)
    command="${ABCL:-abcl}"
    flags="--noinit --nosystem --noinform"
    eval="--eval" ;;
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
  clisp)
    command="${CLISP:-clisp}"
    flags="-norc -ansi -I "
    nodebug="-on-error exit"
    eval="-x" ;;
  cmucl)
    # cmucl likes to have its executable called lisp, but so does scl
    # Please use a symlink or an exec ... "$@" trampoline script.
    command="${CMUCL:-cmucl}"
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  ecl)
    command="${ECL:-ecl}"
    flags="-norc -load sys:cmp"
    eval="-eval" ;;
  ecl_bytecodes)
    command="${ECL:-ecl}"
    flags="-norc -eval (ext::install-bytecodes-compiler)"
    eval="-eval" ;;
  gcl)
    export GCL_ANSI=t
    command="${GCL:-gcl}"
    flags="-batch"
    eval="-eval" ;;
  gclcvs)
    export GCL_ANSI=t
    command="${GCLCVS:-gclcvs}"
    flags="-batch"
    eval="-eval" ;;
  lispworks)
    command="${LISPWORKS:-lispworks}"
    # If you have a licensed copy of lispworks,
    # you can obtain the "lispworks" binary with, e.g.
    # echo '(hcl:save-image "/lispworks" :environment nil)' > /tmp/build.lisp ;
    # ./lispworks-6-0-0-x86-linux -siteinit - -init - -build /tmp/build.lisp
    flags="-siteinit - -init -"
    eval="-eval" ;;
  mkcl)
    command="${MKCL:-mkcl}"
    flags="-norc"
    eval="-eval" ;;
  sbcl)
    command="${SBCL:-sbcl}"
    flags="--noinform --userinit /dev/null --sysinit /dev/null" # --eval (require'asdf)
    nodebug="--disable-debugger"
    eval="--eval" ;;
  scl)
    command="${SCL:-scl}"
    flags="-noinit"
    nodebug="-batch"
    eval="-eval" ;;
  xcl)
    command="${XCL:-xcl}"
    flags="--no-userinit --no-siteinit"
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
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${ASDFDIR}\" (\"${ASDFDIR}/build/fasls\" :implementation)) :ignore-inherited-configuration)"
env | grep asdf

command="$command $flags"
if [ -z "${DEBUG_ASDF_TEST}" ] ; then
  command="$command $nodebug"
fi


create_config () {
    mkdir -p ../build/test-source-registry-conf.d ../build/test-asdf-output-translations-conf.d
}
upgrade_tags () {
    if [ -n "$TEST_ASDF_TAGS" ] ; then
        echo $TEST_ASDF_TAGS ; return
    fi
    # 1.37 is the last release by Daniel Barlow
    # 1.97 is the last release before Gary King takes over
    # 1.369 is the last release by Gary King
    # 2.000 to 2.019 and 2.20 to 2.27 and beyond are Faré's "stable" releases
    echo 1.37 1.97 1.369
    git tag -l '2.0??'
    git tag -l '2.??'
}
extract_tagged_asdf () {
    ver=$1
    file=build/asdf-${tag}.lisp ;
    if [ ! -f $file ] ; then
        case $ver in
            1.*|2.0*|2.2[0-6])
                git show ${tag}:asdf.lisp > $file ;;
            *)
                echo "Don't know how to extract asdf.lisp for version $tag"
                exit 55
                ;;
        esac
    fi
}
run_upgrade_tests () {
    su=test/script-support.lisp
    lu="(load\"$su\")"
    lv="$command $eval $lu $eval" ;
    for tag in `upgrade_tags` ; do
        for x in load-system load-lisp load-lisp-compile-load-fasl load-fasl just-load-fasl ; do
            lo="(asdf-test::load-asdf-lisp \"${tag}\")" ;
            echo "Testing upgrade from ASDF ${tag} using method $x" ;
            extract_tagged_asdf $tag
            case ${lisp}:$tag:$x in
                abcl:2.0[01][1-9]:*|abcl:2.2[1-2]:*)
                    : Skip, because it is so damn slow ;;
                ccl:1.*|ccl:2.0[01]*)
                    : Skip, because ccl broke old asdf ;;
                cmucl:1.*|cmucl:2.00*|cmucl:2.01[0-4]:*)
                    : Skip, CMUCL has problems before 2.014.7 due to source-registry upgrade ;;
                ecl*:1.*|ecl*:2.0[01]*|ecl*:2.20:*)
                    : Skip, because of various ASDF issues ;;
                gcl:1.*|gcl:2.0*|gcl:2.2[0-6]*) : Skip old versions that do not support GCL 2.6 ;;
                mkcl:1.*|mkcl:2.0[01]*|mkcl:2.2[0-3]:*)
                    : Skip, because MKCL is only supported starting with 2.24 ;;
                xcl:1.*|xcl:2.00*|xcl:2.01[0-4]:*|xcl:*)
                    : XCL support starts with ASDF 2.014.2 - It also hangs badly during upgrade. ;;
                *) (set -x ; case $x in
                            load-system) l="$lo (asdf-test::load-asdf-system)" ;;
                            load-lisp) l="$lo (asdf-test::load-asdf-lisp)" ;;
                            load-lisp-compile-load-fasl) l="$lo (asdf-test::compile-load-asdf)" ;;
                            load-fasl) l="$lo (asdf-test::load-asdf-fasl)" ;;
                            just-load-fasl) l="(asdf-test::load-asdf-fasl)" ;;
                            *) echo "WTF?" ; exit 2 ;; esac ;
                        $lv "(asdf-test::test-asdf $l)" ) ||
                    { echo "upgrade FAILED" ; exit 1 ;} ;; esac ;
        done ; done 2>&1 | tee build/results/${lisp}-upgrade.text
}
run_tests () {
  create_config
  mkdir -p ../build/results
  echo failure > ../build/results/status
    thedate=`date "+%Y-%m-%d"`
    do_tests "$command" "$eval" 2>&1 | \
	tee "../build/results/${lisp}.text" "../build/results/${lisp}-${thedate}.save"
    read a < ../build/results/status
  clean_up
  [ success = "$a" ] ## exit code
}
clean_up () {
    rm -rf ../build/test-source-registry-conf.d ../build/test-asdf-output-translations-conf.d
}

if [ -z "$command" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
elif [ -n "$upgrade" ] ; then
    run_upgrade_tests
else
    run_tests
fi
