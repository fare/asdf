#!/bin/sh

# run-tests {lisp invocation} {scripts-regex}
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
    echo "    -c -- clean load test"
    echo "    -l -- load systems tests"
    echo "    -t -- test interactively"
    echo "    -H -- extract all asdf versions to upgrade from"
}

unset DEBUG_ASDF_TEST upgrade clean_load load_systems test_interactively extract_all
SHELL=/bin/sh
export SHELL DEBUG_ASDF_TEST GCL_ANSI ASDF_OUTPUT_TRANSLATIONS

while getopts "cdtHulhu" OPTION
do
    case $OPTION in
        d)
            DEBUG_ASDF_TEST=t
            ;;
        u)
            upgrade=t
            ;;
        c)
            clean_load=t
            ;;
        l)
            load_systems=t
            ;;
        t)
            test_interactively=t
            ;;
        H)
            extract_all=t
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


ECHO () { printf '%s\n' "$*" ;}
ECHOn () { printf '%s' "$*" ;}
DBG () { ECHO "$*" >& 2 ;}
simple_term_p () {
  case "$1" in *[!a-zA-Z0-9-+_,.:=%/]*) return 1 ;; *) return 0 ;; esac
}
kwote0 () { ECHOn "$1" | sed -e "s/\([\\\\\"\$\`]\)/\\\\\\1/g" ;}
kwote1 () { if simple_term_p "$1" ; then ECHOn "$1"
  else ECHOn "\"$(kwote0 "$1")\"" ; fi ;}
kwote () { ( set +x
  k="" ; for i ; do ECHOn "$k" ; kwote1 "$i" ; k=" " ; done ; echo
) }
DO () { kwote "$@" ; "$@" ; }

do_tests () {
  if [ -z "$*" ]; then
       scripts="*.script"
  else
       scripts="$*"
  fi
  env | grep -i asdf
  rm -f ~/.cache/common-lisp/"`pwd`"/* || true
  ( cd .. && DO $cmd $debugp $eval '(or #.(load "test/script-support.lisp") #.(asdf-test::compile-asdf-script))' )
  if [ $? -ne 0 ] ; then
    echo "Compilation FAILED" >&2
    echo "you can retry compilation with:" >&2
    echo ./test/run-tests.sh $lisp >&2
    echo "or more interactively (and maybe with rlwrap or in emacs), start with:" >&2
    echo "$cmd" >&2
    echo "then copy/paste:" >&2
    echo '(load "test/script-support.lisp") (asdf-test::compile-asdf-script)' >&2
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
      if DO $cmd $debugp $eval "(load \"script-support.lisp\")" $eval "(progn (asdf-test::load-asdf) (asdf-test::frob-packages) (asdf-test::with-test () (load \"$i\")))" ; then
        echo "Using $command, $i passed" >&2
	test_pass=`expr "$test_pass" + 1`
      else
        echo "Using $command, $i failed" >&2
	test_fail=`expr "$test_fail" + 1`
	failed_list="$failed_list $i"
        echo "you can retry compilation with:" >&2
        echo ./test/run-tests.sh $lisp $i >&2
        echo "or more interactively (and maybe with rlwrap or in emacs), start with:" >&2
        echo "(cd test ; $cmd )" >&2
        echo "then copy/paste:" >&2
        echo "'(#.(load \"script-support.lisp\") #.(asdf-test::da) #.(load-asdf) #.(load \"$i\"))" >&2
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
    #flags="-q"
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
    flags="-norc --silent -ansi -I "
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
    GCL_ANSI=t
    command="${GCL:-gcl}"
    flags="-batch"
    eval="-eval" ;;
  gclcvs)
    GCL_ANSI=t
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

if ! type "$command" > /dev/null ; then
    echo "lisp implementation not found: $command" >&2
    exit 43
fi

ASDFDIR="$(cd $(dirname $0)/.. ; /bin/pwd)"

cmd="$command $flags"
debugp=
if [ -z "${DEBUG_ASDF_TEST}" ] ; then
  debugp="$nodebug"
fi


create_config () {
    cd ${ASDFDIR}
    mkdir -p build/results/ build/test-source-registry-conf.d build/test-asdf-output-translations-conf.d
}
upgrade_tags () {
    if [ -n "$ASDF_UPGRADE_TEST_TAGS" ] ; then
        echo $ASDF_UPGRADE_TEST_TAGS ; return
    fi
    # REQUIRE is a magic tag meaning whatever your implementation provides
    # 1.85 is the last release by Daniel Barlow (not 1.37, which is the README revision!)
    # 1.97 is the last release before Gary King takes over
    # 1.369 is the last release by Gary King
    # 2.000 to 2.019 and 2.20 to 2.27 and beyond are Faré's "stable" releases
    # 2.26.61 is the last single-file, single-package ASDF.
    echo REQUIRE 1.85 1.97 1.369
    git tag -l '2.0??'
    git tag -l '2.??'
}
upgrade_methods () {
    if [ -n "$ASDF_UPGRADE_TEST_METHODS" ] ; then
        echo $ASDF_UPGRADE_TEST_METHODS ; return
    fi
    cat <<EOF
'load-asdf-lisp'load-asdf-lisp-clean
'load-asdf-lisp'load-asdf-system
'load-asdf-lisp'compile-load-asdf
'load-asdf-lisp'load-asdf-fasl
()'load-asdf-fasl
'load-asdf-lisp-and-test-uiop'load-asdf-fasl
EOF
}
extract_tagged_asdf () {
    cd ${ASDFDIR}
    mkdir -p build/
    tag=$1
    if [ REQUIRE = "$tag" ] ; then return 0 ; fi
    file=build/asdf-${tag}.lisp ;
    if [ ! -f $file ] ; then
        case $tag in
            1.*|2.0*|2.2[0-6]|2.26.61)
                git show ${tag}:asdf.lisp > $file ;;
            2.2[7-9]*|2.[3-9]*|3.*)
                mkdir -p build/old/build
                git archive ${tag} | (cd build/old/ ; tar xf -)
                make -C build/old
                mv build/old/build/asdf.lisp build/asdf-${tag}.lisp
                rm -rf build/old ;;
             *)
                echo "Don't know how to extract asdf.lisp for version $tag"
                exit 55
                ;;
        esac
    fi
}
extract_all_tagged_asdf () {
    for i in `upgrade_tags` ; do
      extract_tagged_asdf $i
    done
}
valid_upgrade_test_p () {
    case "${1}:${2}:${3}" in
        # It's damn slow. Also, for some reason, we punt on anything earlier than 2.25,
        # and only need to test it once, below for 2.24.
        abcl:1.*|abcl:2.00[0-9]:*|abcl:201[0-9]:*|abcl:2.2[0-3]:*) : ;;
        # ccl fasl numbering broke loading of old asdf 2.0
        ccl:2.0[01]*) : ;;
        # my old ubuntu clisp 2.44.1 is wired in
        # with an antique ASDF 1.374 from CLC that can't be downgraded.
        # 2.00[0-7] use UID, which fails on that CLISP and was removed afterwards.
        # Note that for the longest time, CLISP has included 2.011 in its distribution.
        # Since we punt on the upgrade, let's only do the test once, for 2.26.
        clisp:2.00[0-7]:*|clisp:1.*|clisp:2.0[01]*|clisp:2.2[0-5]:*) : ;;
        # Skip, CMUCL has problems before 2.014.7 due to source-registry upgrade.
        # Weird unidentified problems before 2.018, so we punt equally for everything before,
        # and only need to test it once: above, for 2.017.
        cmucl:1.*|cmucl:2.00*|cmucl:2.01[0-6]:*) : ;;
        # Skip many ECL tests, for various ASDF issues
        ecl*:1.*|ecl*:2.0[01]*|ecl*:2.20:*) : ;;
        # GCL 2.6 is only supported with ASDF 2.27, so skip earlier versions
        gcl:1.*|gcl:2.0*|gcl:2.2[0-6]*) : ;;
        # MKCL is only supported starting with 2.24, so skip earlier versions
        mkcl:1.*|mkcl:2.0[01]*|mkcl:2.2[0-3]:*) : ;;
        # XCL support starts with ASDF 2.014.2
        # — It also dies during upgrade trying to show the backtrace.
        xcl:1.*|xcl:2.00*|xcl:2.01[0-4]:*|xcl:*) : ;;
        *) return 0 ;;
   esac
   return 1
}
run_upgrade_tests () {
    cd ${ASDFDIR}
    mkdir -p build/results/
    rm -f build/*.*f* uiop/*.*f* test/*.*f* ## Remove stale FASLs from ASDF 1.x, especially when different implementations have same name
    ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${ASDFDIR}\" (\"${ASDFDIR}/build/fasls/\" :implementation \"asdf/\")) (t (\"${ASDFDIR}/build/fasls/\" :implementation \"root/\")) :ignore-inherited-configuration)"
    su=test/script-support.lisp
    for tag in `upgrade_tags` ; do
        for method in `upgrade_methods` ; do
            if valid_upgrade_test_p $lisp $tag $method ; then
                echo "Testing ASDF upgrade from ${tag} using method $method"
                extract_tagged_asdf $tag
                $cmd $debugp $eval \
                "'(#.(load\"$su\")#.(in-package :asdf-test)#.(test-upgrade $method \"$tag\"))" ||
                { echo "upgrade FAILED for $lisp from $tag using method $method" ;
                  echo "you can retry just that test with:" ;
                  echo ASDF_UPGRADE_TEST_TAGS=\"$tag\" ASDF_UPGRADE_TEST_METHODS=\"$method\" ./test/run-tests.sh -u $lisp ;
                  echo "or more interactively (and maybe with rlwrap or in emacs), start with:"
                  echo "$cmd"
                  echo "then copy/paste:"
                  echo "(load\"$su\") (asdf-test::da) (test-upgrade $method \"$tag\")"
                  exit 1 ;}
    fi ; done ; done 2>&1 | tee build/results/${lisp}-upgrade.text
}
run_tests () {
  create_config
  cd ./test/
  echo failure > ../build/results/status
    thedate=`date "+%Y-%m-%d"`
    do_tests "$@" 2>&1 | \
	tee "../build/results/${lisp}.text" "../build/results/${lisp}-${thedate}.save"
    read a < ../build/results/status
  clean_up
  if [ success = "$a" ] ; then ## exit code
      return 0
  else
     echo "To view full results and failures, try the following command:" >&2
     echo "     less -p ABORTED build/results/${lisp}.text" >&2
     return 1
  fi
}
clean_up () {
    rm -rf ../build/test-source-registry-conf.d ../build/test-asdf-output-translations-conf.d
}
test_clean_load () {
    case $lisp in
        gcl|cmucl) return 0 ;; # These are hopeless
    esac
    cd ${ASDFDIR}
    mkdir -p build/results/
    nop=build/results/${lisp}-nop.text
    load=build/results/${lisp}-load.text
    ${cmd} ${eval} \
      '(or #.(load "test/script-support.lisp" :verbose nil :print nil) #.(asdf-test::exit-lisp 0))' \
        > $nop 2>&1
    ${cmd} ${eval} \
      '(or #.(load "test/script-support.lisp" :verbose nil :print nil) #.(asdf-test::verbose nil) #.(load "build/asdf.lisp" :verbose nil) #.(asdf/image:quit 0))' \
        > $load 2>&1
    if diff $nop $load ; then
      echo "GOOD: Loading ASDF on $lisp produces no message" >&2 ; return 0
    else
      echo "BAD: Loading ASDF on $lisp produces messages" >&2 ; return 1
    fi
}
test_load_systems () {
    case $lisp in
        gcl) return 0 ;; # This one is hopeless
    esac
    cd ${ASDFDIR}
    mkdir -p build/results/
    echo "Loading all these systems: $*"
    ${cmd} ${eval} \
      "(or #.(load \"test/script-support.lisp\") #.(asdf-test::with-test () (asdf-test::test-load-systems $*)))" \
        2>&1 | tee build/results/${lisp}-systems.text
}
test_interactively () {
    cd ${ASDFDIR}
    mkdir -p build/results/
    rlwrap $cmd $eval "(or #.(load \"test/script-support.lisp\") #.(asdf-test::interactive-test '($*)))"
}

if [ -z "$cmd" ] ; then
    echo "Error: cannot find or do not know how to run Lisp named $lisp"
elif [ -n "$test_interactively" ] ; then
    test_interactively "$@"
elif [ -n "$clean_load" ] ; then
    test_clean_load
elif [ -n "$load_systems" ] ; then
    test_load_systems "$@"
elif [ -n "$upgrade" ] ; then
    run_upgrade_tests
elif [ -n "$extract_all" ] ; then
    extract_all_tagged_asdf
else
    run_tests "$@"
fi ; exit # NB: "; exit" makes it robust wrt the script being modified while running.
