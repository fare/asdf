#!/bin/sh

function do_tests {
rm *.$2 || true
( cd .. && echo '(compile-file "asdf")' |$1  )
for i in *.script; 
do 
  rm *.$2 || true
  if $1 < $i ;then
    echo "Using $1, $i passed" >&2
  else
    echo "Using $1, $i failed" >&2
    exit 1
  fi
done
echo "Using $1, all tests apparently successful" >&2
}

# do_tests {lisp invocation} {fasl extension}
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs

set -e

if type sbcl 
then 
  do_tests "sbcl --userinit /dev/null --noprogrammer" fasl 
fi

if [ -x /usr/bin/lisp ]
then 
  do_tests "/usr/bin/lisp -batch" x86f
fi

if [ -x /usr/bin/clisp ]
then 
  do_tests "/usr/bin/clisp -norc -ansi -I " fas
fi
