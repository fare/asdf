#!/bin/sh

# This needs to
# - read lisp forms one at a time from standard input
# - quit with exit status 0 on getting eof
# - quit with exit status >0 if an unhandled error occurs
LISP="sbcl --noprogrammer"

# file extension that $LISP uses for compiled files
FASL=fasl

rm *.$FASL || true
for i in *.script; 
do 
  rm *.$FASL || true
  if $LISP < $i ;then
    echo "$i passed"
  else
    echo "$i failed"
    exit 1
  fi
done
echo "All tests apparently successful"
