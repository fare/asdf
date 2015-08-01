#!/bin/sh

here="$(dirname $0)"

header_lisp="header.lisp"
driver_lisp="uiop/package.lisp uiop/common-lisp.lisp uiop/utility.lisp uiop/os.lisp uiop/pathname.lisp uiop/filesystem.lisp uiop/stream.lisp uiop/image.lisp uiop/run-program.lisp uiop/lisp-build.lisp uiop/configuration.lisp uiop/backward-driver.lisp uiop/driver.lisp"
defsystem_lisp="upgrade.lisp component.lisp system.lisp cache.lisp find-system.lisp find-component.lisp operation.lisp action.lisp lisp-action.lisp plan.lisp operate.lisp output-translations.lisp source-registry.lisp parse-defsystem.lisp bundle.lisp concatenate-source.lisp package-inferred-system.lisp backward-internals.lisp backward-interface.lisp interface.lisp user.lisp footer.lisp"

all () {
  # Default action: bootstrap asdf.lisp
  build_asdf
}
build_asdf () {
  # That's the only thing that we really need before we may invoke asdf-builder.
  mkdir -p build
  a=build/asdf.lisp
  cat ${header_lisp} ${driver_lisp} ${defsystem_lisp} > ${a}.tmp
  if [ -f ${a} ] && cmp -s ${a} ${a}.tmp ; then
    rm -rf ${a}.tmp
  else
    mv -f ${a}.tmp ${a}
  fi
}
ext () {
  # Download all the development-time dependencies of ASDF:
  git submodule update --init
}
noext () {
  # Remove all the development-time dependencies of ASDF:
  git submodule deinit .
}
driver_files () {
  # These targets are used during tests to ensure the Makefile is in synch with the .asd files.
  echo ${driver_lisp}
}
defsystem_files () {
  # These targets are used during tests to ensure the Makefile is in synch with the .asd files.
  echo ${defsystem_lisp}
}

case "$1" in
  "") all ;;
  all|build_asdf|ext|noext|driver_files|defsystem_files) "$@" ;;
  *) build_asdf ; exec ${here}/tools/asdf-tools env "$@" ;;
esac ; exit
