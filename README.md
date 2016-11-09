ASDF: Another System Definition Facility
========================================

For all information about ASDF, see the web page:
<https://common-lisp.net/project/asdf/>

Notably read the manual for instructions on how to use it:
<https://common-lisp.net/project/asdf/asdf.html>

This file is only a guide for new developers.

[TOC]


Building ASDF
-------------

First, make sure ASDF is checked out under a path registered by the source-registry,
if that isn't the case yet (see the [manual](http://common-lisp.net/project/asdf/asdf.html)).
One place would be:

    ~/.local/share/common-lisp/source/asdf/

or, assuming your implementation provides ASDF 3.1 or later:

    ~/common-lisp/asdf/


If you cloned our git repository, rather than extracted a tarball,
bootstrap a copy of `build/asdf.lisp` with:

    make


Building the documentation
--------------------------

The manual is also in the [doc/](doc/) subdirectory, and can be prepared with:

    make -C doc


Testing ASDF
------------

Before you may run tests, you need a few CL libraries.
The simplest way to get them is as follows, but read below:

    make ext

_NOTA BENE_: You may also need to run `make ext` again
after you `git pull` or switch branch, to update the `ext/` directory.
This unhappily is not automatic.
If for some reason tests fail, particularly due to an error
compiling, loading or running a library, then run `make ext` and try again.

The above `make` target uses `git submodule update --init` to download
all these libraries using git. If you don't otherwise maintain your
own set of carefully controlled CL libraries, that's what you want to use.
However, it is only available if you have a git checkout of ASDF;
not if you used a tarball.
If you use a tarball or otherwise do maintain your own set
of carefully controlled CL libraries then you will want to use whichever tools
you use (e.g. `quicklisp`, `clbuild`, or your own scripts around `git`)
to download these libraries:
`alexandria`, `asdf-encodings`, `cl-launch`, `closer-mop`, `cl-ppcre`,
`cl-scripting`, `fare-mop`, `fare-quasiquote`, `fare-utils`, `inferior-shell`,
`lisp-invocation`, `named-readtables`, `optima`.

If you are a CL developer, you may already have them, or may want
to use your own tools to download a version of them you control.
If you use [Quicklisp](https://www.quicklisp.org/), you may let
Quicklisp download those you don't have.
In these cases, you may NOT want to use the git submodules from `make ext`.
Otherwise, if you want to let ASDF download known-working versions
of its dependencies, you can do it with:

    make ext

Once you have all the required libraries and the asdf-tools script can find
a suitable Common Lisp implementation, you may run all the tests
on a given Common Lisp implementation `$L`, with your favorite installed system `$S`, using:

    make t u l=$L s=$S

To run only the regression test scripts, try simply:

    make l=$L test-scripts


Lisp Scripting test system
--------------------------

ASDF by default uses a shell script in `./test/run-tests.sh` to run the scripts
that orchestrate its tests.

An alternate build and test system using Common Lisp as a scripting language is available.
It is disabled by default because the new maintainer is having trouble with it in his environments.
It worked fine for the previous maintainer in his environments, and may be particularly useful
on Windows if and when the shell-based test system fails.
Its source code is in [tools/](tools/) and
it can invoke using the script [make-asdf.sh](make-asdf.sh),
or, on Windows, [make-asdf.bat](make-asdf.bat).

To use this alternate test system, pass to `make` the extra arguments `-f Makefile-lisp-scripting`
as in for instance:

    make -f Makefile-lisp-scripting t l=sbcl

Or you can make that your local default (assuming GNU make) using:

    ln -s Makefile-lisp-scripting GNUmakefile

These Lisp tools by default use Clozure Common Lisp (CCL) to build and run a binary
`build/asdf-tools` that will orchestrate the tests.
By defining and exporting the variable `LISP` to be one of `ccl`, `sbcl` or `allegro`, you
can have it use an alternate Common Lisp implementation instead.
Install CCL (respectively SBCL or Allegro) and make sure an executable called
`ccl` (respectively `sbcl` or `alisp`) is in your `PATH`,
or that you export a variable `CCL` (respectively `SBCL` or `ALLEGRO`)
that points to the executable.
To use a further Common Lisp implementation, suitably edit the script
[`tools/asdf-tools`](tools/asdf-tools),
or, on Windows, the batch file [`tools/asdf-tools.bat`](tools/asdf-tools.bat).
(Note that as of SBCL 1.2.13, we recommend against using SBCL on Windows for that purpose.)

Note that the executable `build/asdf-tools` is being built the first time you test ASDF.
When you update ASDF, via e.g. `git pull` or a branch switch, you may have to update it, with:

    make -f Makefile-lisp-scripting build-asdf-tools

The reason this is not done automatically every time is because
building it depends on a working ASDF;
but when you're modifying ASDF and testing it, you cannot rely on a working ASDF:
indeed, a developer may not only make mistakes, but may deliberately
introduce or re-introduce bugs at some place to test code in another place.


Debugging ASDF
--------------

To interactively debug ASDF, you may load it in such a way that `M-.` will work,
by installing the source code, and running:

    (asdf:load-system :uiop) ;; loading uiop is simple
    (map () 'load ;; loading asdf/defsystem is tricky
     (asdf:input-files :concatenate-source-op "asdf/defsystem"))

Note that the above can be adapted in a general recipe to get all the files in a system, in order.
To also have the files in systems it transitively depends on, add the `:other-systems t` keyword
argument to the call to `asdf::required-components`.

To interactively use the `asdf-tools`, you need to either have
all its dependencies installed and configured.
If you're using them through the `ext/` directory and `make ext`,
then you may need to emulate what the script in [tools/asdf-tools](tools/asdf-tools) does
with respect to initializing the source-registry.
Note that it also declares a system for `cl-launch/dispatch`;
you can either do something similar, or expand the source for `cl-launch` with
`make -C ext/cl-launch source` so `cl-launch.asd` will be created.


How do I navigate this source tree?
-----------------------------------

* [asdf.asd](asdf.asd)
    * The system definition for building ASDF with ASDF.

* `*.lisp`
    * The source code files for `asdf/defsystem`.
      See [asdf.asd](asdf.asd) for the order in which they are loaded.

* [uiop/](uiop/)
    * Utilities of Implementation- and OS- Portability,
      the portability layer of ASDF. It has its own [README](uiop/README.md),
      and functions all have docstrings.

* [Makefile](Makefile)
    * a minimal `Makefile` for bootstrap and development purposes.
      Most of the logic is in the [asdf-tools](tools/asdf-tools.asd) system below.

* [tools/](tools/)
    * `asdf-tools`, a system to build, test and release ASDF. It includes:
        * [asdf-tools](tools/asdf-tools) -- a shell script to run it as a shell command.
        * [asdf-tools.bat](tools/asdf-tools.bat) -- a Windows batch file to run the above.
        * [asdf-tools.asd](tools/asdf-tools.asd) -- system definition for asdf-tools
        * `*.lisp` -- the source code for the `asdf-tools` system, except for the few files below.
    * also a couple scripts to help ASDF users:
        * [load-asdf.lisp](tools/load-asdf.lisp) -- a working example script to load, configure and use ASDF in a self-contained project
        * [install-asdf.lisp](install-asdf.lisp) -- replace and update an implementation's ASDF
        * [cl-source-registry-cache.lisp](cl-source-registry-cache.lisp) -- update a cache for the source-registry as a standalone script.

* [Makefile-lisp-scripting](Makefile-lisp-scripting),
  [make-asdf.sh](make-asdf.sh) and [make-asdf.bat](make-asdf.bat)
    * Makefile and scripts to invoke the lisp scripting variants of the build system.

* [version.lisp-expr](version.lisp-expr)
    * The current version. Bumped up every time the code changes, using:

            make bump

* [doc/](doc/)
    * documentation for ASDF, including:
        * [index.html](doc/index.html) -- the web page for <http://common-lisp.net/project/asdf/>
        * [asdf.texinfo](doc/asdf.texinfo) -- our manual
        * [Makefile](doc/Makefile) -- how to build the manual
        * [cclan.png](doc/cclan.png) [lisp-logo120x80.png](doc/lisp-logo120x80.png)
          [style.css](doc/style.css) [favicon.ico](doc/favicon.ico)
          -- auxiliaries of [index.html](doc/index.html)

* [test/](test/)
    * regression test scripts (and ancillary files) for developers to check
      that they don't unintentionally break any of the functionality of ASDF.
      Far from covering all of ASDF, but a good start.

* [contrib/](contrib/)
    * a few contributed files that show case how to use ASDF
      or help with debugging it or debugging programs that use it.

* [debian/](debian/)
    * files for packaging on Debian, Ubuntu, etc.
      (now only present in the debian branch).

* [build/](build/)
    * where the `Makefile` and `asdf-tools` store their output files, including
        * `asdf.lisp` -- the current one-file deliverable of ASDF
        * `asdf-*.lisp` -- for upgrade test purposes, old versions
        * `asdf-tools` -- the executable for asdf-tools (.exe on Windows)
        * `results/` -- logs of tests that have been run
        * `fasls/` -- output files while running tests

* [ext/](ext/)
    * external dependencies, that can be populated with `make ext`
      or equivalently with `git submodule update --init`.
      Depopulate it with `make noext`
	  or equivalently with: `submodule deinit .`

* [README.md](README.md)
    * this file.

* [TODO](TODO)
    * plenty of ideas for how to further improve ASDF.


Last updated Sunday, October 2nd, 2016.
