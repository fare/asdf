# Syntax Control

This document describes an issue with using reader macros in Common Lisp,
and makes proposals as to how to handle this issue in ASDF.

https://bugs.launchpad.net/asdf/+bug/1293325
http://thread.gmane.org/gmane.lisp.asdf.devel/3883
http://thread.gmane.org/gmane.lisp.asdf.devel/4033


## When Syntax Gets Out of Control

Currently ASDF makes no attempt to control what value `*readtable*` is bound to.
As a result, systems with uncontrolled side-effects to the current `*readtable*`
may appear to build properly when built from scratch by themselves,
yet may break the build in subtle way when the build is incremental and/or
when part of a larger build that contains other systems
with conflicting side-effects.
(NB: The first variants of this document were written in 2014 while working on
ASDF 3.1, but its contents remain current as of ASDF 3.3.0 in October 2017.)

For instance, some CL libraries directly modify the current `*readtable*`
by calling e.g. `set-dispatch-macro-character` without specifying a readtable.
But most CL programs also expect to be read by the standard CL readtable.
This can lead to "interesting" issues:

  * If two systems none of which depends on the other (or an implementation
    and a system not tied to it) both define reader macros for the same
    character or pair of characters (e.g. `#u`), then a build that includes
    both these systems never guarantees which of the two systems will have been
    loaded last and whose side-effect will win at the time any particular
    subsequent system that depends on either of the above is loaded.
    It may depend on which of the libraries was modified last (or last had
    a modified dependency) and therefore was or wasn't recompiled during the
    incremental build; it may depend on the order in which dependencies appear
    in the larger build; it may depend on which strategy is used by the
    particular version of ASDF; it may depend on the use of POIU; etc.

  * Worse, even if some system plays nice with everyone else,
    it can still mess up or be messed up by an interactive build.
    A system can play nice by only using readtables through `named-readtables`,
    whereby it only modifies and uses well-defined private readtables,
    never modifying a random underspecified "current readtable".
    Yet, if ASDF is called while such a readtable is active as the `*readtable*`
    then all hell can break loose:
    If some system is built that doesn't play nice and modifies the readtable,
    then the readtable becomes corrupted and may cause further builds to fail.
    If some unrelated system is built that didn't expect the modified readtable,
    it may be negatively affected by it and corrupt the build.
    At this point, your fasl cache is corrupted and you need to clean it
    and re-build everything from scratch in a new image.

While changes to the readtable will cause the most spectacular failures,
other syntax-controlling variables are at stake, too.

  * Note that because `load` and `compile-file` locally rebinds `*readtable*`
    to the same value as in the surrounding context,
    changes of *binding* to the `*readtable*` variable,
    unlike modifications to the *object* currently bound to it,
    do not escape the evaluation of a given file.
    See ANSI spec description of binding of readtable:
    [ANSI spec issue "IN-SYNTAX"](http://clhs.lisp.se/Issues/iss196_w.htm).
    ["compile-file binds `*readtable*` and `*package*` to the values they held before processing the file."](http://clhs.lisp.se/Body/f_cmp_fi.htm),
    ["load binds `*readtable*` and `*package*` to the values they held before /loading/ the file."](http://clhs.lisp.se/Body/f_load.htm)

  * However, modifications to *other* syntax variables,
    such as `*read-base*`, or `*read-default-float-format*` or `*read-eval*`
    or any of the `*print-FOO*` variables, do escape this evaluation.

  * Furthermore, any syntax extension whether using reader macros or regular
    macros, if it depends on the bindings of any variable whatsoever,
    causes changes to that variable to similarly escape in the environment.
    The set of syntax variables is therefore potentially unbounded.

  * Because of the possibility of an incremental build, there can never be
    a guarantee in advance about what will or won't be rebuilt incrementally,
    and which changes to syntax variables may or may not have been effected
    when any system is compiled.
    Therefore, it is not allowed to ever let modifications to any such variable
    to escape a given system.
    Thus, it is not acceptable to use
    `(setf *read-default-float-format* 'double-float)`
    and not restore the initial value before the end of a given system's build;
    if your code depends on such changes, you must either hook into ASDF
    to make such bindings happen, or you must only use ASDF as wrapped through
    build scripts that enforce the correct values when building any given file.


## Some examples of breakage

### Conflict with macro-characters

If two systems define the same macro-character or dispatch-macro-character,
such as `#"` being defined by both `closure-common` and `string-escape`,
then these systems, if they are part of the same build,
may cause non-determinism as to which definition will be active at any time
during an incremental build.
Depending on which system was last modified, then only that system may be
re-built, at which point its modifications to the readtable will happen last.
No system can use `#"` syntax and trust which of the above last won.

Other conflicts found by grepping Quicklisp (as of October 2017) for
`'^(set-dispatch-macro-character '` include:
`#T` defined by both `closure-html` and `racer`, or
`#{` defined by both `cl-quickcheck` and `folio`, or
`#~` defined by both `metatilities` and `perlre`, or
`#t` and `#f` defined by both `paiprolog-html` and `racer`, or
`#_` defined by both `software-evolution` and CCL or MCL.
There may or may not be further conflicts not detected through this simple grep
for direct toplevel modification of the shared `*readtable*`.


### Building with the wrong `*readtable*`

Currently, all components compiled with ASDF promiscuously share
whatever readtable is active at the time `asdf:operate` is called.
This means that if some component side-effects the `*readtable*` variable
and/or the state of the data structure it's bound to,
the effects will be seen by all components compiled with ASDF,
*including components that do not "depends-on" support functions* that the reader output produces.
This causes catastrophic failure, when at the REPL you e.g.

```
(asdf:load-system :fare-quasiquote)
(named-readtables:in-readtable :fare-quasiquote)
;; Now modify a file in any system depends on, e.g. ASDF itself.
(uiop:run-program "touch somefile-that-in-the-system-below.lisp")
(asdf:load-system :some-modified-system-that-doesnt-use-fare-quasiquote) ;; example: ASDF
```

Then your system will be recompiled using `fare-quasiquote` for its backquote implementation,
except that since it doesn't depends-on `fare-quasiquote`,
the `fare-quasiquote` runtime will not be there next time you load the file from scratch.
Your fasl cache is therefore corrupted, and
there is nothing `fare-quasiquote` can do to protect users.
This is not specific to `fare-quasiquote`, but applies to anyone who uses any private readtable
with any discrepancy from the shared readtable concerning any macro character.

To protect users who use anything but the shared readtable at the REPL,
ASDF should make sure this shared readtable is used whenever building software.


## Statu quo: Readtable discipline

The _de facto_ though tacit requirements for all Common Lisp software currently
is therefore to follow a double discipline,
without which software may fail to build in mysterious ways:

  * Developers must *never* introduce conflicts with *any* known extension when
    modifying the `*readtable*` as inherited from the initial environment.
    This means that no one may ever modify `#u` except `puri`,
    or else become incompatible with `puri` and prevent the two systems
    from ever being used together. Similarly, no one may modify `#$` except CCL
    or else become incompatible with CCL.
    Unhappily, there is no global registry of who claims what extension,
    which makes defining any extension a gamble that jeopardizes the build.
    There is thus a chilling effect whereby
    prudent programmers never define extensions to the current `*readtable*`;
    and there is thus a counter-selective effect whereby
    only reckless programmers do define such extensions.

  * Developers must *never* call ASDF while the current `*readtable*` is bound
    to any readtable but the initial, shared readtable.
    If they do, their readtable may either be corrupted,
    or cause the build to be corrupted, or both.
    But there is currently no good means to access for certain this readtable,
    so no program may call ASDF at runtime while such readtable might be active;
    and no program may change the `*readtable*` while calling systems
    that might themselves call ASDF.
    This once again is a chilling effect against any use readtables other than
    the initial `*readtable*` and against any use of ASDF at runtime,
    and a counter-selection in who will use those features.


## Proposal 1: Minimal Syntax Control

This first proposal is for immediate implementation in the next release of ASDF
(whichever it may be, which would be ASDF 3.3.1 if done in October 2017).
It would offer a sane way for disciplined programmers to define reader macros
either as part of the shared readtable or as part of private readtables:

  * Start by merely documenting the restrictions that apply to modifying
    the implicitly shared initial readtable, and mandating that
    modifications be themselves documented:

     1. No one is allowed to modify any standard character in this readtable,
        in any way whatsoever.

     2. No two systems loaded in the same image may assign different meanings
        to a same character in the *shared readtable*.
        (This is not presently checkable, but see Proposal 4 below).

     3. Systems need to document any change to the *shared readtable*.
        Note that unqualified readtable modifications in the program will modify
        the object to which `*readtable*` is bound, which unless rebound
        will also be the *shared readtable*.

     4. Free software libraries will register these changes in
        a suitable page of cliki.net: < http://cliki.net/Macro%20Characters >

  * Optionally, declare a moratorium on defining any further such extension,
    except as part of a [CDR](https://common-lisp.net/project/cdr/);
    let's call that option 1.1.1 (and no such option 1.1.0).

  * Have ASDF itself enforce that the same readtable will be used
    whenever building Lisp software, by defining a `uiop:*shared-readtable*`
    that will be shared by all CL code by binding it around `asdf:operate`, thus
    allowing developers who want to keep using the above shared modifications
    and developers who prefer to use private readtables
    to work without interfering with each other anymore.

  * Optionally, have a similar mechanism for the `*print-pprint-dispatch*`,
    for all variables modified by `with-standard-io-syntax`, and/or
    for a user-extensible set of variables;
    let's call these options 1.2.1, 1.2.2 and 1.2.3 respectively
    (and adopting none of them 1.2.0).

With this minimal change in place, there is a viable story going forward
for how to use reader macros in Common Lisp, and
one that maintains backward compatibility.
Note that for a minimal change, the bindings should only go around `operate`,
at which point changes to syntax variables can still backward-compatibly escape
from one system to the other, and it remains the responsibility of the user
to make sure that it happens in a deterministic fashion.

My vote is for next version of ASDF (i.e. ASAP) to implement this proposal 1,
with options 1.1.1 and 1.2.2 (and maybe later 1.2.3, if the code is ready).


### Some hacks still supported under Proposal 1

One notably way to modify the syntax in a deterministic way that works with the
current ASDF 3.3.0 and would keep working is to serialize all dependencies
through a syntax-modification system: a system called e.g. `my-modified-syntax`
depends on all the regular-syntaxed software dependencies in the project,
before it itself modifies syntax variables;
all other systems in the project will then depend-on `my-modified-syntax`,
forcing the order of evaluation of the syntactic side-effects.
The above setup ensures that all regular CL files are build with regular syntax,
and that all files that depend on the modified syntax see that modified syntax.
Proposal 1 ensures that the latter modifications do not cause non-deterministic
build issues when one of the regular systems is modified
while the latter modifications are active.
Note that this approach is only possible in a closed system,
as opposed to when publishing free software libraries.
But this approach is indeed possible, and we may want to keep supporting it,
at least until we have better alternatives to offer.


### Details of the proposed implementation of Proposal 1

UIOP (and hence ASDF) remembers the `uiop:*initial-readtable*` that was
the current value of `cl:*readtable*` when UIOP was first loaded.
This readtable is presumably the
[initial readtable](http://clhs.lisp.se/Body/26_glo_i.htm#initial_readtable).
This readtable comes with all the restrictions described above.
UIOP also remembers a read-only `uiop:*standard-readtable*`
with only the standard characters.
The two above variables are not mean to be re-bound after initialization.

Last but not least, UIOP maintains a `uiop:*shared-readtable*`.
Its default value is `uiop:*initial-readtable*` which is backward compatible.
The variable can instead be re-bound to `uiop:*standard-readtable*`
for enforcement against uncontrolled readtable modifications,
or to a value of `(copy-readtable nil)`
(which should be a modifiable copy of the standard-readtable,
for code that modifies the current readtable yet
doesn't trust what was "initially" current when UIOP was loaded
and/or doesn't want to use implementation-dependent modifications),
or something else, so users may decide what shared readtable enforcement
they do or don't want in their builds.
Several implementations provide extensions in the *initial readtable*
that are *not* part of the *standard readtable*, such as
a `#!` ignorer sometimes, or importantly in CCL the `#$` and `#_` readers, etc.
A strict reading of the CLHS might make this non-conformant, but
many systems might rely on it, as the ITA codebase for QRes used to.

Systems that want to modify a persistent readtable
in ways not permitted with respect to the initial readtable
*must* arrange to use their own private readtable, e.g. via `named-readtables`.
Thanks to the re-binding of `*readtable*` by ASDF under Proposal 1 (and later),
they can now do it safely and not cause a build corruption if that readtable
is active while ASDF is called.


## Proposal 2: Per-System Syntax

The second proposal is builds on top of proposal 1, but
instead of only binding syntax variables around the entire build,
it allows for binding those variables
around the build of every system and/or every component.
This proposal adds significant complexity, as it requires
defining and tracking a set variables around every system.
In exchange for this complexity, however, you can achieve
both determinism and flexibility at the same time.

I (Faré) had code to do a lot of this proposal in the `syntax-control` branch,
but I believe it is not actually ready for release yet,
so I'm removing it.
I do not recommend this approach at this point, but I do describe it here
for comparison purposes and because of the light it sheds on the issue
and the rationale for solving it even in a minimal way.

With per-system syntax variables, it is possible for the `*readtable*`
as well as all other variables (such as `*print-pprint-dispatch*`)
to be specified as in a way better defined
than with using the promiscuous `*shared-readtable*`.
This makes it possible for systems to fully take advantage of syntax extension,
without either interfering with other systems
or suffering from interference by these other systems, and without
painfully doing a `(named-readtables:in-readtable :foo)` in every file.

   * ASDF tracks a set of "syntax variables".

   * For each variable in the set, and for each system,
     ASDF, tracks the "entry value" and "exit value" of the variable
     at the beginning and end of building the system.

   * ASDF maintains a partial order on these systems based on dependencies.
     For every considered system, identify the set of "maximal" dependencies,
     such that there isn't any intermediate system in the partial order
     between them and the considered system.

   * For every system and every variable, either the system must specify
     an explicit entry value for the variable, or all the maximal dependencies
     of that system must agree on their exit value (or else raise an error),
     at which point that value becomes the system's entry value of the variable.
     This applies to the readtable as to other values of syntax variables.
     (Alternatively (which I don't recommend) always inherit exit values
     from the last (or the first) of the explicit dependencies as entry values
     for the system.)

   * After a system is loaded, record all the exit values
     of all its syntax variables.

   * For the purpose of specifying the entry value of `*readtable*`,
     each readtable can be identified either
     by a string that designates the least system that has it as exit value,
     or by a symbol that designates a `named-readtables` readtable.

   * Since `*readtable*` is bound by `load` and `compile-file`,
     there needs be a separate mechanism for specifying
     exit values of readtable, for instance using `named-readtables` symbols.

   * A given readtable may be modified by further systems, according to rules
     essentially similar to those that apply to the initial readtable:
     no conflict is allowed.
     An extension that allows for read-only readtables would be welcome, too,
     so these user-defined readtables too can be protected from corruption.

   * After the end of a call to `operate`, ASDF may either:

     * restore the syntax variable values from before ASDF was called, or

     * side-effect the syntax variables to reflect exit values of the system it just built.

     * Conceivably, `operate` could do the former, and `load-system` the latter.
       Or the two options could otherwise both be available depending on a flag.


Does that strike you as complex? Because it is.
That's the price of *safely* supporting arbitrary modifications
to syntax variables in a deterministic yet general way.


## Proposal 3: Strict Enforcement.

This third proposal forbids any modification to the current `*readtable*`.
I made this proposal in earnest in 2014, but it was justifiedly met with horror
and a big pushback from the community: on the one hand, it is not
backward compatible and causes a lot of legacy software to fail to build;
on the other hand, most Common Lisp implementations offer little protection
in case this proposal is in place and someone makes a mistake.

Under this proposal, all Lisp libraries are read and compiled while
the current `*readtable*` is bound to the *standard* readtable,
as made available via `with-standard-io-syntax`
(see [CLHS 2.1.1.2](http://clhs.lisp.se/Body/02_aab.htm)
and [CLHS glossary for "standard readtable"](http://clhs.lisp.se/Body/26_glo_s.htm#standard_readtable)).
This readtable is notionally read-only as per the standard;
however only SBCL is actually enforcing that as of 2017,
such that you will have a nice clean error immediately,
instead of horrible silent system corruption later.
But this SBCL behavior is enough to detect systems that cause this issue and fix them.

This change however is clearly backward-incompatible,
and will break libraries that currently modify the current `*readtable*`.
These include unmaintained libraries this feature of which is actually used
by other libraries, as can be found in Quicklisp.
Fixing all these libraries will take time.
Yet without all those fixes, this proposal is a non-starter.
And even after those fixes, commercial users with non-visible code
may legitimately object to this change.

Using proposal 1, you can easily opt in this proposal 3 by setting
the `uiop:*shared-readtable*` to `uiop:*standard-readtable*`
just after you load ASDF and before you load anything else.
Even without proposal 1, you can achieve this effect
by having you entire build inside a `with-standard-io-syntax`,
or by inserting a `(setf *readtable* (with-standard-io-syntax *readtable*))`
before your build (or at an appropriate point in your build).

I (Faré) still think it is a good idea to mandate that at least public libraries
in Quicklisp should be fixed to never side-effect the current `*readtable*`.
But I acknowledge that the Common Lisp ecosystem is not ready for such a change
at this point; and since I'm not a paid Common Lisp professional at this point,
I don't have the energy to push for such a change by getting all libraries fixed.


## Proposal 4: Strict Enforcement modulo Declaration.

Proposal 4 is more elaborate, and would require modifications
of a least some implementations' support for readtables:
systems would declare as part of their `defsystem` declarations
which system defines (or uses?) which macro characters.
The implementation for readtables (or some semi-portable wrapper around them)
could then enforce those declarations.
I am not going to push for this proposal at this point,
but in case someone ever wants to implement a complete solution, here is a sketch.

  * Unhappily, there is no cheap way to enforce these restrictions,
    but they can be achieved by writing suitable wrappers
    on top of the implementation's native readtables.
    To hook these wrappers onto the standard symbols used by regular libraries,
    the implementation's package-locks, if any,
    may have to be temporarily disabled.
    On implementations where such hooks can't be used,
    we can fall back to mandating restrictions without enforcement,
    which isn't a regression compared to what is available today.

  * Readtables would detect what characters have or haven't been
    either used or modified so far.
    These flags can be harvested, individually or globally set or reset,
    to detect which systems have used or modified which characters.
    When defining a new macro character, an error is issued
    if the character was already used before and/or modified before.

  * A system may declare that it modifies some characters
    in the shared readtable. That it does actually modify those characters
    and only those characters can now be enforced via the above mechanism.
    If and when there are conflicts, they can be detected early,
    and a meaningful error message can be issued.
    Developers do not have to rely on a out-of-date manually curated cliki page
    to check that there are no conflicts;
    the build system can automatically check it for them,
    and the cliki page can be automatically generated from information
    automatically gathered by building all of Quicklisp.
