# Syntax Control

This document describes an issue with using reader macros in Common Lisp,
and makes proposals as to how to handle this issue when using ASDF.

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
ASDF 3.1, but its contents remain current as of ASDF 3.3.1 in December 2017.)

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
and/or the state of the underlying data structure that variable is bound to,
the effects will be seen by all components compiled with ASDF.
Worse, if some reader macro expands into uses of symbols, functions and other entities,
these entities will now be used in the fasls for components
that do not "depends-on" the systems in which those entities are defined.
All further attempts to load those fasls in a fresh image will then be catastrophic failures,
with no easy solution beside clearing the fasl cache
(for instance with `rm -rf ~/.cache/common-lisp/' from a shell outside Lisp).

As a case that happened to me, I was using my `fare-quasiquote` syntax so I may portably
use quasiquotation in match expressions (with `fare-matcher`, `optima` or now `trivia`).
Then for some reason I had to modify some software that didn't use `fare-quasiquote`.

```
(asdf:load-system :fare-quasiquote)
(named-readtables:in-readtable :fare-quasiquote)
;; modify a file in any system that does not depend on fare-quasiquote, e.g. ASDF itself.
(uiop:run-program "touch somefile-that-in-the-system-below.lisp")
(asdf:load-system :some-modified-system-that-doesnt-use-fare-quasiquote) ;; example: ASDF
```

My system was recompiled using `fare-quasiquote` for its backquote implementation.
Backquotes were thus expanded into uses of
`fare-quasiquote::list`, `fare-quasiquote::cons`, `fare-quasiquote::append`, `fare-quasiquote::quote`
and other such symbols that shadow the equivalent in package `common-lisp`.
However, since my system didn't depends-on `fare-quasiquote`,
the `fare-quasiquote` runtime will not be there next time you load the file in a fresh Lisp image.
(And in the case of ASDF itself being recompiled this way,
there is nothing left working to fix the mess from Lisp.)
Your fasl cache is therefore corrupted.
What more, there is nothing `fare-quasiquote` and its users can do to protect end-users.
And this is not specific to `fare-quasiquote`, but applies to anyone who uses any private readtable
with any discrepancy whatsoever from the shared readtable concerning any macro character.

To protect users who use anything but the shared readtable at the REPL,
ASDF should make sure this shared readtable is used whenever building software.


## Status quo: Readtable discipline

The _de facto_ though tacit requirements for all Common Lisp software currently
is to follow a double discipline,
without which software may fail to build in mysterious ways:
First, users must only make monotonic modifications to the shared readtable, and
use private readtables for any modification that cannot be guaranteed monotonic.
Second, users must manually ensure that whenever ASDF is invoked,
or might otherwise be invoked by a library,
then the current `*readtable*` is bound to the shared `*readtable*`.
These requirements are detailed in the respective subsections below.

Once again, these requirements already exist.
This essay only documents them when they were previously unsurfaced;
this might be another case of Lispers discovering a law of computing,
where designers and practitioners of other languages are often
under the delusion that they are in control of arbitrarily "making" the laws.
Interestingly though, these requirements apply to syntax extension through
reader-macros in a way that they don't apply to syntax extension through
regular macros: because regular macros are named by *symbols*,
that already have their own naming and packaging mechanisms and conventions
to avoid conflicts, in a way that is more common and more familiar
to Lispers as well as to practitioners of other languages.


### Shared Readtable Monotonicity

The first discipline to follow is that in their Lisp source code,
users must *never* introduce conflict with *any* known extension when
modifying the `*readtable*` as inherited from the initial environment.
They may only add new behaviors that do not conflict
on characters so far undefined,
and may never override a previous behavior defined or used
by either the implementation or any program or extension.
Indeed, whatever previous behavior that a previous program may have relied upon,

it will rely upon again when their are updated and rebuilt;
and any conflict means that the build will then fail.
Therefore:

  1. No one is allowed to modify any standard character (as listed in the CLHS)
     in the *shared readtable* they inherited as current `*readtable*`
     at the start of compiling or loading a file.

  2. No two systems loaded in the same image may assign different meanings
     to a same character in this *shared readtable*.
     (This is not presently checkable, but see Proposal 4 below).

  3. Using a non-standard character defined by the implementation
     such as `#$` in CCL, or any unquoted non-ASCII character in an symbol,
     while permitted, counts as "assigning meaning" to that character,
     at which point no other system in this image is allowed
     to override this meaning with a conflicting definition.

  4. All readtable modifications that do not, cannot or will not abide by
     the above restrictions must happen in a *private readtable*:
     a new readtable object, initially obtained via `copy-readtable`,
     saved in its own variable, to which `*readtable*` is dynamically bound
     around evaluations that use its incompatible syntax.
     The shared readtable must be restored outside of these evaluations,
     or be itself dynamically bound around sub-evaluations
     that use the shared syntax.

These restrictions are largely undocumented as a theoretical requirement,
yet mostly respected as a practical necessity:
users who try to make or use some piece of software that fails to respect them
soon enough discover the hard way that that software fails to interoperate
nicely with other software, at which point they fix it, or they work around it,
but in any case they avoid the conflict.
Yet, as of 2017, no implementation can nor will help enforce monotonicity;
users are wholly responsible for manually ensuring this monotonicity.

Thus for instance, no one may ever modify `#u` except `puri`,
or else they become incompatible with `puri` and
the two systems from ever being used together.
Ideally, `puri` and all its clients should be amended
to use a private readtable instead — but that's already beyond the status quo.
Similarly, no one may modify `#$` except CCL or else
become incompatible with CCL and systems that rely on that CCL extension.

Unhappily, there is no global registry of who claims what extension,
which makes defining any extension a gamble that jeopardizes the build.
There is thus a chilling effect whereby
prudent programmers never define extensions to the current `*readtable*`;
and there is thus a counter-selective effect whereby
only reckless programmers do define such extensions.


### Shared Readtable Restoration

The second discipline that users must follow as they invoke ASDF is
to *never* call ASDF while the current `*readtable*` is bound
to any readtable but the initial, shared readtable.
If they do, either their current readtable may be corrupted,
or it cause some other part of the build to be corrupted, or both.

However, there is currently no good means to access for certain
this priveleged "shared initial readtable", except by supposing that
it is the readtable that was current at the start of the file,
and by extending to authors of subsequent files the courtesy
of passing by the same readtable.
In the case of the readtable, this convention is happily facilitated
by the fact that `cl:load` and `cl:compile-file`
will both bind `*readtable*` around the execution of their bodies.

Now, this facilitation is not available in the case of other syntax variables
that may disrupt the build, such as `*read-base*`, `*read-default-float-format*`
or `*read-eval*`, that users must also take care to bind to their default value
around the invocation of ASDF, as part of the same discipline.

All in all, no program may call ASDF at runtime while any readtable but
the shared readtable might be active.
No program may even indirectly call any other program that might,
unless it or that other program ensures that is the case.
The only current way to safely invoke ASDF when the `*readtable*` is or may be
bound to anything but the initial readtable is to must explicitly
save that initial readtable to some variable and bind `*readtable*` back to it
around invocations of ASDF.

This restriction is also valid at the REPL. If for any reason
you change the `*readtable*` or any other syntax variable while at your REPL,
then you must not invoke ASDF or any program that might invoke ASDF
unless you first restore the default bindings for this
and all other modified syntax variables.

This once again is a chilling effect against any use readtables other than
the initial `*readtable*` and against any use of ASDF at runtime;
it is also a counter-selection in who will use those features.


## Proposal 1: Minimal Syntax Control

This first proposal is to make some minimal changes that would
maximally improve the situation given the small size of the changes.
This proposal is meant be adopted immediately in the next release of ASDF
(whichever it may be, which would be ASDF 3.3.2 if done in early 2018).
It independently amends each of the above two requirements:
The first requirement is amended by making monotonicity an official policy,
and by requiring that users should in turn explicitly document
the ways that their programs define or use syntax extensions.
The second requirement is amended by having ASDF itself
save the initial readtable and bind the `*readtable*` to it around evaluation,
so users of private readtables don't have to worry about it.

### Terminology

* Standard readtable/standard syntax: defined by the ANSI spec as
  follows: "standard syntax n. the syntax represented by the standard
  readtable and used as a reference syntax throughout this document."

* Initial syntax: the syntax in place when UIOP and ASDF are loaded.
  This *may* be the same as the standard syntax, but is not
  necessarily.

* Private syntax: a syntax intended for use by a system, or a coherent
  set of systems.  Note that while there is a single standard syntax,
  initial syntax, and shared syntax, there may be multiple private
  syntaxes.

* Shared syntax: the syntax that will be shared by ASDF across the
  systems it is loading.  The shared syntax will be initialized to the
  initial syntax, rather than standard syntax, in order to preserve
  backwards compatibility.

### Documenting Monotonicity

From now on, monotonicity of syntax extensions
is not just a de facto requirement, but an official policy.
Users *must* document what extensions their software makes,
and what extensions it uses.
Enforcing monotonicity remains a matter of discipline,
not enforced by software but by social convention.
But at least it is well-documented:

  * Systems need to document any change to the *shared readtable*
    that they either make or rely upon.
    Note that unqualified readtable modifications in the program
    will modify the object to which `*readtable*` is bound,
    which unless explicitly re-bound will also be the *shared readtable*.

  * Free software libraries will register these changes in
    a suitable page of cliki.net: < http://cliki.net/Macro%20Characters >

  * Optionally, declare a moratorium on defining any further such extension,
    except as part of a [CDR](https://common-lisp.net/project/cdr/);
    let's call that option 1.1.1 (and no such option 1.1.0).
    This means there must be stronger social control as to what is or isn't
    acceptable in terms of syntax extension to the shared readtable.
    However, since there currently exist no means of technical enforcement,
    this social control may prove inefficient or ineffective;
    and if the above declaration isn't backed by a strong consensus
    and accompanied by actual social control, then indeed the declaration
    will only have brought confusion, dissension, and
    a false sense of security in those who believe it is more than it really is.
    Then again, the battle can't be won if it is not fought.
    This option should probably be pursued as an add-on
    that depends on Proposal 1 but that Proposal 1 as such does not depend on.

  * Systems must also document any use of private readtables
    that they require from their clients or provide to their clients.

To compensate the heightened burden that this social solution imposes
on users or the shared syntax, the second part of this proposals
alleviates the burden on use of private syntax.


### Supporting Private Syntax

ASDF can itself handle shared readtable restoration that so far
every direct and indirect ASDF user has to worry about,
replacing a lot of social convention with a litte bit of code.
This change will enable safe interoperation of ASDF with
code that uses private readtables without placing the high burden
that currently lies upon them.
This change will crucially support users of such private readtables.

As before, users of incompatible syntax must locally bind the `*readtable*`
to their private readtable object. For that, they may use e.g.
`named-readtables:in-readtable` or `cl-syntax:use-syntax`.
Furthermore, their build files must be such that any non-standard binding
of the `*readtable*` and other syntax variables
(such as `*read-default-float-format*`) shall only apply
to the dynamic scope of building relevant files
(e.g. via `:around-compile-hook`, or else `:around` methods to perform).
Note that `*readtable*` is locally bound by `cl:load` already,
so you may safely use `named-readtables:in-readtable` within a file;
such however is not the case for other syntax variables,
so they still require special care.

Now, ASDF itself will enforce the binding of `*readtable*`
to a canonical shared readtable object around its building of Lisp software:
UIOP will define a variable `uiop:*shared-readtable*` that will at any moment
be bound to this special readtable object.
Then, ASDF will bind `*readtable*` to the value of `uiop:*shared-readtable*`
in an `:around` method to `asdf:operate`, so that all code built by ASDF
will reliably be using this shared readtable object
— except of course that code that itself explicitly and locally binds
`*readtable*` to a private readtable object, which it still very much supported.
Thus, developers use monotonic modifications to "the" shared readtable, and
developers who use private readtables (whether by preference or necessity)
can keep working together without interfering with each other anymore.

Importantly, as long as they keep abiding by the restrictions of the status quo,
developers do not have to be aware of this change,
or of the existence of `uiop:*shared-readtable*`:
things "just work", as ASDF takes care of business.
However, users who explicitly *want* to juggle with multiple syntaxes
can now rely on `uiop:*shared-readtable*` instead of current situation:
currently, each of these users must reinvent their own variable
to stash "the" initial syntax; and they don't even have a documented convention
or any technical or social guarantee that this will work.

Of course, ASDF could *further* manage a larger set of syntax variables,
and bind them to their default value around the building of Lisp code.
Optionally, the same mechanism of binding syntax variables in `asdf:operate`
could be extended beyond `*readtable*` only (which would be option 1.2.0),
to `*print-pprint-dispatch*` (option 1.2.1),
to all variables bound by `with-standard-io-syntax` (option 1.2.2), and/or
to a user-extensible set of variables (option 1.2.3).
Also, instead of binding variables only around `asdf:operate` (option 1.3.0),
the variables could be bound by UIOP around all invocations of
`uiop:compile-file*` and `uiop:load*` (option 1.3.1);
one problem with this latter option, though, is that it might be incompatible
with how some systems currently override these variables in `:around` methods,
so requires some migration, and is not probably applicable as an immediate change.
The `:around-compile` mechanism of `uiop:compile-file*`,
that might have to be adapted for `uiop:load*`,
is still available for such overrides — but we must make sure that users use them.
As these options may make this change not minimal,
they should probably be deferred as part of Proposal 2 below, if adopted at all.

### Proposal 1, NOW.

With the minimal changes of Proposal 1 in place, there will be a viable story
going forward for how to define and use reader macros in Common Lisp;
furthermore that story will maintain backward compatibility.

For a truly minimal change,
the bindings should only go around `operate` (option 1.3.0),
at which point changes to syntax variables can still backward-compatibly escape
from one system to the other, and it remains the responsibility of the user
to make sure that it happens in a deterministic fashion,
e.g. through use of proper dependencies and
`perform` methods on `prepare-op` for some systems.

My vote is for next version of ASDF (i.e. ASAP) to immediately implement
this proposal 1, with options 1.1.0, 1.2.2 (or else 1.2.0) and 1.3.0.
I further propose that option 1.1.1 be promoted in a separate campaign.
And option 1.2.3 or maybe even Proposal 2 below should be strongly considered
for a future release of ASDF (e.g. 3.5 or such).


### Some hacks still supported under Proposal 1

One way to modify the syntax in a deterministic way
that works with the current ASDF 3.3.1 and will keep working under Proposal 1
is to serialize all dependencies through a syntax-modification system:
a system called e.g. `my-modified-syntax`
depends on all the software dependencies in the project that must be
built with standard syntax,
before it itself modifies syntax variables.
All the systems in the project that use the modified syntax will then depend-on `my-modified-syntax`,
forcing the order of evaluation of the syntactic side-effects.
The above setup ensures that all regular CL files are built with standard syntax,
and that all files that depend on the modified syntax see that modified syntax.

Proposal 1 ensures that the latter modifications do not cause non-deterministic
build issues when one of the regular systems is modified
while the latter modifications are active.
Note that "modifying syntax variables" must be done in a robust way,
i.e. in a `perform :before` method, or it may be undone when
`*readtable*` is bound again.
At that point, though, you might as well use a `:around` method instead,
and make the modifications local.

In any case, this approach is only possible in a closed system,
as opposed to when publishing free software libraries.
For instance, it applies if you write a proprietary software application
that isn't meant to be depended on by anything outside your own ecosystem.
But this approach is indeed possible, and we may want to keep supporting it,
at least until we have better alternatives to offer.
Or we may want to deprecate it, and
document how users may do things cleanly instead.


### Details of the proposed implementation of Proposal 1

UIOP (and hence ASDF) remembers the `uiop:*initial-readtable*` that was
the current value of `cl:*readtable*` when UIOP was first loaded.
This readtable is presumably the
[initial readtable](http://clhs.lisp.se/Body/26_glo_i.htm#initial_readtable).
This readtable comes with all the restrictions described above.
UIOP also remembers a `uiop:*standard-readtable*`,
with only the standard characters, that must be treated as read-only
(though that is only enforced on SBCL, ECL, Allegro, CMUCL as of December 2017;
see Proposal 3 below).
Neither of these two variables is meant to be re-bound after initialization.

Last but not least, UIOP maintains a `uiop:*shared-readtable*`,
that will be used when compiling using ASDF.
Its default value is the same as that of `uiop:*initial-readtable*`,
for backward compatibility:
Using the `uiop:*standard-readtable*` would break backward compatibility
with the status quo because that table is read-only
whether that restriction is enforced by the implementation or not
(at which point *bad things* can happen).
Even using a mutable copy of the standard readtable with `(copy-readtable nil)`
is not backwards compatible, because several implementations provide extensions
in their *initial readtable* that are *not* part of the *standard readtable*,
such as a `#!` ignorer sometimes, or notably in CCL the `#$` and `#_` readers.
A strict reading of the CLHS might make this non-conformant, but
some widely used systems in Quicklisp do rely on it.
These systems may have to be fixed before any change in default happens.
Desirable or not and welcome by the community or not,
a change that breaks backward compatibility defeats
the immediate applicability of Proposal 1.

Now, though that must not be the default,
the `*shared-readtable*` variable can be usefully re-bound
to `uiop:*standard-readtable*` for enforcement
against uncontrolled readtable modifications.
It can be re-bound to a value of `(copy-readtable nil)`,
which should be a modifiable copy of the standard-readtable,
for code that modifies the current readtable yet
doesn't trust what was "initially" current when UIOP was loaded
and/or doesn't want to use implementation-dependent modifications.
It can be re-bound to some readtable that somehow provides an instrumented
Common Lisp in Common Lisp implementation.
Or it can be re-bound to something else, so
users may decide what shared readtable enforcement
they do or don't want in their builds.

All in all, systems that want to modify a persistent readtable
in ways not permitted with respect to the initial shared readtable
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
but I believe it was not actually ready for release yet, so I removed it.
I do not recommend this approach at this point, but I do describe it here
for comparison purposes and because of the light it sheds on the issue
and the rationale for solving it even in a minimal way.

With per-system syntax variables, it is possible for the `*readtable*`
as well as all other variables (such as `*print-pprint-dispatch*`)
to be specified in a way better defined
than by using the promiscuous `*shared-readtable*`.
This makes it possible for systems to fully take advantage of syntax extension,
without either interfering with other systems
or suffering from interference by these other systems, and without
painfully doing a `(named-readtables:in-readtable :foo)` in every file.

   * ASDF tracks a set of "syntax variables".

   * For each variable in the set, and for each system,
     ASDF tracks the "entry value" and "exit value" of the variable
     at the beginning and end of building the system.
     In this, Proposal 2 differs from Proposal 1 option 1.2.3,
     that always binds each variable to the same constant global default value.

   * ASDF maintains a partial order on these systems based on dependencies.
     For every considered system, identify the set of "maximal" dependencies,
     such that there isn't any intermediate system in the partial order
     between them and the considered system.

   * For every system and every variable, either the system must specify
     an explicit entry value for the variable, or all the maximal dependencies
     of that system must agree on their exit value (or else raise an error),
     at which point that value becomes the system's entry value of the variable.
     This applies to the `*readtable*` as well as to all other syntax variables.
     (Alternatively (which I don't recommend) always inherit exit values
     from the last (or the first) of the *explicit* dependencies as entry values
     for the system.)

   * After a system is loaded, record all the exit values
     of all its syntax variables.

   * For the purpose of specifying the entry value of `*readtable*`,
     each readtable can be identified either
     by a string that designates the least system that has it as exit value,
     or by a symbol that designates a `named-readtables` readtable.

   * Since `*readtable*` is bound by `load` and `compile-file`,
     there needs be a separate mechanism for specifying the exit values
     of `*readtable*`, for instance in `perform` methods,
     or using the special variable `*shared-readtable*`
     as the communication channel.

   * A given readtable may be modified by further systems, according to rules
     essentially similar to those that apply to the initial readtable:
     no conflict is allowed.
     Except it's only conflict between users of that private readtable.
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
and a big pushback from a significant part of the community:
on the one hand, it is not backward compatible and
causes a number of legacy systems to fail to build;
on the other hand, most Common Lisp implementations currently offer little protection
in case this proposal is in place and someone makes a mistake.

Under this proposal, all Lisp libraries are read and compiled while
the current `*readtable*` is bound to the *standard* readtable,
as made available via `with-standard-io-syntax`
(see [CLHS 2.1.1.2](http://clhs.lisp.se/Body/02_aab.htm)
and [CLHS glossary for "standard readtable"](http://clhs.lisp.se/Body/26_glo_s.htm#standard_readtable)).
This readtable is notionally read-only as per the standard.
However, as of December 2017, only SBCL, Allegro, ECL and CMUCL
actually reject modifications to the standard readtable and
give you a nice clean error immediately.
On at least CCL, CLISP, ABCL, Lispworks and MKCL,
instead will silently let you corrupt the standard readtable,
which may result in horrible failures later
when you use `with-standard-io-syntax` and it doesn't do what you expect.
Happily, those implementations that do enforce read-only behavior
may be enough to detect systems that cause this issue and fix them;
but that will still be painful.

You may test your current implementation
by evaluating the following form in a throw-away image:
```
(with-standard-io-syntax (set-macro-character #\$ (constantly 42)) (read-from-string "$"))
```
If it returns `42`,
then your implementation just let you corrupt the standard readtable
(so don't do it in an image that matters).
If it returns an error object,
then your implementation did enforce the standard readtable being read-only.

Now this Proposal 3 is clearly backward-incompatible,
and will break libraries that currently modify the current `*readtable*`.
These actually include unmaintained libraries that will be a pain to fix,
yet are themselves used by other libraries, as can be found in Quicklisp.
Fixing all these libraries will take time.
Yet without all those fixes, this proposal is a non-starter.
And even after those fixes, commercial users with non-visible code
may legitimately object to this change.

Using Proposal 1, you can easily opt in this proposal 3 by setting
the `uiop:*shared-readtable*` to `uiop:*standard-readtable*`
just after you load ASDF and before you load anything else.
Even without Proposal 1, you can achieve this effect
by having your entire build inside a `with-standard-io-syntax`,
or by inserting a `(setf *readtable* (with-standard-io-syntax *readtable*))`
before your build (or at an appropriate point in your build).

I (Faré) still think it is a good idea to mandate that
at least public libraries in Quicklisp should be fixed
to never side-effect the current `*readtable*`.
But I acknowledge that the Common Lisp community
is not ready for such a change at this point;
and since I haven't myself been a paid Common Lisp professional for years,
and have jumped ship to Gerbil Scheme, I don't persaonally have the energy
to push for such a change by getting all libraries fixed.
But maybe some future maintainer will read this and make it their mission.


## Proposal 4: Strict Enforcement modulo Declaration.

Proposal 4 is more elaborate, and would require modifications
of a least some implementations' support for readtables:
systems would declare as part of their `defsystem` declarations
what reader macros they use or define.
The implementation for readtables (or some semi-portable wrapper around them)
could then enforce those declarations.
I am not going to push for this proposal at this point,
but in case someone ever wants to implement a complete solution,
here is a sketch.

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
