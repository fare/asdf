(defpackage :test-asdf/monodll-1 (:use)) ;; dummy, for package-system dependencies.

#+ecl
(ffi:clines "
extern int always_7();

int always_7()
{
	return 7;
}
")
