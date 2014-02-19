(defpackage :test-asdf/monodll (:use :test-asdf/monodll-1)) ;; dummy, for package-system dependencies.

#+ecl
(ffi:clines "
extern int always_42();

int always_42()
{
	return 6*always_7();
}
")
