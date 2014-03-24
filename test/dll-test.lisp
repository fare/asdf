(defpackage :test-asdf/dll-test (:use)) ;; dummy, for package-system dependencies.

#+ecl
(ffi:clines "
extern int sample_function();

int sample_function()
{
	return 42;
}
")

#+mkcl
(ffi:clines "
extern MKCL_DLLEXPORT int sample_function(void);

int sample_function(void)
{
	return 42;
}
")
