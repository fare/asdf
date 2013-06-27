#+ecl
(ffi:clines "
extern int sample_function();

int sample_function()
{
	return 42;
}
")
