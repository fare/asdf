@echo off
::: By default. We use CCL.
if "%CCL%" == "" set CCL=ccl
%CCL% --no-init --load %~dp0\asdf-tools -- %*

::: To use SBCL instead (assuming its Windows support has been improved enough),
::: comment out the above lines, and uncomment these:
::if "%SBCL%" == "" set SBCL=sbcl
::%SBCL% --no-userinit --no-sysinit --script %~dp0\asdf-tools %*

::: To use Allegro instead,
::: comment out the above lines, and uncomment these:
::if "%ALLEGRO%" == "" set ALLEGRO=alisp
:::Note that maybe you need something more like this (untested):
::ALLEGRO=c:\path\to\allegro\buildi.exe -I c:\path\to\allegro\alisp.dxl
::%ALLEGRO% --bat %~dp0\asdf-tools %*
