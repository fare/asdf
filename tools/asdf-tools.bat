@echo off
if "%CCL%" == "" set CCL=ccl
%CCL% --load %~dp0\asdf-tools -- %*
