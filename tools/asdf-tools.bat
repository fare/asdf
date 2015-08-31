@echo off
if "%CCL%" == "" set CCL=ccl
%CCL% --no-init --load %~dp0\asdf-tools -- %*
