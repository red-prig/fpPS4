
@echo off

Set spirvgls=spirv\glslangValidator -g0 -V --target-env vulkan1.0 

Set spirvopt=spirv\spirv-opt --eliminate-dead-branches --eliminate-local-multi-store --inline-entry-points-exhaustive --eliminate-dead-code-aggressive --scalar-replacement --simplify-instructions

For /F %%a in ('dir /B') do if "%%~xa"==".comp" (call :compil %%a %%~na)

For /F %%a in ('dir /B') do if "%%~xa"==".vert" (call :compil %%a %%~na)

pause
exit

:compil
 echo  %2
 %spirvgls% %1      -o %2.spv
 %spirvopt% %2.spv  -o %2.spv
exit /b
