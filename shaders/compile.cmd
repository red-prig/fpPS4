
@echo off

Rem The source of "glslangValidator" and "spirv-opt" is Vulkan SDK
Rem The source of "lazres" is Lazarus IDE

Set spirvgls=glslangValidator -g0 -V --target-env vulkan1.0 

Set spirvopt=spirv-opt --eliminate-dead-branches --eliminate-local-multi-store --inline-entry-points-exhaustive --eliminate-dead-code-aggressive --scalar-replacement --simplify-instructions

Set lrs=shaders.lrs

echo [build comp]
For /F %%a in ('dir /B') do if "%%~xa"==".comp" (call :build %%a %%~na)

echo [build vert]
For /F %%a in ('dir /B') do if "%%~xa"==".vert" (call :build %%a %%~na)

echo [build lrs]
lazres %lrs%

echo [clean]
For /F %%a in ('dir /B') do if "%%~xa"==".spv" (call del /Q %%a)

pause
exit

:build
 Set lrs=%lrs% %2.spv
 %spirvgls% %1      -o %2.spv
 %spirvopt% %2.spv  -o %2.spv
exit /b
