
@echo off
cd /D "%~dp0"

Set opt=spirv-opt --eliminate-dead-branches --eliminate-local-multi-store --eliminate-dead-code-aggressive --scalar-replacement --simplify-instructions

call :folder shader_dump

pause
exit

:folder
 For /F %%a in ('dir /B %1') do if "%%~xa"==".txt" (call :compil %1\%%~na)
exit /b

:compil
 echo %~n1
 del %1.spv >nul 2>&1
 spirv-as  --target-env vulkan1.1 %1.txt -o %1.spv
 spirv-val --scalar-block-layout --target-env vulkan1.1 %1.spv
 rem %opt% %1.spv -o %1.spv
exit /b
