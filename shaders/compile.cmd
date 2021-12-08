
Set spirvgls=spirv\glslangValidator -g0 -V --target-env vulkan1.0 

Set spirvopt=spirv\spirv-opt --eliminate-dead-branches --eliminate-local-multi-store --inline-entry-points-exhaustive --eliminate-dead-code-aggressive --scalar-replacement --simplify-instructions

%spirvgls% FLIP_CURSOR.comp -o FLIP_CURSOR.spv
%spirvopt% FLIP_CURSOR.spv -o FLIP_CURSOR.spv

%spirvgls% FLIP_TILE_A8R8G8B8_SRGB.comp -o FLIP_TILE_A8R8G8B8_SRGB.spv
%spirvopt% FLIP_TILE_A8R8G8B8_SRGB.spv -o FLIP_TILE_A8R8G8B8_SRGB.spv

%spirvgls% FLIP_LINE_A8R8G8B8_SRGB.comp -o FLIP_LINE_A8R8G8B8_SRGB.spv
%spirvopt% FLIP_LINE_A8R8G8B8_SRGB.spv -o FLIP_LINE_A8R8G8B8_SRGB.spv


%spirvgls% FLIP_TILE_A8R8G8B8_SRGB_NEO.comp -o FLIP_TILE_A8R8G8B8_SRGB_NEO.spv
%spirvopt% FLIP_TILE_A8R8G8B8_SRGB_NEO.spv -o FLIP_TILE_A8R8G8B8_SRGB_NEO.spv

pause
