cd %~dp0
"%ProgramFiles%\Microsoft Visual Studio\2022\Community\Common7\Tools\VsDevCmd.bat" -arch=amd64 &^
cd ../../ &^
cmake -G "Visual Studio 17 2022" -S ./llvm -B ./build -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DLLVM_BUILD_TESTS=OFF -DCMAKE_CXX_FLAGS="/utf-8" -DCMAKE_C_FLAGS="/utf-8"


