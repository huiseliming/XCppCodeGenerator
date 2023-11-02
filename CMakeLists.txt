set(LLVM_LINK_COMPONENTS support)

add_clang_executable(XCppCodeGenerator Main.cpp)

set_property(TARGET XCppCodeGenerator PROPERTY CXX_STANDARD 20)

target_link_libraries(XCppCodeGenerator
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
)
