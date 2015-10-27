set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_executable(clang-rename
  ClangRename.cpp
  Transform.cpp
  Transform.hpp
  RenameNamespace.cpp
  )

target_link_libraries(clang-rename
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangTooling
  )
