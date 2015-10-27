set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_executable(ClangRename
  ClangRename.cpp
  Transform.cpp
  Transform.hpp
  RenameNamespace.cpp
  )

target_link_libraries(ClangRename
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangTooling
  )
