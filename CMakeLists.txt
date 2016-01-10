set(LLVM_LINK_COMPONENTS
  Support
  )

add_clang_executable(clang-rename-2
  ClangRename.cpp
  Transform.cpp
  Transform.hpp
  RenameNamespace.cpp
  RenameMacro.cpp
  ChangeIncludePrefix.cpp
  helper.hpp
  )

target_link_libraries(clang-rename-2
  clangAST
  clangASTMatchers
  clangBasic
  clangFrontend
  clangTooling
  )
