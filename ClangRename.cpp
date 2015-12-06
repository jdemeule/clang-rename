//===---- tools/extra/ClangRename.cpp -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements a rename refactoring tool using the clang tooling.
//
//  Usage:
//  clang-rename [options] <source0> [... <sourceN>]
//
//  -change-include-prefix - Change include prefix but do not move files
//  -from=<string>         - Symbol to rename
//  -outputdir=<string>    - <path> output dir.
//  -p=<string>            - Build path
//  -quiet                 - Do not report colored AST in case of error
//  -rename-macro          - Replace macro (exact match)
//  -rename-macro-prefix   - Replace macro prefix
//  -rename-ns             - Rename a namespace
//  -stdout                - Print output to cout instead of file
//  -to=<string>           - New name
//
//===----------------------------------------------------------------------===//

#include <iostream>

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring.h"

#include "Transform.hpp"

using namespace clang;
using namespace clang::tooling;
using namespace clang_rename;


int main(int argc, const char **argv) {
   Transforms transforms;
   transforms.registerOptions();

   CommonOptionsParser op(argc, argv, TransformsCategory);

   transforms.apply(op.getCompilations(), op.getSourcePathList());

   return 0;
}
