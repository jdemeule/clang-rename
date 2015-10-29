#include "Transform.hpp"

#include <iostream>
#include <sstream>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "helper.hpp"

using namespace clang;
using namespace clang::tooling;

using namespace clang_rename;

class PPChangeIncludeTracker : public PPCallbacks {
public:
   PPChangeIncludeTracker(Preprocessor& pp, Transform& T)
      : PP(pp)
      , Owner(T) {}

   virtual ~PPChangeIncludeTracker() {}

   virtual void InclusionDirective(SourceLocation HashLoc,
                                   const Token&     IncludeTok,
                                   StringRef        FileName,
                                   bool             IsAngled,
                                   CharSourceRange  FilenameRange,
                                   const FileEntry* File,
                                   StringRef        SearchPath,
                                   StringRef        RelativePath,
                                   const Module* Imported) override {

      std::string target = FileName;
      if (target.find(From) != 0)
         return;

      std::stringstream recomputed;
      recomputed << (IsAngled ? '<' : '"') << To << target.substr(From.size()) << (IsAngled ? '>' : '"');

      Owner.addReplacement(Replacement(PP.getSourceManager(), FilenameRange, recomputed.str()));
   }


private:
   Preprocessor& PP;
   Transform&    Owner;
};

class PPChangeIncludeConsumer : public ASTConsumer {
public:
   PPChangeIncludeConsumer(Preprocessor& PP, Transform& T) {
      // PP takes ownership.
      PP.addPPCallbacks(new PPChangeIncludeTracker(PP, T));
   }
};

class PPChangeIncludeAction : public SyntaxOnlyAction {
public:
   PPChangeIncludeAction(Transform& T)
      : Owner(T) {}

protected:
   virtual clang::ASTConsumer* CreateASTConsumer(CompilerInstance& CI,
                                                 StringRef InFile) {
      return new PPChangeIncludeConsumer(CI.getPreprocessor(), Owner);
   }

private:
   Transform& Owner;
};

class PPChangeIncludeFrontendActionFactory : public FrontendActionFactory {
public:
   PPChangeIncludeFrontendActionFactory(Transform& T)
      : Owner(T) {}

   virtual PPChangeIncludeAction* create() {
      return new PPChangeIncludeAction(Owner);
   }
   Transform& Owner;
};


class ChangeIncludePrefixTransform : public Transform {
public:
   virtual int apply(const CompilationDatabase& Compilations,
                     const std::vector<std::string>& SourcePaths) override {
      ClangTool Tool(Compilations, SourcePaths);

      int HadErrors = Tool.run(new PPChangeIncludeFrontendActionFactory(*this));
      if (StdOut) {
         LangOptions                           DefaultLangOptions;
         IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
         TextDiagnosticPrinter DiagnosticPrinter(llvm::errs(), &*DiagOpts);
         DiagnosticsEngine Diagnostics(IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()),
                                       &*DiagOpts, &DiagnosticPrinter, false);
         SourceManager Sources(Diagnostics, Tool.getFiles());
         Rewriter Rewrite(Sources, DefaultLangOptions);

         if (!tooling::applyAllReplacements(getReplacements(), Rewrite)) {
            llvm::errs() << "Skipped some replacements.\n";
         } else {
            std::for_each(Rewrite.buffer_begin(),
                          Rewrite.buffer_end(),
                          [](const Rewriter::buffer_iterator::value_type& x) {
                             llvm::raw_os_ostream out(std::cout);
                             x.second.write(out);
                          });
         }
      }
      return 0;
   }
};



struct ChangeIncludePrefixTransformFactory : public TransformFactory {
   virtual ~ChangeIncludePrefixTransformFactory() {}

   virtual std::unique_ptr<Transform> createTransform() const {
      return std::make_unique<ChangeIncludePrefixTransform>();
   }
};

static TransformFactoryRegistry::Add<ChangeIncludePrefixTransformFactory>
    M("change-include-prefix", "Change include prefix but do not move files");
