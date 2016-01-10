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

// Get a "file:line:column" source location string.
static std::string getSourceLocationString(clang::Preprocessor &PP,
                                           clang::SourceLocation Loc) {
   if (Loc.isInvalid())
      return std::string("(none)");

   if (Loc.isFileID()) {
      clang::PresumedLoc PLoc = PP.getSourceManager().getPresumedLoc(Loc);

      if (PLoc.isInvalid()) {
         return std::string("(invalid)");
      }

      std::string Str;
      llvm::raw_string_ostream SS(Str);

      // The macro expansion and spelling pos is identical for file locs.
      SS << "\"" << PLoc.getFilename() << ':' << PLoc.getLine() << ':'
         << PLoc.getColumn() << "\"";

      std::string Result = SS.str();

      // YAML treats backslash as escape, so use forward slashes.
      std::replace(Result.begin(), Result.end(), '\\', '/');

      return Result;
   }

   return std::string("(nonfile)");
}

enum class MacroTransformationKind {
   Exact,
   Prefix
};

class MacroTransform : public Transform {
public:
   MacroTransform(MacroTransformationKind transformation);

   virtual int apply(const CompilationDatabase& Compilations,
                     const std::vector<std::string>& SourcePaths) override;

   MacroTransformationKind Transformation;
};



struct MacroReplacement {
   std::string new_macro;
   std::size_t original_size;
   bool        replace;
};


MacroReplacement BuildMacroReplacement(Preprocessor& PP, const Token& MacroNameTok, MacroTransformationKind Transformation) {
   auto spelling = PP.getSpelling(MacroNameTok);

   std::stringstream m;

   if (Transformation == MacroTransformationKind::Exact) {
      if (spelling != From)
         return MacroReplacement({"", 0, false});

      m << To;
   }
   else {
      if (spelling.find(From) != 0)
         return MacroReplacement({ "", 0, false });

      m << To << spelling.substr(From.size());
   }
   return MacroReplacement({m.str(), spelling.size(), true});
}

class PPRenameMacroTracker : public PPCallbacks {
public:
   PPRenameMacroTracker(Preprocessor& pp, MacroTransform& T)
      : PP(pp)
      , Owner(T) {}

   virtual ~PPRenameMacroTracker() {}

   virtual void MacroExpands(const clang::Token &MacroNameTok,
                             const clang::MacroDefinition &MD, clang::SourceRange Range,
                             const clang::MacroArgs *Args) override {
      TryReplace(MacroNameTok);
   }

   virtual void MacroDefined(const Token& MacroNameTok,
                             const MacroDirective* MD) override {
      TryReplace(MacroNameTok);
   }

   virtual void MacroUndefined(const clang::Token &MacroNameTok,
                               const clang::MacroDefinition &MD) override {
      TryReplace(MacroNameTok);
   }

   virtual void Defined(const clang::Token &MacroNameTok,
                        const clang::MacroDefinition &MD,
                        clang::SourceRange Range) override {
      TryReplace(MacroNameTok);
   }

   virtual void Ifdef(clang::SourceLocation Loc, const clang::Token &MacroNameTok,
                      const clang::MacroDefinition &MD) override {
      TryReplace(MacroNameTok);
   }

   virtual void Ifndef(clang::SourceLocation Loc, const clang::Token &MacroNameTok,
                       const clang::MacroDefinition &MD) override {
      TryReplace(MacroNameTok);
   }

   void TryReplace(const Token& MacroNameTok) {
      auto rep = BuildMacroReplacement(PP, MacroNameTok, Owner.Transformation);
      if (!rep.replace)
         return;

      Owner.addReplacement(Replacement(PP.getSourceManager(),
                                       MacroNameTok.getLocation(),
                                       rep.original_size,
                                       rep.new_macro));
   }

private:
   Preprocessor&   PP;
   MacroTransform& Owner;
};

class PPRenameMacroConsumer : public ASTConsumer {
public:
   PPRenameMacroConsumer(Preprocessor& PP, MacroTransform& T) {
      // PP takes ownership.
      PP.addPPCallbacks(llvm::make_unique<PPRenameMacroTracker>(PP, T));
   }
};

class PPRenameMacroAction : public SyntaxOnlyAction {
public:
   PPRenameMacroAction(MacroTransform& T)
      : Owner(T) {}

protected:
   virtual std::unique_ptr<clang::ASTConsumer>
   CreateASTConsumer(CompilerInstance& CI,
                     StringRef InFile) {
      return llvm::make_unique<PPRenameMacroConsumer>(CI.getPreprocessor(), Owner);
   }

private:
   MacroTransform& Owner;
};

class PPRenameMacroFrontendActionFactory : public FrontendActionFactory {
public:
   PPRenameMacroFrontendActionFactory(MacroTransform& T)
      : Owner(T) {}

   virtual PPRenameMacroAction* create() {
      return new PPRenameMacroAction(Owner);
   }
   MacroTransform& Owner;
};








MacroTransform::MacroTransform(MacroTransformationKind transformation)
   : Transformation(transformation) {}

int MacroTransform::apply(const CompilationDatabase& Compilations,
                          const std::vector<std::string>& SourcePaths) {
   ClangTool Tool(Compilations, SourcePaths);

   int HadErrors = Tool.run(new PPRenameMacroFrontendActionFactory(*this));
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

struct RenameMacroTransformFactory : public TransformFactory {
   virtual ~RenameMacroTransformFactory() {}

   virtual std::unique_ptr<Transform> createTransform() const {
      return std::make_unique<MacroTransform>(MacroTransformationKind::Exact);
   }
};

static TransformFactoryRegistry::Add<RenameMacroTransformFactory>
    R("rename-macro", "Replace macro (exact match)");




struct PrefixMacroTransformFactory : public TransformFactory {
   virtual ~PrefixMacroTransformFactory() {}

   virtual std::unique_ptr<Transform> createTransform() const {
      return std::make_unique<MacroTransform>(MacroTransformationKind::Prefix);
   }
};

static TransformFactoryRegistry::Add<PrefixMacroTransformFactory>
    M("rename-macro-prefix", "Replace macro prefix");
