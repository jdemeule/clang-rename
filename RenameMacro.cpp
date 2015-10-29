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

class PPCallbacksTracker : public PPCallbacks {
public:
   PPCallbacksTracker(Preprocessor& pp, MacroTransform& T)
      : PP(pp)
      , Owner(T) {}

   virtual ~PPCallbacksTracker() {}

   virtual void InclusionDirective(SourceLocation HashLoc,
                                   const Token &IncludeTok,
                                   StringRef FileName,
                                   bool IsAngled,
                                   CharSourceRange FilenameRange,
                                   const FileEntry *File,
                                   StringRef SearchPath,
                                   StringRef RelativePath,
                                   const Module *Imported) override {
      auto hloc = getSourceLocationString(PP, HashLoc);
      auto tloc = getSourceLocationString(PP, IncludeTok.getLocation());

      std::string boost = "boost";
      std::string target = FileName;
      std::stringstream recomputed;
      recomputed << (IsAngled ? '<' : '"') << "murex/mbl" << target.substr(boost.size()) << (IsAngled ? '>' : '"');

      Owner.addReplacement(Replacement(PP.getSourceManager(), FilenameRange, recomputed.str()));
   }

   virtual void MacroExpands(const Token& MacroNameTok, const MacroDirective* MD,
                             SourceRange Range, const MacroArgs* Args) override {
      TryReplace(MacroNameTok);
   }

   virtual void MacroDefined(const Token& MacroNameTok,
                             const MacroDirective* MD) override {
      TryReplace(MacroNameTok);
   }

   virtual void MacroUndefined(const Token& MacroNameTok,
                               const MacroDirective* MD) {
      TryReplace(MacroNameTok);
   }

   virtual void Defined(const Token& MacroNameTok, const MacroDirective* MD,
                        SourceRange Range) override {
      TryReplace(MacroNameTok);
   }

   virtual void Ifdef(SourceLocation Loc, const Token& MacroNameTok,
                      const MacroDirective* MD) override {
      TryReplace(MacroNameTok);
   }

   virtual void Ifndef(SourceLocation Loc, const Token& MacroNameTok,
                       const MacroDirective* MD) override {
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

class PPTraceConsumer : public ASTConsumer {
public:
   PPTraceConsumer(Preprocessor& PP, MacroTransform& T) {
      // PP takes ownership.
      PP.addPPCallbacks(new PPCallbacksTracker(PP, T));
   }
};

class PPTraceAction : public SyntaxOnlyAction {
public:
   PPTraceAction(MacroTransform& T)
      : Owner(T) {}

protected:
   virtual clang::ASTConsumer* CreateASTConsumer(CompilerInstance& CI,
                                                 StringRef InFile) {
      return new PPTraceConsumer(CI.getPreprocessor(), Owner);
   }

private:
   MacroTransform& Owner;
};

class PPTraceFrontendActionFactory : public FrontendActionFactory {
public:
   PPTraceFrontendActionFactory(MacroTransform& T)
      : Owner(T) {}

   virtual PPTraceAction* create() {
      return new PPTraceAction(Owner);
   }
   MacroTransform& Owner;
};








MacroTransform::MacroTransform(MacroTransformationKind transformation)
   : Transformation(transformation) {}

int MacroTransform::apply(const CompilationDatabase& Compilations,
                          const std::vector<std::string>& SourcePaths) {
   ClangTool Tool(Compilations, SourcePaths);

   int HadErrors = Tool.run(new PPTraceFrontendActionFactory(*this));
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
