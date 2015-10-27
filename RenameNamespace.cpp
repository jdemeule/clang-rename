#include "Transform.hpp"

#include <sstream>

#include "clang/AST/AST.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Rewrite/Core/Rewriter.h"

#include "helper.hpp"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

using namespace clang_rename;

const char* CurrentNS = "current-ns";

DeclarationMatcher makeRenameNSDeclMatcher(const std::string& name) {
   return namespaceDecl(hasName(name)).bind(CurrentNS);
}

class RenameNSCallback : public MatchFinder::MatchCallback {
public:
   RenameNSCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      SourceManager &SM = *Result.SourceManager;
      auto ns = Result.Nodes.getDeclAs<NamespaceDecl>(CurrentNS);
      ns->dump();
//      Owner.addReplacement(Replacement(ns->getLocation()));
//      Owner.addReplacementForCurrentTU(Replacement(SM, StartLoc, 0, ReplacementText));
   }

private:
   Transform& Owner;
};



const char* FullQualifiedVarDeclID = "full-qualified-vardecl";

DeclarationMatcher makeVarDeclNSMatcher(const std::string& ns) {
   return varDecl(hasType(elaboratedType(hasQualifier(nestedNameSpecifier(specifiesNamespace(hasName(ns)))))))
      .bind(FullQualifiedVarDeclID);
}

class VarDeclNSCallback : public MatchFinder::MatchCallback {
public:
   VarDeclNSCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {

   }

private:
   Transform& Owner;
};



const char* UsingDeclID = "using-decl";

DeclarationMatcher makeUsingNSMatcher(const std::string& ns) {
   return usingDecl(hasAnyUsingShadowDecl(hasTargetDecl(matchesName(ns + "::"))))
      .bind(UsingDeclID);
}

class UsingNSCallback : public MatchFinder::MatchCallback {
public:
   UsingNSCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {

   }

private:
   Transform& Owner;
};


class RenameNSTransform : public Transform {
public:
   ~RenameNSTransform() {}

   virtual int apply(const CompilationDatabase& Compilations,
                     const std::vector<std::string>& SourcePaths) override {

      RefactoringTool Tool(Compilations, SourcePaths);
      auto            diagConsumer = std::make_unique<IgnoringDiagConsumer>();

      if (Quiet)
         Tool.setDiagnosticConsumer(diagConsumer.get());

      RenameNSCallback  Callback(*this);
      VarDeclNSCallback VarDeclCallback(*this);
      UsingNSCallback   UsingCallback(*this);
      MatchFinder       Finder;

      Finder.addMatcher(makeRenameNSDeclMatcher(From), &Callback);
      Finder.addMatcher(makeVarDeclNSMatcher(From), &VarDeclCallback);
      Finder.addMatcher(makeUsingNSMatcher(From), &UsingCallback);


      int res = Tool.run(newFrontendActionFactory(&Finder).get());


      LangOptions DefaultLangOptions;
      IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
      TextDiagnosticPrinter DiagnosticPrinter(llvm::errs(), &*DiagOpts);
      DiagnosticsEngine Diagnostics(IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()),
                                    &*DiagOpts, &DiagnosticPrinter, false);
      SourceManager Sources(Diagnostics,  Tool.getFiles());
      Rewriter Rewrite(Sources, DefaultLangOptions);

      if (!tooling::applyAllReplacements(getReplacements(), Rewrite)) {
         llvm::errs() << "Skipped some replacements.\n";
      }

      return res;
   }
};



struct RenameNSTransformFactory : public TransformFactory {
   virtual std::unique_ptr<Transform> createTransform() const {
      return std::make_unique<RenameNSTransform>();
   }
};


static TransformFactoryRegistry::Add<RenameNSTransformFactory>
    X("rename-ns", "Rename a namespace");
