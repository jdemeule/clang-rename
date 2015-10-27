#include "Transform.hpp"

#include <sstream>

#include "clang/AST/AST.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Refactoring.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

using namespace mef_modernize;

const char* MethodId = "explore-dependency-method";

DeclarationMatcher makeExploreDependencyFunctionDeclMatcher() {
   return functionDecl(
              hasName("ExploreDependency"),
              parameterCountIs(2))
       .bind(MethodId);
}

class ImplementsDependencyExplorerCallback : public MatchFinder::MatchCallback {
public:
   ImplementsDependencyExplorerCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      auto ExploreDependency_method = Result.Nodes.getNodeAs<FunctionDecl>(MethodId);
      if (!ExploreDependency_method)
         return;

      //ExploreDependency_method->dump();
      auto p0 = ExploreDependency_method->getParamDecl(0);

      std::stringstream pxThis_parameter;
      pxThis_parameter << MefClassName << "* /*pxThis*/, ";

      Owner.addReplacement(Replacement(*(Result.SourceManager),
                                       p0->getLocStart(),
                                       0,
                                       pxThis_parameter.str()));
   }

private:
   Transform& Owner;
};

class ImplementsDependencyExplorerTransform : public Transform {
public:
   virtual int apply(const CompilationDatabase& Compilations,
                     const std::vector<std::string>& SourcePaths) override {

      RefactoringTool Tool(Compilations, SourcePaths);
      auto            diagConsumer = std::make_unique<IgnoringDiagConsumer>();

      if (Quiet)
         Tool.setDiagnosticConsumer(diagConsumer.get());

      ImplementsDependencyExplorerCallback Callback(*this);

      MatchFinder Finder;

      Finder.addMatcher(makeExploreDependencyFunctionDeclMatcher(),
                        &Callback);

      return Tool.run(newFrontendActionFactory(&Finder).get());
   }
};



struct ImplementsDependencyExplorerTransformFactory : public TransformFactory {
   virtual std::unique_ptr<Transform> createTransform() const {
      return std::make_unique<ImplementsDependencyExplorerTransform>();
   }
};


static TransformFactoryRegistry::Add<ImplementsDependencyExplorerTransformFactory>
    X("implements-dependency-explorer", "Find ExploreDependency and implements mxSystemDataIDEPENDENCY_EXPLORER instead");
