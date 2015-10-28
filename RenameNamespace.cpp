#include "Transform.hpp"

#include <iostream>
#include <sstream>

#include "clang/AST/AST.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"

#include "helper.hpp"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

using namespace clang_rename;


namespace {

static std::string replace_all(const std::string& str, const std::string& what, const std::string& with) {
   std::stringstream      sstr;
   std::string::size_type pos        = 0;
   std::string::size_type previous   = 0;
   std::string::size_type whatLength = what.length();

   while ((pos = str.find(what, pos)) != std::string::npos) {
      if (pos != 0)
         std::copy(str.begin() + previous, str.begin() + pos, std::ostream_iterator<char>(sstr));
      pos += whatLength;
      previous = pos;
      sstr << with;
   }

   if (previous != std::string::npos)
      std::copy(str.begin() + previous, str.end(), std::ostream_iterator<char>(sstr));

   return sstr.str();
}

static std::vector<std::string> split_by(const std::string& str, const std::string& what) {
   std::vector<std::string> values;
   std::string::size_type   pos        = 0;
   std::string::size_type   previous   = 0;
   std::string::size_type   whatLength = what.length();

   while ((pos = str.find(what, pos)) != std::string::npos) {
      if (pos != 0)
         values.push_back(std::string(str.begin() + previous, str.begin() + pos));
      pos += whatLength;
      previous = pos;
   }

   if (previous != std::string::npos)
      values.push_back(std::string(str.begin() + previous, str.end()));

   return values;
}
}


const char* CurrentNS = "current-ns";

DeclarationMatcher makeRenameNSDeclMatcher(const std::string& name) {
   return namespaceDecl(hasName(name)).bind(CurrentNS);
}

class RenameNSCallback : public MatchFinder::MatchCallback {
public:
   RenameNSCallback(Transform& T, std::vector<std::string>& from_ns, std::vector<std::string>& to_ns)
      : Owner(T)
      , NS_from(from_ns)
      , NS_to(to_ns) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      SourceManager &SM = *Result.SourceManager;
      auto ns = Result.Nodes.getDeclAs<NamespaceDecl>(CurrentNS);
      ns->dump();
      auto s = ns->getLocStart();
      auto r = ns->getRBraceLoc();
      auto l = ns->getLocation();


      std::stringstream decl_replacement;

      // ok if not increasing namespace deep.
      if (NS_from.size() == NS_to.size()) {
         decl_replacement << "namespace " << NS_to.back();
      }
      if (NS_from.size() < NS_to.size()) {
         bool first = true;
         std::for_each(std::next(NS_to.begin(), NS_from.size() - 1),
                       NS_to.end(),
                       [&decl_replacement, &first](const std::string& ns) {
                          decl_replacement << (first ? "" : " {\n") << "namespace " << ns;
                          first = false;
                       });
      }

      Owner.addReplacement(Replacement(SM,
                                       CharSourceRange::getTokenRange(s, l),
                                       decl_replacement.str()));

      if (NS_from.size() < NS_to.size()) {
         std::stringstream close_replacement;
         const char* closing_ns = "}\n";
         for (std::size_t i = 0, e = NS_to.size() - NS_from.size(); i != e; ++i)
            close_replacement << closing_ns;
         Owner.addReplacement(Replacement(SM, r, 0, close_replacement.str()));
      }
   }

private:
   Transform& Owner;
   std::vector<std::string>& NS_from;
   std::vector<std::string>& NS_to;
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



      std::string from_ns = From;
      std::string to_ns = To;

      auto from_ns_v = split_by(from_ns, "::");
      auto to_ns_v   = split_by(to_ns, "::");

      // assert from_ns_v[-1] � to_ns_v[-1]

      RenameNSCallback  Callback(*this, from_ns_v, to_ns_v);
      VarDeclNSCallback VarDeclCallback(*this);
      UsingNSCallback   UsingCallback(*this);
      MatchFinder       Finder;

      Finder.addMatcher(makeRenameNSDeclMatcher(From), &Callback);
      Finder.addMatcher(makeVarDeclNSMatcher(From), &VarDeclCallback);
      Finder.addMatcher(makeUsingNSMatcher(From), &UsingCallback);


      int res = Tool.run(newFrontendActionFactory(&Finder).get());

      // if stdout

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
                          x.second.write(llvm::raw_os_ostream(std::cout));
                       });
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
