#include "Transform.hpp"

#include <iostream>
#include <sstream>

#include "clang/AST/AST.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Refactoring.h"
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

template <typename It>
static std::string join_with(It first, It last, const std::string& with) {
   std::stringstream buffer;
   for (It it = first; it != last; ++it) {
      if (it != first)
         buffer << with;
      buffer << *it;
   }
   return buffer.str();
}
}


const char* NamespaceDeclID = "current-ns";

DeclarationMatcher makeRenameNSDeclMatcher(const std::string& name) {
   return namespaceDecl(hasName(name)).bind(NamespaceDeclID);
}

const char* OpenNS_STD     = " {\n";
const char* CloseNS_STD    = "}\n";
const char* OpenNS_INLINE  = " { ";
const char* CloseNS_INLINE = "} ";

class RenameNSCallback : public MatchFinder::MatchCallback {
public:
   RenameNSCallback(Transform& T, std::vector<std::string>& from_ns, std::vector<std::string>& to_ns)
      : Owner(T)
      , NS_from(from_ns)
      , NS_to(to_ns) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      SourceManager& SM = *Result.SourceManager;

      auto ns = Result.Nodes.getDeclAs<NamespaceDecl>(NamespaceDeclID);
      auto s  = ns->getLocStart();
      auto r  = ns->getRBraceLoc();
      auto l  = ns->getLocation();

      std::stringstream decl_replacement;

      const char* openNS  = s.isMacroID() ? OpenNS_INLINE : OpenNS_STD;
      const char* closeNS = r.isMacroID() ? CloseNS_INLINE : CloseNS_STD;

      // ok if not increasing namespace deep.
      if (NS_from.size() == NS_to.size()) {
         decl_replacement << "namespace " << NS_to.back();
      }
      if (NS_from.size() < NS_to.size()) {
         bool first = true;
         std::for_each(std::next(NS_to.begin(), NS_from.size() - 1),
                       NS_to.end(),
                       [&decl_replacement, &first, openNS](const std::string& ns) {
                          decl_replacement << (first ? "" : openNS) << "namespace " << ns;
                          first = false;
                       });
      }

      Owner.addReplacement(Replacement(SM,
                                       CharSourceRange::getTokenRange(s, l),
                                       decl_replacement.str()));

      if (NS_from.size() < NS_to.size()) {
         std::stringstream close_replacement;
         for (std::size_t i = 0, e = NS_to.size() - NS_from.size() + 1; i != e; ++i)
            close_replacement << closeNS;

         Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(r, r), close_replacement.str()));
      }
   }

private:
   Transform&                Owner;
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
   VarDeclNSCallback(Transform& T, std::vector<std::string>& from_ns, std::vector<std::string>& to_ns)
      : Owner(T)
      , NS_from(from_ns)
      , NS_to(to_ns) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      // replace "foo::baz::Tata x;" by "foo::xxx::Tata x;"
      SourceManager& SM = *Result.SourceManager;

      auto var  = Result.Nodes.getDeclAs<VarDecl>(FullQualifiedVarDeclID);
      auto s    = var->getLocStart();
      auto e    = var->getLocEnd();
      auto type = var->getType().getAsString();

      if (s.isMacroID())
         return;

      auto type_v = split_by(type, "::");
      auto deep = NS_from.size() - (type_v.size() - 1);

      std::string to_replace = join_with(std::next(NS_from.begin(), deep), NS_from.end(), "::");
      std::string new_type = join_with(std::next(NS_to.begin(), deep), NS_to.end(), "::");


      std::stringstream new_decl;
      new_decl << new_type << type.substr(to_replace.size()) << " " << var->getNameAsString();


      Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(s, e), new_decl.str()));
   }

private:
   Transform&                Owner;
   std::vector<std::string>& NS_from;
   std::vector<std::string>& NS_to;
};

const char* FullQualifiedFieldDeclID = "full-qualified-fielddecl";

DeclarationMatcher makeFieldDeclNSMatcher(const std::string& ns) {
   return fieldDecl(hasType(elaboratedType(hasQualifier(nestedNameSpecifier(specifiesNamespace(hasName(ns)))))))
       .bind(FullQualifiedFieldDeclID);
}

class FieldDeclNSCallback : public MatchFinder::MatchCallback {
public:
   FieldDeclNSCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      SourceManager& SM = *Result.SourceManager;

      auto var  = Result.Nodes.getDeclAs<FieldDecl>(FullQualifiedFieldDeclID);
      auto s    = var->getLocStart();
      auto e    = var->getLocEnd();
      auto type = var->getType().getAsString();

      if (s.isMacroID())
         return;

      std::stringstream new_decl;
      new_decl << replace_all(type, From, To) << " " << var->getNameAsString();

      Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(s, e), new_decl.str()));
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
      // replace "using foo::baz::Tata" by "using foo::zzz::Tata"
      SourceManager& SM = *Result.SourceManager;

      auto u = Result.Nodes.getDeclAs<UsingDecl>(UsingDeclID);
      auto s = u->getLocStart();
      auto e = u->getLocEnd();

      if (s.isMacroID())
         return;

      auto q = u->getQualifier();
      while (q->getPrefix()) {
         q = q->getPrefix();
      }

      auto kind = q->getKind();

      std::stringstream buffer;
      buffer << "using " << (kind == NestedNameSpecifier::Global ? "::" : "") << To << "::" << u->getNameAsString();

      Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(s, e), buffer.str()));
   }

private:
   Transform& Owner;
};



const char* UsingDirectiveDeclID = "using-directive-decl";

DeclarationMatcher makeUsingDirectiveNSMatcher(const std::string& ns) {
   return usingDirectiveDecl()
       .bind(UsingDirectiveDeclID);
}

class UsingDirectiveNSCallback : public MatchFinder::MatchCallback {
public:
   UsingDirectiveNSCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      // replace "using foo::baz::Tata" by "using foo::zzz::Tata"
      SourceManager& SM = *Result.SourceManager;

      auto usingNS = Result.Nodes.getDeclAs<UsingDirectiveDecl>(UsingDirectiveDeclID);
      auto qname   = usingNS->getNominatedNamespace()->getQualifiedNameAsString();

      auto s = usingNS->getLocStart();
      auto e = usingNS->getLocEnd();

      if (s.isMacroID())
         return;

      if (qname != From)
         return;

      std::stringstream buffer;
      buffer << "using namespace " << To;

      Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(s, e), buffer.str()));
   }

private:
   Transform& Owner;
};



const char* NamespaceAliasDeclID = "namespace-alias-decl";

DeclarationMatcher makeNamespaceAliasDeclMatcher(const std::string& ns) {
   return namespaceAliasDecl()
       .bind(NamespaceAliasDeclID);
}

class NamespaceAliasDeclCallback : public MatchFinder::MatchCallback {
public:
   NamespaceAliasDeclCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      // replace "using foo::baz::Tata" by "using foo::zzz::Tata"
      SourceManager& SM = *Result.SourceManager;

      auto aliasNS = Result.Nodes.getDeclAs<NamespaceAliasDecl>(NamespaceAliasDeclID);
      auto qname   = aliasNS->getNamespace()->getQualifiedNameAsString();

      auto s = aliasNS->getLocStart();
      auto e = aliasNS->getLocEnd();

      if (s.isMacroID())
         return;

      if (qname != From)
         return;

      std::stringstream buffer;
      buffer << "namespace " << aliasNS->getNameAsString() << " = " << To;

      Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(s, e), buffer.str()));
   }

private:
   Transform& Owner;
};


const char* TypedefDeclID = "typedef-decl";

DeclarationMatcher makeTypeDefeclMatcher(const std::string& ns) {
   // filtering should be done later or with a more complex matcher
   return typedefDecl()
       .bind(TypedefDeclID);
}

class TypedefDeclCallback : public MatchFinder::MatchCallback {
public:
   TypedefDeclCallback(Transform& T, std::vector<std::string>& from_ns, std::vector<std::string>& to_ns)
      : Owner(T)
      , NS_from(from_ns)
      , NS_to(to_ns) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      // replace "using foo::baz::Tata" by "using foo::zzz::Tata"
      SourceManager& SM = *Result.SourceManager;

      auto TD    = Result.Nodes.getDeclAs<TypedefDecl>(TypedefDeclID);
      auto u     = TD->getUnderlyingType();
      auto qname = u.getAsString();

      auto s = TD->getLocStart();
      auto e = TD->getLocEnd();

      if (s.isMacroID())
         return;

      if (qname.find(From) == std::string::npos)
         return;


      auto ctx = TD->getDeclContext();
      while (ctx && ctx->getDeclKind() != Decl::Namespace) {
         if (ctx->getDeclKind() == Decl::ClassTemplateSpecialization) {
            return;
         }
         ctx = ctx->getParent();
      }

      std::string enclosingNamespace = "";
      if (ctx) {
         if (auto enclosingNS = dyn_cast<NamespaceDecl>(ctx))
            enclosingNamespace = enclosingNS->getQualifiedNameAsString();
      }

      std::stringstream buffer;

      if (enclosingNamespace.find(From) == 0) {
         return;
         //auto qname_v = split_by(replace_all(qname, "struct ", ""), "::");
         //auto currentns_v = split_by(enclosingNamespace, "::");
         //buffer << "typedef " << join_with(std::next(qname_v.begin(), currentns_v.size()), qname_v.end(), "::") << " " << TD->getNameAsString();
      } else
         buffer << "typedef " << replace_all(qname, From, To) << " " << TD->getNameAsString();


      Owner.addReplacement(Replacement(SM, CharSourceRange::getTokenRange(s, e), buffer.str()));
   }

private:
   Transform&                Owner;
   std::vector<std::string>& NS_from;
   std::vector<std::string>& NS_to;
};

const char* NestedNameSpecifierID = "nestedname-specifier-decl";

NestedNameSpecifierLocMatcher makeNestedNameSpecifierMatcher(const std::string& ns) {
   // filtering should be done later or with a more complex matcher
   return loc(specifiesNamespace(hasName(ns)))
       .bind(NestedNameSpecifierID);
}

class NestedNameSpecifierCallback : public MatchFinder::MatchCallback {
public:
   NestedNameSpecifierCallback(Transform& T)
      : Owner(T) {}

   virtual void run(const MatchFinder::MatchResult& Result) {
      // replace "foo::baz::" by "foo::zzz::"
      SourceManager& SM = *Result.SourceManager;

      auto specifier     = Result.Nodes.getDeclAs<NestedNameSpecifierLoc>(NestedNameSpecifierID);
      auto IdentifierLoc = specifier->getBeginLoc();

      if (IdentifierLoc.isMacroID())
         IdentifierLoc = SM.getSpellingLoc(IdentifierLoc);


      while (true) {
         llvm::SmallVector<char, 8> Buffer;
         bool                       Invalid = false;
         llvm::StringRef spelling = Lexer::getSpelling(IdentifierLoc, Buffer, SM, LangOptions(), &Invalid);
         if (Invalid)
            return;
         if (spelling[0] == ':') {
            IdentifierLoc = IdentifierLoc.getLocWithOffset(2);
            continue;
         }
         if (spelling != From)
            return;
         break;
      }

      Owner.addReplacement(Replacement(SM, IdentifierLoc, From.size(), To));
   }

private:
   Transform& Owner;
};


class RenameNSTransform : public Transform {
public:
   virtual ~RenameNSTransform() {}

   virtual int apply(const CompilationDatabase& Compilations,
                     const std::vector<std::string>& SourcePaths) override {

      RefactoringTool Tool(Compilations, SourcePaths);
      auto            diagConsumer = std::make_unique<IgnoringDiagConsumer>();

      if (Quiet)
         Tool.setDiagnosticConsumer(diagConsumer.get());

      std::string from_ns = From;
      std::string to_ns   = To;

      auto from_ns_v = split_by(from_ns, "::");
      auto to_ns_v   = split_by(to_ns, "::");


      RenameNSCallback           Callback(*this, from_ns_v, to_ns_v);
      VarDeclNSCallback          VarCallback(*this, from_ns_v, to_ns_v);
      FieldDeclNSCallback        FieldCallback(*this);
      UsingNSCallback            UsingCallback(*this);
      UsingDirectiveNSCallback   UsingDirectiveCallback(*this);
      NamespaceAliasDeclCallback NamespaceAliasCallback(*this);
      //TypedefDeclCallback        TypedefCallback(*this, from_ns_v, to_ns_v);
      NestedNameSpecifierCallback NestedNameCallback(*this);

      MatchFinder Finder;

      Finder.addMatcher(makeRenameNSDeclMatcher(From), &Callback);
      Finder.addMatcher(makeVarDeclNSMatcher(From), &VarCallback);
      Finder.addMatcher(makeFieldDeclNSMatcher(From), &FieldCallback);
      Finder.addMatcher(makeUsingNSMatcher(From), &UsingCallback);
      Finder.addMatcher(makeUsingDirectiveNSMatcher(From), &UsingDirectiveCallback);
      Finder.addMatcher(makeNamespaceAliasDeclMatcher(From), &NamespaceAliasCallback);
      //Finder.addMatcher(makeTypeDefeclMatcher(From), &TypedefCallback);
      Finder.addMatcher(makeNestedNameSpecifierMatcher(From), &NestedNameCallback);


      int res = Tool.run(newFrontendActionFactory(&Finder).get());

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

      return res;
   }
};



struct RenameNSTransformFactory : public TransformFactory {
   virtual ~RenameNSTransformFactory() {}

   virtual std::unique_ptr<Transform> createTransform() const {
      return std::make_unique<RenameNSTransform>();
   }
};


static TransformFactoryRegistry::Add<RenameNSTransformFactory>
    X("rename-ns", "Rename a namespace");
