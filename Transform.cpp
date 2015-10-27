#include "Transform.hpp"

#include <iostream>
#include <sstream>

#include "clang/Tooling/ReplacementsYaml.h"

#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/YAMLTraits.h"

#include "helper.hpp"

using clang::tooling::CompilationDatabase;
using clang::tooling::TranslationUnitReplacements;
using clang::tooling::Replacement;
using clang::tooling::Replacements;

using namespace llvm;


namespace clang_rename {

cl::OptionCategory TransformsCategory("Transforms");


cl::opt<bool> Quiet("quiet",
                    cl::desc("Do not report colored AST in case of error"),
                    cl::cat(TransformsCategory));

cl::opt<std::string> From("from",
                          cl::desc("Symbol to rename"),
                          cl::cat(TransformsCategory));
cl::opt<std::string> To("to",
                        cl::desc("New name"),
                        cl::cat(TransformsCategory));

static cl::opt<bool> AllTransformation("all", cl::desc("Apply all transformations"), cl::cat(TransformsCategory));

static cl::opt<std::string> OutputDir("outputdir",
                                      cl::desc("<path> output dir."),
                                      cl::cat(TransformsCategory));

static std::string GetOutputDir() {
   if (OutputDir.empty())
      return "";

   std::string path = OutputDir;
   if (std::error_code EC = sys::fs::create_directory(path)) {
      std::cerr << "Error when create output directory (" << EC.value() << ")";
   }
   return path;
}

void copy_substr(const std::string::const_iterator& start,
                 const std::string::const_iterator& end, std::ostream& ostr) {
   for (std::string::const_iterator it = start; it != end; ++it)
      ostr << *it;
}

std::string replace_all(const std::string& str, const std::string& what, const std::string& with) {
   std::stringstream      sstr;
   std::string::size_type pos        = 0;
   std::string::size_type previous   = 0;
   std::string::size_type whatLength = what.length();

   while ((pos = str.find(what, pos)) != std::string::npos) {
      if (pos != 0)
         copy_substr(str.begin() + previous, str.begin() + pos, sstr);
      pos += whatLength;
      previous = pos;
      sstr << with;
   }

   if (previous != std::string::npos)
      copy_substr(str.begin() + previous, str.end(), sstr);

   return sstr.str();
}

bool Transform::addReplacement(const Replacement& replacement) {
   m_replacements.insert(replacement);
   return true;
}


template <typename It>
TranslationUnitReplacements BuildTURs(const std::string& mainfilepath,
                                      const std::string& context,
                                      It                 firstReplacement,
                                      It lastReplacement) {
   TranslationUnitReplacements TURs;
   TURs.MainSourceFile = mainfilepath;
   TURs.Context        = context;
   TURs.Replacements.assign(firstReplacement, lastReplacement);
   return TURs;
}

static void WriteReplacements(const Replacements& replacements) {
   if (replacements.empty())
      return;

   auto mainfilepath = replacements.begin()->getFilePath();

   auto filename = replace_all(sys::path::filename(mainfilepath).str(), ".", "_");

   std::stringstream outputPath;
   outputPath << GetOutputDir() << "/" << filename << ".yaml";
   std::string    ErrorInfo;
   raw_fd_ostream ReplacementsFile(outputPath.str().c_str(), ErrorInfo,
                                   llvm::sys::fs::F_None);
   if (!ErrorInfo.empty()) {
      std::cerr << "Error opening file: " << ErrorInfo << "\n";
      return;
   }

   std::stringstream context;
   context << "modernize of " << static_cast<std::string>(mainfilepath);

   yaml::Output YAML(ReplacementsFile);
   auto turs = BuildTURs(mainfilepath, context.str(), replacements.begin(), replacements.end());
   YAML << turs;
}

void Transform::serializeReplacements() {
   WriteReplacements(m_replacements);
}



Transforms::Transforms()
   : m_transforms()
   , m_options() {
}

void Transforms::registerOptions() {
   for (TransformFactoryRegistry::iterator I = TransformFactoryRegistry::begin(),
                                           E = TransformFactoryRegistry::end();
        I != E; ++I) {

      m_options[I->getName()] = std::make_unique<cl::opt<bool> >(I->getName(),
                                                                 cl::desc(I->getDesc()),
                                                                 cl::cat(TransformsCategory));
   }
}

void Transforms::apply(const CompilationDatabase& Compilations, const std::vector<std::string>& SourcePaths) {
   instanciateTransforms();

   if (m_transforms.empty()) {
      std::cout << "No transformation to apply." << std::endl;
      return;
   }

   for (const auto& t : m_transforms) {
      t->apply(Compilations, SourcePaths);
      t->serializeReplacements();
   }
}


void Transforms::instanciateTransforms() {
   for (TransformFactoryRegistry::iterator I = TransformFactoryRegistry::begin(),
                                           E = TransformFactoryRegistry::end();
        I != E; ++I) {

      if (*m_options[I->getName()] || AllTransformation) {
         auto factory = I->instantiate();
         m_transforms.emplace_back(factory->createTransform());
      }
   }
}
}
