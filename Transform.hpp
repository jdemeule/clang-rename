#ifndef CLANG_RENAME_TRANSFORM_HPP
#define CLANG_RENAME_TRANSFORM_HPP

#include <map>
#include <memory>
#include <string>

#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Registry.h"

namespace clang_rename {

class Transform {
public:
   virtual int apply(const clang::tooling::CompilationDatabase& Compilations,
                     const std::vector<std::string>& SourcePaths) = 0;

   bool addReplacement(const clang::tooling::Replacement& replacement);

   void serializeReplacements();

private:
   clang::tooling::Replacements m_replacements;
};



struct TransformFactory {
   virtual std::unique_ptr<Transform> createTransform() const = 0;
};

typedef llvm::Registry<TransformFactory> TransformFactoryRegistry;


class Transforms {
public:
   Transforms();

   void registerOptions();

   void apply(const clang::tooling::CompilationDatabase& Compilations,
              const std::vector<std::string>& SourcePaths);

private:
   void instanciateTransforms();

private:
   typedef std::vector<std::unique_ptr<Transform> >                      TransformsInstances;
   typedef std::map<std::string, std::unique_ptr<llvm::cl::opt<bool> > > OptionsMap;

   TransformsInstances m_transforms;
   OptionsMap          m_options;
};

extern llvm::cl::OptionCategory   TransformsCategory;
extern llvm::cl::opt<bool>        Quiet;

extern llvm::cl::opt<std::string> From;
extern llvm::cl::opt<std::string> To;

}

#endif
