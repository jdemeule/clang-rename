#ifndef clang_rename_helper_hpp
#define clang_rename_helper_hpp

#include <memory>

#if __clang__

namespace std {
   template< class T, class... Args >
   unique_ptr<T> make_unique(Args&&... args) {
      return unique_ptr< T >(new T(std::forward<Args>(args)...));
   }
}

#endif

#endif
