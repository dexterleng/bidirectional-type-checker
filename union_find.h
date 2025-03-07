#ifndef UNION_FIND_H
#define UNION_FIND_H
#include <vector>

#include "type.h"

// TODO(perf): implement ranking
class UnionFind {
  std::vector<std::optional<std::shared_ptr<Type>>> types;
  std::vector<TypeVar> parents;

public:
  TypeVar insert(const std::optional<std::shared_ptr<Type>>& type) {
    types.push_back(type);
    TypeVar typeVar = types.size() - 1;
    parents.push_back(typeVar);
    return typeVar;
  }

  std::optional<std::shared_ptr<Type>> getType(TypeVar typeVar) {
    auto rootTypeVar = find(typeVar);
    return types[rootTypeVar];
  }

  TypeVar find(TypeVar typeVar) {
    auto curr = typeVar;
    while (parents[curr] != curr) {
      parents[curr] = parents[parents[curr]]; // path compression
      curr = parents[curr];
    }
    return curr;
  }

  void join(TypeVar a, TypeVar b) {
    auto rootA = find(a);
    auto rootB = find(b);
    if (rootA == rootB) { // same set; nothing to merge
      return;
    }
    auto typeAOpt = getType(rootA);
    auto typeBOpt = getType(rootB);
    // if both are null, pick any to be the root, type is null
    // if typeA is null, typeB is not, rootB is the root, typeB is the value. vice versa.
    // if both are IntegerType, pick any to be the root, type is IntegerType
    // if both are FunctionType, pick any to be the root, type is IntegerType
    // if both are VariableType, pick any to be the root,

    // let combined = V::unify_values(&self.value(root_a).value, &self.value(root_b).value)?;
    //
    // Ok(self.unify_roots(root_a, root_b, combined))

  //   fn unify_values(a: &Option<V>, b: &Option<V>) -> Result<Self, V::Error> {
  //     match (a, b) {
  //       (&None, &None) => Ok(None),
  //       (&Some(ref v), &None) | (&None, &Some(ref v)) => Ok(Some(v.clone())),
  //       (&Some(ref a), &Some(ref b)) => match V::unify_values(a, b) {
  //         Ok(v) => Ok(Some(v)),
  //         Err(err) => Err(err),
  //     },
  // }
  //   }

    if (!typeAOpt.has_value() && !typeBOpt.has_value()) {
      joinRoots(rootA, rootB, std::nullopt);
      return;
    }

    if (typeAOpt.has_value() && !typeBOpt.has_value()) {
      joinRoots(rootA, rootB, typeAOpt);
      return;
    }

    if (!typeAOpt.has_value() && typeBOpt.has_value()) {
      joinRoots(rootA, rootB, typeBOpt);
      return;
    }

    auto typeA = *typeAOpt;
    auto typeB = *typeBOpt;
    if (typeA == typeB || *typeA == *typeB) {

    }
  }

private:
  void joinRoots(TypeVar rootA, TypeVar rootB, std::optional<std::shared_ptr<Type>> newType) {
    // since there's no ranking, we arbitrarily pick rootA as the new root
    parents[rootB] = rootA;
    types[rootA] = newType;
  }
};

#endif //UNION_FIND_H
