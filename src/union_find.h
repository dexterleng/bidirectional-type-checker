#ifndef UNION_FIND_H
#define UNION_FIND_H
#include <vector>

#include "error.h"
#include "type.h"

class UnificationError : public Error {
public:
  UnificationError(const Type& typeA, const Type& typeB)
    : Error("Cannot unify types: " + typeA.toString() + " and " + typeB.toString()) {}
};

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
    auto newRootType = pickNewRootType(typeAOpt, typeBOpt);
    joinRoots(rootA, rootB, newRootType);
  }

  void setType(TypeVar var, const std::optional<std::shared_ptr<Type>>& type) {
    auto rootVar = find(var);
    auto rootType = getType(rootVar);
    auto newRootType = pickNewRootType(rootType, type);
    types[rootVar] = newRootType;
  }

private:
  std::optional<std::shared_ptr<Type>> pickNewRootType(const std::optional<std::shared_ptr<Type>>& typeAOpt, const std::optional<std::shared_ptr<Type>>& typeBOpt) {
    if (!typeAOpt.has_value() && !typeBOpt.has_value()) {
      return std::nullopt;
    }

    if (typeAOpt.has_value() && !typeBOpt.has_value()) {
      return typeAOpt;
    }

    if (!typeAOpt.has_value() && typeBOpt.has_value()) {
      return typeBOpt;
    }

    auto typeA = *typeAOpt;
    const auto& typeB = *typeBOpt;
    if (*typeA == *typeB) {
      return typeA;
    }

    throw UnificationError(*typeA, *typeB);
  }

  void joinRoots(const TypeVar rootA, const TypeVar rootB, std::optional<std::shared_ptr<Type>> newType) {
    // since there's no ranking, we arbitrarily pick rootA as the new root
    parents[rootB] = rootA;
    types[rootA] = std::move(newType);
  }
};

#endif //UNION_FIND_H
