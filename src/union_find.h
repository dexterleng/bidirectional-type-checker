#ifndef UNION_FIND_H
#define UNION_FIND_H
#include <utility>
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
    if (*typeA == *typeB) {
      joinRoots(rootA, rootB, typeA);
      return;
    }

    throw std::runtime_error("Failed to join.");
  }

private:
  void joinRoots(const TypeVar rootA, const TypeVar rootB, std::optional<std::shared_ptr<Type>> newType) {
    // since there's no ranking, we arbitrarily pick rootA as the new root
    parents[rootB] = rootA;
    types[rootA] = std::move(newType);
  }
};

#endif //UNION_FIND_H
