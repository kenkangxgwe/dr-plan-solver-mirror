#pragma once

#include "stdafx.h"
#include "Polynomial.hpp"

/**
 * The DR-plan Node class.
 */
namespace DRPlan {

class Node {
	friend class Tree;
	friend class Factory;

public:
	~Node();

private:
	Node();
	Node(Polynomial, std::vector<int>, int);

	Node *parentNode;
	std::vector<Node *> subNodes;
	std::vector<int> freeVars; ///< The variables that appear in the sub nodes and remain free after solving.
	Polynomial targetPoly; ///< The polynomial that will be solved at the node.
	int targetVar; ///< The index of the variable that will be solved from the target polynomial.

	/**
	 * Use the free variables and the target variable to substitute the other variables
	 * that appear in current polynomial. (Currying)
	 * @param solutionList a map that stores substitute functions between variables.
	 * @return The curried polynomial
	 */
	Polynomial curryPoly(const std::unordered_map<int, Polynomial>&) const;
};

}  // namespace DRPlan
