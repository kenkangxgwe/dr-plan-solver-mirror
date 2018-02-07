#pragma once

#include "stdafx.h"
#include "Tree.h"
#include "Factory.h"
#include "BlackBox.hpp"

/**
 * The DR-plan Node class.
 */
namespace DRPlan {

class Node {
	friend class Tree<Node>;
	friend class Factory<Node>;

public:
	~Node();

private:
	Node();
	Node(Polynomial, std::vector<unsigned>, unsigned);

	Node *parentNode;
	std::vector<Node *> subNodes;
	std::vector<unsigned> freeVars; ///< The variables that appear in the sub nodes and remain free after solving.
	Polynomial targetPoly; ///< The polynomial that will be solved at the node.
	unsigned targetVar; ///< The index of the variable that will be solved from the target polynomial.

	/**
	 * Use the free variables and the target variable to substitute the other variables
	 * that appear in current polynomial. (Currying)
	 * @param solutionList a map that stores substitute functions between variables.
	 * @return The curried polynomial
	 */
	Polynomial curryPoly(const std::unordered_map<unsigned , Polynomial>&) const;
};

}  // namespace DRPlan
