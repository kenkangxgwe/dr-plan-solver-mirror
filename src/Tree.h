#pragma once

#include "stdafx.h"
#include "Node.h"
#include "Polynomial.hpp"

namespace DRPlan {

class Tree {
    friend class Factory;
	friend class Node;

public:
    Tree();
    Tree(Node*, int, std::unordered_map<int, std::pair<double, double>>);
    ~Tree();

    std::vector<std::unordered_map<int, double>> solveTree();
    std::vector<std::unordered_map<int, double>> finalSolutionLists; /**< A list of maps that stores all the possible solutions.
																	 Each solution maps a variable index to its corresponding value.*/

private:
    Node *root;
    int sampleNum; ///< The number of samples for interpolation.
    std::unordered_map<int, std::pair<double, double>> intervalList; ///< A map that stores the interval of the interpolation samples.
    std::vector<std::unordered_map<int, Polynomial>> solutionMapList; /**< A list of maps that stores all current possible solutions.
																	Each solution map contains the representation for each variable
																	which only involves the free variables .
																	*/

    void solveNode(Node *);

	/**
	 * Interpolates the polynomial to solve the roots
	 * and represent the target variable in terms of free variables.
	 * @param poly the polynomial to solve.
	 * @param solvedVar the index of the target variable.
	 * @return A list of representations of the target variable.
	 */
    std::vector<Polynomial> interpolate(Polynomial, int) const;

	/**
	 * Uses the free variables to replace the target variable wherever it occurs in the previous solutionMap.
	 * @param solutionMap the solution map where the representation was solved.
	 * @param reps the representation of the target variable in terms of the free variables.
	 * @param targetVar the index of the target variable.
	 * @return the new solutionMap with no target variable appears.
	 */
	std::unordered_map<int, Polynomial> updateSolution(const std::unordered_map<int, Polynomial>&, const Polynomial&, int) const;
};

}