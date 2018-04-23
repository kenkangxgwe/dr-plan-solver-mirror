/**
 * This file is part of DRPLAN.
 *
 * DRPLAN is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DRPLAN is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#include "stdafx.h"
#include "TwoTree.h"
#include "RootFinder.h"
#include "BlackBox.hpp"
#include "Enumeration.hpp"

namespace DRPLAN
{

class Tree
{

    typedef std::unordered_map<unsigned, double> DoubleMap;
    typedef std::unordered_map<unsigned, BlackBox<>> Solution;
    typedef std::unordered_map<unsigned, std::pair<double, double>> Domain;
    typedef BlackBox<unsigned, double, DoubleMap> MapTransform;

public:
    Tree();

    Tree(DRPLAN::Node *root, unsigned sampleNum);

    ~Tree();

    bool solveTree();

    std::vector<DoubleMap> finalSolutionList; /**< A list of maps that stores all the possible solutions.
																	 Each solution maps a variable index to its corresponding value.*/

private:
    DRPLAN::Node *root;
    unsigned sampleNum; ///< The number of samples for interpolation.
    std::vector<Domain> domainList; ///< A map that stores the interval of the interpolation samples.
    std::vector<Solution> solutionList; /**< A list of maps that stores all current possible solutions.
																	Each solution map contains the representation for each variable
																	which only involves the free variables .
																	*/
    bool solveNode(DRPLAN::Node *curNode);
    MapTransform transformMap(DRPLAN::Node *node, const Tree::Solution &solution) const;
    /**
     * Interpolates the sample points to solve the roots
     * and represent the target variable in terms of free variables.
     * @param varMap a function that gives the values of all cayley parameters from the active cayley parameters.
     * @param domain a domain to sample points;
     * @param node a pointer to currently solving root.
     * @return A list of representations of the target variable.
     */
    std::pair<std::vector<BlackBox<>>, std::vector<Domain>>
    solveTarget(Node *node, const MapTransform varMap, const Domain domain) const;
    /**
     * Uses the free variables to replace the target variable wherever it occurs in the previous solution.
     * @param solution the solution map where the representation was solved.
     * @param reps the representation of the target variable in terms of the free variables.
     * @param targetVar the index of the target variable.
     * @return the new solution with no target variable appears.
     */
    Solution updateSolution(const Solution &solution, const BlackBox<> &reps, unsigned targetVar) const;
    Domain updateDomain(const Domain &domain, const Domain &interDomain) const;

};

}
