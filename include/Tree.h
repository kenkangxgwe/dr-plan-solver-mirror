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
#include "BlackBox.hpp"
#include "TwoTree.h"

namespace DRPLAN
{

typedef std::unordered_map<unsigned, double> DoubleMap;
typedef std::unordered_map<unsigned, BlackBox<>> Solution;
typedef std::unordered_map<unsigned, std::pair<double, double>> Domain;
typedef BlackBox<unsigned, double, DoubleMap> MapTransform;

struct NodeSolution
{
    Solution solution; /**< A list of maps that stores all current possible solutions.
																	Each solution map contains the representation for each variable
																	which only involves the free variables .
																	*/
    Domain domain; ///< A map that stores the interval of the interpolation samples.
    std::vector<int> drop_filp;
    unsigned offset;
};

class Tree
{

public:
    Tree();

    Tree(DRPLAN::Node *root, unsigned sampleNum = 20, unsigned maxOffset = 0);

    ~Tree();

    bool solveTree();

    std::vector<DoubleMap> finalSolutionList; /**< A list of maps that stores all the possible solutions.
																	 Each solution maps a variable index to its corresponding value.*/

private:
    DRPLAN::Node *root;
    unsigned sampleNum; ///< The number of samples for interpolation.
    unsigned max_offset; ///< The number of maximum times using tolenrance during the sampling.
    std::vector<NodeSolution> nodeSolutionList;
    bool solveNode(DRPLAN::Node *curNode);
    MapTransform transformMap(DRPLAN::Node *node, const Solution &solution) const;
    /**
     * Interpolates the sample points to solve the roots
     * and represent the target variable in terms of free variables.
     * @param varMap a function that gives the values of all cayley parameters from the active cayley parameters.
     * @param domain a domain to sample points;
     * @param node a pointer to currently solving root.
     * @return A list of representations of the target variable.
     */
    std::vector<NodeSolution>
    solveTarget(Node *node, MapTransform const &varMap, NodeSolution const &domain) const;
    Solution updateSolution(const Solution &solution, const BlackBox<> &reps, unsigned targetVar) const;

    friend std::vector<std::vector<std::pair<DoubleMap, double>>>
    findRoots(Node *, SPLINTER::DataTable const &, MapTransform const &, DoubleMap &, std::vector<double> const &);

};

}
