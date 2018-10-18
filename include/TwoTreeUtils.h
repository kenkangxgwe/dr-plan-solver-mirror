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
#include "TwoTree.h"
#include "stdafx.h"

namespace DRPLAN
{
    void testSubgraph(TwoTree::graph_t const &, TwoTree const &);
    TwoTree::VerDesc findTargetVertex(TwoTree::EdgeDesc const &tarEdge, TwoTree::graph_t &graph, TwoTree const &tt);
    std::pair<TwoTree::VerDesc, TwoTree::VerDesc>
    getSupportiveVertexPair(TwoTree::VerDesc const &vc, TwoTree::graph_t const &, TwoTree const &);
    TwoTree::EdgeDesc findCommonEdge(TwoTree::VerDesc const &v1, TwoTree::VerDesc const &v2, TwoTree::graph_t &graph);
    std::vector<std::pair<TwoTree::EdgeDesc, TwoTree::EdgeDesc>>
    findCommonTargetEdges(TwoTree::VerDesc const &v1, TwoTree::VerDesc const &v2, TwoTree::graph_t const &graph);
}
