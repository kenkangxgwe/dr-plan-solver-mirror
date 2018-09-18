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

namespace DRPLAN
{
    VerDesc<TTGT> findTargetVertex(EdgeDesc<TTGT> const &tarEdge, TTGT const &graph);
    std::pair<VerDesc<TTGT>, VerDesc<TTGT>> getSupportiveVertexPair(VerDesc<TTGT> const &vc, TTGT const &graph);
    EdgeDesc<TTGT> findCommonEdge(VerDesc<TTGT> const &v1, VerDesc<TTGT> const &v2, TTGT &graph);
    std::vector<std::pair<EdgeDesc<TTGT>, EdgeDesc<TTGT>>> findCommonTargetEdges(VerDesc<TTGT> const &v1, VerDesc<TTGT> const &v2, TTGT const &graph);
    void calcEdgeBoundaries(TTGT &graph);
}
