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

#include "stdafx.h"
#include "TwoTreeUtils.h"

namespace DRPLAN
{
VerDesc<TTGT> findTargetVertex(EdgeDesc<TTGT> const &tarEdge, TTGT const &graph)
{
    VerDesc<TTGT> &&v1 = source(tarEdge, graph);
    VerDesc<TTGT> &&v2 = target(tarEdge, graph);
    if(graph[v1].pointReflex->e1 == tarEdge || graph[v1].pointReflex->e2 == tarEdge) {
        return v1;
    } else {
        return v2;
    }
}

std::pair<VerDesc<TTGT>, VerDesc<TTGT>> getSupportiveVertexPair(VerDesc<TTGT> const &vc, TTGT const &graph)
{
    return std::make_pair(
            target(graph[vc].pointReflex->e1, graph),
            target(graph[vc].pointReflex->e2, graph)
    );
}

EdgeDesc<TTGT> findCommonEdge(VerDesc<TTGT> const &v1, VerDesc<TTGT> const &v2, TTGT &graph)
{
    OutEdgeIter<TTGT> oe, oe_end;
    for(std::tie(oe, oe_end) = out_edges(v1, graph); oe != oe_end; ++oe) {
        if(target(*oe, graph) == v2) {
            return *oe;
        }
    }
    throw("There is no common edges between the two input vertices.");
}

}
