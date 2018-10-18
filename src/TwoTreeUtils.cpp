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

#include <TwoTreeUtils.h>

#include "TwoTreeUtils.h"
#include "stdafx.h"

using namespace boost;

namespace DRPLAN
{

TwoTree::VerDesc findTargetVertex(TwoTree::EdgeDesc const &tarEdge, TwoTree::graph_t &graph, TwoTree const &tt)
{
    TwoTree::VerDesc &&v1 = source(tarEdge, graph);
    TwoTree::VerDesc &&v2 = target(tarEdge, graph);

    if(tt[graph[v1].e1] == graph.local_to_global(tarEdge)
       || tt[graph[v1].e2] == graph.local_to_global(tarEdge)) {
        return v1;
    } else {
        return v2;
    }
}

std::pair<TwoTree::VerDesc, TwoTree::VerDesc>
getSupportiveVertexPair(TwoTree::VerDesc const &vc, TwoTree::graph_t const &graph, TwoTree const &tt)
{
    TwoTree::EdgeDesc ed1 = graph.global_to_local(tt[graph[vc].e1]);
    TwoTree::EdgeDesc ed2 = graph.global_to_local(tt[graph[vc].e2]);
    TwoTree::VerDesc v1 = target(ed1, graph);
    TwoTree::VerDesc v2 = target(ed2, graph);
    if(v1 == vc) {
        v1 = source(ed1, graph);
    }
    if(v2 == vc) {
        v2 = source(ed2, graph);
    }
    return {v1, v2};
}

/**
 * This function returns the common edges of two local vertex descriptors.
 */
TwoTree::EdgeDesc findCommonEdge(TwoTree::VerDesc const &v1, TwoTree::VerDesc const &v2, TwoTree::graph_t &graph)
{
    TwoTree::OutEdgeIter oe, oe_end;
    for(std::tie(oe, oe_end) = out_edges(v1, graph); oe != oe_end; ++oe) {
        if(target(*oe, graph) == v2) {
            return *oe;
        }
    }
    throw("There is no common edges between the two input vertices.");
}

std::vector<std::pair<TwoTree::EdgeDesc, TwoTree::EdgeDesc>>
findCommonTargetEdges(TwoTree::VerDesc const &v1, TwoTree::VerDesc const &v2, TwoTree::graph_t const &graph)
{
    TwoTree::OutEdgeIter oe, oe_end;
    std::vector<std::pair<TwoTree::EdgeDesc, TwoTree::EdgeDesc>> res;
    std::unordered_map<TwoTree::VerDesc, TwoTree::EdgeDesc> ve_map;
    for(std::tie(oe, oe_end) = out_edges(v1, graph); oe != oe_end; ++oe) {
        ve_map[target(*oe, graph)] = *oe;
    }
    for(std::tie(oe, oe_end) = out_edges(v2, graph); oe != oe_end; ++oe) {
        if(ve_map.count(target(*oe, graph))) {
            res.emplace_back(ve_map[target(*oe, graph)], *oe);
        }
    }
    return res;
}

void testSubgraph(TwoTree::graph_t const &graph, TwoTree const &tt)
{
    TwoTree::VerIter vi, vi_end;
    boost::tie(vi, vi_end) = vertices(graph);
    TwoTree::EdgeDesc l3, l3_r;
    for(; vi != vi_end; ++vi) {
        if(graph[*vi].e1 == 3) {
            auto ed1 = tt[graph[*vi].e1];
            l3 = graph.global_to_local(ed1);
            break;
        } else if (graph[*vi].e2 == 3) {
            auto ed2 = tt[graph[*vi].e2];
            l3 = graph.global_to_local(ed2);
            break;
        }
    }
    auto eIndexMap = get(edge_index_t(), graph);
    TwoTree::EdgeIter ei, ei_end;
    boost::tie(ei, ei_end) = edges(graph);
    for(; ei != ei_end; ++ei) {
        if(get(eIndexMap, *ei) == 3) {
            l3_r = *ei;
            break;
        }
    }
    //try{
    //    auto ind1 = get(eIndexMap, l3);
    //    auto ind2 = get(eIndexMap, l3_r);
    //    int i = 0;
    //} catch(...) {
    //    int i = 0;
    //}

    TwoTree::ChildIter gi, gi_end;
    boost::tie(gi, gi_end) = graph.children();
    for(; gi != gi_end; ++gi) {
        testSubgraph(*gi, tt);
    }
}

}
