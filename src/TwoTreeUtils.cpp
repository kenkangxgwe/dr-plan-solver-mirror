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

using namespace boost;

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

std::vector<std::pair<EdgeDesc<TTGT>, EdgeDesc<TTGT>>> findCommonTargetEdges(VerDesc<TTGT> const &v1, VerDesc<TTGT> const &v2, TTGT const &graph)
{
    OutEdgeIter<TTGT> oe, oe_end;
    std::vector<std::pair<EdgeDesc<TTGT>, EdgeDesc<TTGT>>> res;
    std::unordered_map<VerDesc<TTGT>, EdgeDesc<TTGT>> ve_map;
    for(std::tie(oe, oe_end) = out_edges(v1, graph); oe != oe_end; ++oe) {
        ve_map[target(*oe, graph)] = *oe;
    }
    for(std::tie(oe, oe_end) = out_edges(v2, graph); oe != oe_end; ++oe) {
        if(ve_map.count(target(*oe, graph))) {
            res.push_back(std::make_pair(ve_map[target(*oe, graph)], *oe));
        }
    }
    return res;
}

void calcEdgeBoundaries(TTGT &graph)
{
    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "find_edge_boundary");

    auto eIndexMap = get(edge_index_t(), graph);
    std::unordered_map<size_t, int> edge_to_col;
    EdgeIter<TTGT> ei, ei_end;

    // add the column of the added edges.
    tie(ei, ei_end) = edges(graph);
    for(; ei != ei_end; ++ei) {
        if(graph[*ei].edge_type == EdgeType::ADDED) {
            int col = glp_add_cols(lp, 1);
            edge_to_col[get(eIndexMap, *ei)] = col;
        }
    }

    std::vector<int> cons_i, cons_j;
    cons_i.push_back(0);
    cons_j.push_back(0);
    std::vector<double> cons_coef;
    cons_coef.push_back(0.0);
    for(tie(ei, ei_end) = edges(graph); ei != ei_end; ++ei) {
        if(graph[*ei].edge_type != EdgeType::ADDED) {
            continue;
        }
        double min = 0, max = DBL_MAX;
        VerDesc<TTGT> v1 = source(*ei, graph);
        VerDesc<TTGT> v2 = target(*ei, graph);
        auto edge_pairs = std::move(findCommonTargetEdges(v1, v2, graph));
        for(auto [e1, e2]: edge_pairs) {
            if(graph[e1].edge_type == EdgeType::DROPPED
            || graph[e2].edge_type == EdgeType::DROPPED) {
                continue;
            }

            if(graph[e1].edge_type == EdgeType::ADDED) {
                if(graph[e2].edge_type == EdgeType::ADDED) {
                    // add constrains for (- x0 + x1 + x2> 0)
                    int row = glp_add_rows(lp, 1);
                    glp_set_row_bnds(lp, row, GLP_LO, 0.0, 0.0);
                    cons_i.insert(cons_i.end(), 3, row);
                    cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                    cons_coef.push_back(-1.0);
                    cons_j.push_back(edge_to_col[get(eIndexMap, e1)]);
                    cons_coef.push_back(1.0);
                    cons_j.push_back(edge_to_col[get(eIndexMap, e2)]);
                    cons_coef.push_back(1.0);
                } else {
                    // add constrains for (x0 - x1 < d2)
                    int row = glp_add_rows(lp, 1);
                    glp_set_row_bnds(lp, row, GLP_UP, 0.0, graph[e2].distance);
                    cons_i.insert(cons_i.end(), 2, row);
                    cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                    cons_coef.push_back(1.0);
                    cons_j.push_back(edge_to_col[get(eIndexMap, e1)]);
                    cons_coef.push_back(-1.0);

                    // add constrains for (x0 + x1 > d2)
                    row = glp_add_rows(lp, 1);
                    glp_set_row_bnds(lp, row, GLP_LO, graph[e2].distance, 0.0);
                    cons_i.insert(cons_i.end(), 2, row);
                    cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                    cons_coef.push_back(1.0);
                    cons_j.push_back(edge_to_col[get(eIndexMap, e1)]);
                    cons_coef.push_back(1.0);
                }
            }

            if(graph[e2].edge_type == EdgeType::ADDED) {
                // add constrains for (x0 - x2 < d1)
                int row = glp_add_rows(lp, 1);
                glp_set_row_bnds(lp, row, GLP_LO, 0.0, graph[e2].distance);
                cons_i.insert(cons_i.end(), 2, row);
                cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                cons_coef.push_back(1.0);
                cons_j.push_back(edge_to_col[get(eIndexMap, e2)]);
                cons_coef.push_back(-1.0);

                // add constrains for (x0 + x2 > d1)
                row = glp_add_rows(lp, 1);
                glp_set_row_bnds(lp, row, GLP_LO, graph[e2].distance, 0.0);
                cons_i.insert(cons_i.end(), 2, row);
                cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                cons_coef.push_back(1.0);
                cons_j.push_back(edge_to_col[get(eIndexMap, e2)]);
                cons_coef.push_back(1.0);
            } else {
                double sum = graph[e1].distance + graph[e2].distance;
                double diff = abs(graph[e1].distance - graph[e2].distance);
                if(max > sum) {
                    max = sum;
                }
                if(min < diff) {
                    min = diff;
                }
            }
        }

        // add constrains on min < x0 < max
        int col =  edge_to_col[get(eIndexMap, *ei)];
        glp_set_col_bnds(lp, col, GLP_DB, min, max);
    }

    glp_load_matrix(lp, static_cast<int>(cons_coef.size() - 1), &cons_i[0], &cons_j[0], &cons_coef[0]);
    for(tie(ei, ei_end) = edges(graph); ei != ei_end; ++ei) {
        if(graph[*ei].edge_type == EdgeType::ADDED) {
            int col = edge_to_col[get(eIndexMap, *ei)];
            glp_set_obj_coef(lp, col, 1);
            glp_set_obj_dir(lp, GLP_MIN);
            glp_simplex(lp, NULL);
            double min = glp_get_obj_val(lp);
            glp_set_obj_dir(lp, GLP_MAX);
            glp_simplex(lp, NULL);
            double max = glp_get_obj_val(lp);
            graph[*ei].interval = std::make_pair(min, max);
            double lb = glp_get_col_lb(lp, col);
            double ub = glp_get_col_ub(lp, col);
            glp_set_obj_coef(lp, col, 0);
        }
    }
    glp_delete_prob(lp);
}


}
