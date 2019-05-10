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

#include "TwoTree.h"
#include "TwoTreeUtils.h"

#include <glpk.h>
#include "stdafx.h"

using namespace boost;


/**
 * The definitions of functions in TwoTree::TwoTree;
 */
namespace DRPLAN
{

struct position_t
{
    typedef vertex_property_tag kind;
};

struct color_t
{
    typedef edge_property_tag kind;
};

struct copy_nothing
{
    template<typename obj1, typename obj2>
    void operator()(const obj1 &, obj2 &) const {}
};

template<typename inputGraph, typename outputGraph>
struct edge_copier
{
    edge_copier(const inputGraph &ig, outputGraph &og, bool useDist)
            : ig(ig),
              vertex_pos_map(get(position_t(), ig)),
              edge_length_map(get(&Link::distance, og)),
              edge_color_map(get(color_t(), ig)),
              edge_type_map(get(&Link::edge_type, og)),
              useDist(useDist)
    {
    }

    Point parsePos(const std::string &pos) const
    {
        std::string::size_type y;
        double px = stod(pos, &y);
        double py = stod(pos.substr(y + 1));
        return Point(px, py);
    }

    template<typename inputEdge, typename outEdge>
    void operator()(const inputEdge &ie, outEdge &oe) const
    {
        std::string color = get(edge_color_map, ie);
        put(edge_type_map, oe, Link::getEdgeType(color));
        if(useDist) {
            Point src = parsePos(get(vertex_pos_map, source(ie, ig)));
            Point tar = parsePos(get(vertex_pos_map, target(ie, ig)));
            double distance = Point::distance(src, tar);
            put(edge_length_map, oe, distance);
        }
    }

    const inputGraph &ig;
    bool useDist;
    typename property_map<inputGraph, position_t>::const_type vertex_pos_map;
    mutable typename property_map<outputGraph, double Link::*>::type edge_length_map;
    typename property_map<inputGraph, color_t>::const_type edge_color_map;
    mutable typename property_map<outputGraph, EdgeType Link::*>::type edge_type_map;
};

typedef undirected_graph<
    property<
        position_t, ///< Vertex's position
        std::string >,
    property<
        color_t, ///< Edge's color
        std::string >,
    no_property
> graphviz_t; ///< Type for GraphViz m_graph

TwoTree::TwoTree()
{
    m_graph = graph_t(0);
}

TwoTree::TwoTree(std::string filePath, bool useDistanceInfo)
{
    std::ifstream ifs(filePath);
    graphviz_t graphviz(0);
    dynamic_properties dp(ignore_other_properties);

    dp.property("node_id", get(vertex_index_t(), graphviz));
    dp.property("pos", get(position_t(), graphviz));
    dp.property("color", get(color_t(), graphviz));
    assert(read_graphviz(ifs, graphviz, dp, "node_id"));

    copy_graph(graphviz, m_graph, vertex_copy(copy_nothing())
            .edge_copy(edge_copier<graphviz_t, graph_t>(graphviz, m_graph, useDistanceInfo)));
    updateEdgeList();
    getSupportEdges();
    flip = Flip((unsigned)num_vertices(m_graph));
    generateDRplan();
}

TwoTree::TwoTree(TwoTree const & twotree)
    : m_graph(TwoTree::graph_t{twotree.m_graph}), flip(twotree.flip), dropFlip(twotree.dropFlip)
{
    updateEdgeList();
    //copy_node(m_graph, twotree.m_graph);
}

TwoTree::~TwoTree()
{

}

auto TwoTree::operator[](index_t edge_index) const -> EdgeDesc const &
{
    if(edge_map.count(edge_index)) {
        return edge_map.at(edge_index);
    } else {
        return edge_map.cbegin()->second;
    }
}

void TwoTree::updateEdgeList() {
    EdgeIter ei, ei_end;
    auto eIndexMap = get(edge_index_t(), m_graph);
    for(tie(ei, ei_end) = edges(m_graph); ei != ei_end; ++ei) {
        edge_map[get(eIndexMap, *ei)] = *ei;
        switch(m_graph[*ei].edge_type) {
            case EdgeType::ADDED: {
                added_list.push_back(get(eIndexMap, (*ei)));
            } break;
            case EdgeType::DROPPED: {
                dropped_list.push_back(get(eIndexMap, (*ei)));
            } break;
            default: {
                continue;
            }
        }
    }
}

void TwoTree::copy_node(graph_t &m_graph, graph_t const &o_graph)
{
    //if(!m_graph.is_root()) {
    //    get_property(m_graph) = get_property(o_graph);
    //}
    //graph_t::children_iterator m_gi, m_gi_end;
    //graph_t::const_children_iterator o_gi, o_gi_end;
    //boost::tie(m_gi, m_gi_end) = m_graph.children();
    //boost::tie(o_gi, o_gi_end) = o_graph.children();
    //auto [m_gi, m_gi_end] = m_graph.children();
    //auto [o_gi, o_gi_end] = o_graph.children();
    //for( ; m_gi != m_gi_end; ++m_gi, ++o_gi) {
    //    copy_node(*m_gi, *o_gi);
    //}
}

void TwoTree::getSupportEdges()
{
    if(num_edges(m_graph) < 3) {
        throw ("Not enough vertices.");
    }
    auto eIndexMap = get(edge_index_t(), m_graph);
    OutEdgeIter oe_start, oe, oe_end;
    VerIter vi, vi_end;
    tie(vi, vi_end) = vertices(m_graph);
    ++vi;
    tie(oe_start, oe_end) = out_edges(*vi, m_graph);
    for(oe = oe_start; oe != oe_end; ++oe) {
        if(target(*oe, m_graph) < *vi) {
            m_graph[*vi].e1 = get(eIndexMap, *oe);
            break;
        }
    }
    ++vi;
    for(; vi != vi_end; ++vi) {
        bool firstEdge = true;
        tie(oe_start, oe_end) = out_edges(*vi, m_graph);
        for(oe = oe_start; oe != oe_end; ++oe) {
            if(target(*oe, m_graph) < *vi) {
                if(m_graph[*oe].edge_type == EdgeType::DROPPED) {
                    continue;
                }
                if(firstEdge) { ///< First edge
                    m_graph[*vi].e1 = get(eIndexMap, *oe);
                    firstEdge = false;
                } else { ///< second edge
                    if(target(*oe, m_graph) < getOppositeVertex(*vi, edge_map[m_graph[*vi].e1], m_graph)) {
                        m_graph[*vi].e2 = m_graph[*vi].e1;
                        m_graph[*vi].e1 = get(eIndexMap, *oe);
                    } else {
                        m_graph[*vi].e2 = get(eIndexMap, *oe);
                    }
                    break;
                }
            }
        }
    }
}

void TwoTree::print_vertices() const
{
    boost::print_vertices(m_graph, get(vertex_index, m_graph));
}

void TwoTree::print_edges() const
{
    boost::print_edges2(m_graph, get(vertex_index, m_graph), get(&Link::edge_type, m_graph));
}

void TwoTree::print_graph() const
{
    boost::print_graph(m_graph, get(vertex_index, m_graph));
}

void TwoTree::generateDRplan()
{
    calcEdgeBoundaries();
    generateDRplan(m_graph);
}

size_t TwoTree::dropped_num() const
{
    return dropped_list.size();
}

index_t TwoTree::getDroppedEdge(size_t drop_i) const
{
    return dropped_list[drop_i];
}

double TwoTree::changeDistanceBy(index_t edge_i, double ratio)
{
    m_graph[edge_map[edge_i]].distance *= ratio;
    return m_graph[edge_map[edge_i]].distance;
}

void TwoTree::calcEdgeBoundaries()
{
    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "find_edge_boundary");

    auto eIndexMap = get(edge_index_t(), m_graph);
    std::unordered_map<size_t, int> edge_to_col;
    EdgeIter ei, ei_end;

    // add the column of the added edges.
    tie(ei, ei_end) = edges(m_graph);
    for(; ei != ei_end; ++ei) {
        if(m_graph[*ei].edge_type == EdgeType::ADDED) {
            int col = glp_add_cols(lp, 1);
            edge_to_col[get(eIndexMap, *ei)] = col;
        }
    }

    std::vector<int> cons_i, cons_j;
    cons_i.push_back(0);
    cons_j.push_back(0);
    std::vector<double> cons_coef;
    cons_coef.push_back(0.0);
    for(tie(ei, ei_end) = edges(m_graph); ei != ei_end; ++ei) {
        if(m_graph[*ei].edge_type != EdgeType::ADDED) {
            continue;
        }
        double min = 0, max = DBL_MAX;
        VerDesc v1 = source(*ei, m_graph);
        VerDesc v2 = target(*ei, m_graph);
        auto edge_pairs = std::move(findCommonTargetEdges(v1, v2, m_graph));
        for(auto [e1, e2]: edge_pairs) {
            if(m_graph[e1].edge_type == EdgeType::DROPPED
               || m_graph[e2].edge_type == EdgeType::DROPPED) {
                continue;
            }

            if(m_graph[e1].edge_type == EdgeType::ADDED) {
                if(m_graph[e2].edge_type == EdgeType::ADDED) {
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
                    glp_set_row_bnds(lp, row, GLP_UP, 0.0, m_graph[e2].distance);
                    cons_i.insert(cons_i.end(), 2, row);
                    cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                    cons_coef.push_back(1.0);
                    cons_j.push_back(edge_to_col[get(eIndexMap, e1)]);
                    cons_coef.push_back(-1.0);

                    // add constrains for (x0 + x1 > d2)
                    row = glp_add_rows(lp, 1);
                    glp_set_row_bnds(lp, row, GLP_LO, m_graph[e2].distance, 0.0);
                    cons_i.insert(cons_i.end(), 2, row);
                    cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                    cons_coef.push_back(1.0);
                    cons_j.push_back(edge_to_col[get(eIndexMap, e1)]);
                    cons_coef.push_back(1.0);
                }
            } else if(m_graph[e2].edge_type == EdgeType::ADDED) {
                // add constrains for (x0 - x2 < d1)
                int row = glp_add_rows(lp, 1);
                glp_set_row_bnds(lp, row, GLP_LO, 0.0, m_graph[e2].distance);
                cons_i.insert(cons_i.end(), 2, row);
                cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                cons_coef.push_back(1.0);
                cons_j.push_back(edge_to_col[get(eIndexMap, e2)]);
                cons_coef.push_back(-1.0);

                // add constrains for (x0 + x2 > d1)
                row = glp_add_rows(lp, 1);
                glp_set_row_bnds(lp, row, GLP_LO, m_graph[e2].distance, 0.0);
                cons_i.insert(cons_i.end(), 2, row);
                cons_j.push_back(edge_to_col[get(eIndexMap, *ei)]);
                cons_coef.push_back(1.0);
                cons_j.push_back(edge_to_col[get(eIndexMap, e2)]);
                cons_coef.push_back(1.0);
            } else {
                double sum = m_graph[e1].distance + m_graph[e2].distance;
                double diff = abs(m_graph[e1].distance - m_graph[e2].distance);
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
    for(tie(ei, ei_end) = edges(m_graph); ei != ei_end; ++ei) {
        if(m_graph[*ei].edge_type == EdgeType::ADDED) {
            int col = edge_to_col[get(eIndexMap, *ei)];
            glp_set_obj_coef(lp, col, 1);
            glp_set_obj_dir(lp, GLP_MIN);
            glp_simplex(lp, nullptr);
            double min = glp_get_obj_val(lp);
            glp_set_obj_dir(lp, GLP_MAX);
            glp_simplex(lp, nullptr);
            double max = glp_get_obj_val(lp);
            m_graph[*ei].interval = std::make_pair(min, max);
            double lb = glp_get_col_lb(lp, col);
            double ub = glp_get_col_ub(lp, col);
            glp_set_obj_coef(lp, col, 0);
        }
    }
    glp_delete_prob(lp);
}

void TwoTree::printDRplan() const
{
    printDRplan(m_graph);
    testSubgraph(m_graph, *this);
}

void TwoTree::realize(std::unordered_map<unsigned, double> valMap, std::string suffix)
{
    for(const auto &kvPair : valMap) {
        std::cout << " x_" << kvPair.first << " = " << std::setw(5) << kvPair.second << "\t";
    }
    std::cout << std::endl;
    try {
        realize(m_graph, valMap);
        VerIter vi, vi_end;
        EdgeIter ei, ei_end;
        for(tie(vi, vi_end) = vertices(m_graph); vi != vi_end; ++vi) {
            std::cout << "Vertex " << *vi << ": "
                      << "(x, y) = " << m_graph[*vi].toString() << std::endl;
        }
        for(tie(ei, ei_end) = edges(m_graph); ei != ei_end; ++ei) {
            std::cout << m_graph[*ei].edge_type << " Edge " << *ei << ": "
                      << "Expect Length:" << m_graph[*ei].distance
                      << "\tActual Length:" << Point::distance(m_graph[source(*ei, m_graph)], m_graph[target(*ei, m_graph)])
                      << std::endl;
        }
        exportGraphviz(m_graph, suffix);
    } catch(const char *msg) {
        std::cout << msg << std::endl;
    }
    std::cout << std::endl;

}

TwoTree::Flip::Flip()
{
}

TwoTree::Flip::Flip(unsigned n)
        : flip(n, false)
{
}

TwoTree::Flip::~Flip()
{
}

void TwoTree::Flip::next()
{
    for(unsigned i = 3; i < flip.size(); i++) {
        if(flip[i]) {
            flip[i] = false;
        } else {
            flip[i] = true;
            return;
        }
    }
}

bool TwoTree::Flip::isBegin()
{
    for(const auto &bit : flip) {
        if(bit) { return false; }
    }
    return true;
}

bool TwoTree::Flip::flipAt(unsigned i)
{
    if(i > 1) {
        flip[i] = !flip[i];
    }
    return flip[i];
};

std::ostream& operator<<(std::ostream & os, const Point &pt) {
    return os << pt.toString();
}

}
