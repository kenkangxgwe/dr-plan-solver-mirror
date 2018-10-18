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

#include <TwoTree.h>

#include "TwoTree.h"
#include "TwoTreeUtils.h"
#include "stdafx.h"

using namespace boost;

/**
 * The definitions of functions in TwoTree::Node.
 */
namespace DRPLAN
{

Node &TwoTree::get_node(graph_t &graph)
{
    return get_property(graph);
}

Node const &TwoTree::get_node(graph_t const &graph) const
{
    return get_property(graph);
}

Node &TwoTree::operator[](graph_t &graph)
{
    return get_node(graph);
}

Node const &TwoTree::operator[](graph_t const &graph) const
{
    return get_node(graph);
}

TwoTree& TwoTree::realize(graph_t &graph, std::unordered_map<unsigned, double> valMap)
{
    if(num_edges(graph) < 3) {
        throw ("Not enough vertices.");
    }
    testSubgraph(graph, *this);
    VerIter vi, vi_end;
    tie(vi, vi_end) = vertices(graph);
    graph[*vi].setXY(0, 0); ///< First vertex.
    ++vi;
    graph[*vi].setXY(m_graph[edge_map[graph[*vi].e1]].distance, 0); ///< Second vertex
    ++vi;
    for(; vi != vi_end; ++vi) {
        index_t const e1 = graph[*vi].e1;
        index_t const e2 = graph[*vi].e2;
        EdgeDesc ed1 = edge_map[graph[*vi].e1];
        EdgeDesc ed2 = edge_map[graph[*vi].e2];
        VerDesc v1, v2;
        tie(v1, v2) = getSupportiveVertexPair(*vi, graph, *this);
        double d1, d2;
        if(m_graph[ed1].edge_type == EdgeType::ADDED) {
            d1 = valMap[e1];
        } else {
            d1 = m_graph[ed1].distance;
        }
        if(m_graph[ed2].edge_type == EdgeType::ADDED) {
            d2 = valMap[e2];
        } else {
            d2 = m_graph[ed2].distance;
        }

        if(Point::distance(graph[*vi], graph[v1]) == d1
           && Point::distance(graph[*vi], graph[v2]) == d2) {
            continue;
        }

        double dx = graph[v1].x - graph[v2].x;
        double dy = graph[v1].y - graph[v2].y;
        double d0 = sqrt(dx * dx + dy * dy);
        double mx = (graph[v1].x + graph[v2].x) / 2;
        double my = (graph[v1].y + graph[v2].y) / 2;
        double dd = d1 - d2;
        double md = (d1 + d2) / 2;
        double delta = (d0 * d0 - dd * dd) * (md * md - d0 * d0 / 4);
        if(delta < 0) {
            throw ("The graph is unrealizable");
        }
        /**
         * If we flip the vertex, then CW(v1, v2, v3) else CCW (counter-clockwise).
         * If dy < 0, then we choose the smaller x for CCW, and the larger x for CW.
         * If dx < 0, then we choose the larger y for CCW, and the smaller y for CW.
         */
        int sign = (flip[*vi]) ? 1 : -1;
        double x = (-dd * md * dx - sign * dy * sqrt(delta)) / d0 / d0 + mx;
        double y = (-dd * md * dy + sign * dx * sqrt(delta)) / d0 / d0 + my;
        graph[*vi].setXY(x, y);
    }
    return (*this);
}

std::pair<double, double> TwoTree::getDropFlip(graph_t &graph)
{
    auto const &node = get_node(graph);
    if(node.isCayleyNode) {
        throw ("This is a Cayley node.");
    }
    auto v1 = source(edge_map[node.targetDrop], graph);
    auto v2 = target(edge_map[node.targetDrop], graph);
    auto v3 = source(edge_map[node.dropFlipEdge], graph);
    auto v4 = target(edge_map[node.dropFlipEdge], graph);
    double sourceFlip, targetFlip;
    sourceFlip = Point::CCW(graph[v1], graph[v2], graph[v3]);
    targetFlip = Point::CCW(graph[v1], graph[v2], graph[v4]);

    return std::make_pair(sourceFlip, targetFlip);
}

double TwoTree::dropDiff(graph_t &graph)
{
    auto const &node = get_node(graph);
    if(node.isCayleyNode) {
        throw ("This is a Cayley node.");
    }
    double actualLength = Point::distance(graph[source(edge_map[node.targetDrop], graph)],
                                          graph[target(edge_map[node.targetDrop], graph)]);
    //            if (isnan(res)){
    //                throw("The result is not a number.");
    //            }
    return (actualLength - node.targetLength);
}

std::pair<double, double> TwoTree::refineInterval(index_t targetCayley, std::unordered_map<unsigned, double> valMap)
{
    EdgeDesc et = edge_map[targetCayley];
    VerDesc vt = findTargetVertex(et, m_graph, *this);
    auto [v1, v2] = getSupportiveVertexPair(vt, m_graph, *this);
    EdgeDesc e0 = findCommonEdge(v1, v2, m_graph);
    EdgeDesc e1;
    if(et == edge_map[m_graph[vt].e1]) {
        e1 = edge_map[m_graph[vt].e2];
    } else {
        e1 = edge_map[m_graph[vt].e1];
    }
    double d0, d1;
    auto eIndexMap = get(edge_index_t(), m_graph);
    if(m_graph[e0].edge_type == EdgeType::ADDED) {
        d0 = valMap[get(eIndexMap, e0)];
    } else {
        d0 = m_graph[e0].distance;
    }
    if(m_graph[e1].edge_type == EdgeType::ADDED) {
        d1 = valMap[get(eIndexMap, e1)];
    } else {
        d1 = m_graph[e1].distance;
    }

    return std::make_pair(abs(d0 - d1) + Link::getEps(), d0 + d1 - Link::getEps());
}

void TwoTree::generateDRplan(graph_t &graph)
{
    auto &node = get_node(graph);
    node.isCayleyNode = num_edges(graph) == 1;
    if(node.isCayleyNode) {
        auto eIndexMap = get(edge_index_t(), graph);
        auto edi = *(edges(graph).first);
        index_t ei = get(eIndexMap, edi);
        node.interval.first = graph[edi].interval.first + Link::getEps();
        node.interval.second = graph[edi].interval.second - Link::getEps();
        node.targetCayley = ei;
        node.freeCayley.push_back(node.targetCayley);
        node.allCayley = node.freeCayley;
        return;
    }

    /**
     * Finds the subnodes.
     */
    auto vIndexMap = get(vertex_index_t(), graph);
    auto eIndexMap = get(edge_index_t(), graph);
    EdgeIter ei_start, ei, ei_end;
    tie(ei_start, ei_end) = edges(graph);
    ei = ei_end;
    VerIter v1 = vertices(graph).first;
    unsigned dropCounter = 0;
    /**
     * Finds sub DR-Nodes.
     */
    do{
        --ei;
        if(graph[*ei].edge_type != EdgeType::DROPPED) {
            continue;
        }
        dropCounter++;
        if(dropCounter == 1) {
            node.targetDrop = get(eIndexMap, *ei);
            node.targetLength = graph[*ei].distance;
        } else {
            unsigned vs = (unsigned)get(vIndexMap, source(*ei, graph));
            unsigned vt = (unsigned)get(vIndexMap, target(*ei, graph));
            graph_t &sub_graph = graph.create_subgraph(v1, v1 + (vs > vt ? vs : vt) + 1);
            auto &sub_node = get_node(sub_graph);
            generateDRplan(sub_graph);
            std::vector<index_t> sub_cayley = sub_node.freeCayley;
            node.freeCayley.insert(node.freeCayley.end(), sub_cayley.begin(), sub_cayley.end());
            sub_cayley = sub_node.allCayley;
            node.allCayley.insert(node.allCayley.end(), sub_cayley.begin(), sub_cayley.end());
            break;
        }
    } while(ei != ei_start);
    /**
     * Finds Cayley nodes.
     */
    unsigned addCounter = 0;
    do {
        if(graph[*ei].edge_type != EdgeType::ADDED) {
            continue;
        }
        addCounter++;
        /**
         * Always solves for the last Cayley edge for the current node.
         */
        if(addCounter > 1) {
            node.freeCayley.push_back(node.targetCayley);
        }
        node.targetCayley = get(eIndexMap, *ei);
        node.allCayley.push_back(node.targetCayley);

        graph_t &cayley_graph = graph.create_subgraph();
        add_vertex(source(*ei, graph), cayley_graph);
        add_vertex(target(*ei, graph), cayley_graph);
        generateDRplan(cayley_graph);
    } while(++ei != ei_end);
    if(!addCounter) {
        node.targetCayley = node.freeCayley.back();
        node.freeCayley.pop_back();
    }
    findFlip(graph);
}


void TwoTree::findFlip(graph_t &graph)
{
    auto &node = get_node(graph);
    auto d1 = source(edge_map[node.targetDrop], graph);
    auto d2 = target(edge_map[node.targetDrop], graph);
    auto t1 = source(edge_map[node.targetCayley], graph);
    auto t2 = target(edge_map[node.targetCayley], graph);
    if(t1 == d1 || t1 == d2) {
        auto tf = t2;
        t2 = t1;
        t1 = tf;
    } else if(t2 != d1 && t2 != d2) {
        node.dropFlipEdge = node.targetCayley;
    }
    OutEdgeIter eo, eo_end;
    std::unordered_map<VerDesc, EdgeDesc> veMap;
    std::tie(eo, eo_end) = out_edges(t1, graph);
    for(; eo != eo_end; ++eo) {
        auto vt = target(*eo, graph);
        if(vt < t2) {
            veMap[vt] = *eo;
        }
    }
    AdjVerIter va, va_end;
    auto eIndexMap = get(edge_index_t(), graph);
    std::tie(va, va_end) = adjacent_vertices(t2, graph);
    for(; va != va_end; ++va) {
        if(veMap.count(*va)) {
            node.dropFlipEdge = get(eIndexMap, veMap.at(*va));
            break;
        }
    }
}

std::string TwoTree::toString(graph_t const &graph) const
{
    auto const &node = get_node(graph);
    return "tv" + std::to_string(node.targetCayley);
}

std::string TwoTree::toStringFull(graph_t const &graph) const
{
    auto const &node = get_node(graph);
    std::string output = "x_" + std::to_string(node.targetCayley) + "(";
    for(auto const &freeVar : node.freeCayley) {
        output += "x_" + std::to_string(freeVar) + ",";
    }
    output.pop_back();
    output += ")";
    return output;
}

template<typename Graph>
struct posWriter
{
    posWriter(Graph const &graph)
        : g(graph)
    {
    }

    template<class Vertex>
    void operator()(std::ostream &out, Vertex const &v) const
    {
        out << "[pos = \"" << g[v].x << ", " << g[v].y << "!\"]";
    }

    Graph const &g;
};


template<typename Graph>
struct colorWriter
{
    colorWriter(Graph const &graph)
        : g(graph)
    {
    }

    template<typename Edge>
    void operator()(std::ostream &out, Edge const &e) const
    {
        out << "[color=\"" << Link::getEdgeColor(g[e].edge_type) << "\", penwidth = \"1\"]";
    }

    Graph const &g;
};

void TwoTree::exportGraphviz(graph_t const &graph, std::string suffix) const
{
    auto vIndexMap = get(vertex_index_t(), graph);
    auto eIndexMap = get(edge_index_t(), graph);
    std::string timestamp = to_string(std::time(nullptr));
    std::ofstream out("exports/" + toString(graph) + "t" + timestamp + suffix + ".dot");
    write_graphviz(out, graph, posWriter<graph_t>(graph), colorWriter<graph_t>(graph));
}

void TwoTree::printDRplan(graph_t const &graph) const
{
    auto &node = get_node(graph);
    if(node.isCayleyNode) {
        return;
    }
    auto vIndexMap = get(vertex_index_t(), graph);
    auto eIndexMap = get(edge_index_t(), graph);
    VerIter vi, vi_end;
    if(graph.is_root()) {
        EdgeIter ei, ei_end;
        for(tie(ei, ei_end) = edges(graph); ei != ei_end; ++ei) {
            std::cout << graph[*ei].edge_type << " Edge " << get(eIndexMap, *ei) << ":"
                      << "d(" << get(vIndexMap, source(*ei, graph)) << ", "
                      << get(vIndexMap, target(*ei, graph)) << ") = "
                      << graph[*ei].distance << std::endl;
        }
    }
    std::cout << "Node: " << std::endl
              << "Target Function: x_" << node.targetCayley << "(";
    for(auto iter = node.freeCayley.begin(); iter != node.freeCayley.end(); ++iter) {
        if(iter != node.freeCayley.begin()) std::cout << ", ";
        std::cout << "x_" << (*iter);
    }
    std::cout << ")" << std::endl << "Target Drop: " << node.targetDrop << std::endl;
    //	for(tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
    //		std::cout << "Vertex " << get(vIndexMap, *vi) << ":";
    //		std::cout << "(x,y) = (" << graph[*vi].x << ",";
    //		std::cout << graph[*vi].y << ")" << std::endl;
    //	}
    typename TwoTree::graph_t::children_iterator gi, gi_end;
    for(tie(gi, gi_end) = graph.children(); gi != gi_end; gi++) {
        printDRplan(*gi);
    }
}

}
