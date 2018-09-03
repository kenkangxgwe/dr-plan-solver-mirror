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
#include "TwoTree.h"

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

TwoTree::TwoTree(std::string filePath, bool useDistanceInfo)
        : GraphBundle(boost::local_property<boost::graph_bundle_t>(boost::graph_bundle))
{
    std::ifstream ifs(filePath);
    typedef undirected_graph<
            property<
                    position_t, ///< Vertex's position
                    std::string >,
            property<
                    color_t, ///< Edge's color
                    std::string >,
            no_property
    > graphviz_t; ///< Type for GraphViz graph
    graphviz_t graphviz(0);
    dynamic_properties dp(ignore_other_properties);

    dp.property("node_id", get(vertex_index_t(), graphviz));
    dp.property("pos", get(position_t(), graphviz));
    dp.property("color", get(color_t(), graphviz));
    assert(read_graphviz(ifs, graphviz, dp, "node_id"));

    copy_graph(graphviz, graph, vertex_copy(copy_nothing())
            .edge_copy(edge_copier<graphviz_t, graph_t>(graphviz, graph, useDistanceInfo)));
    getSupportEdges();
    flip = Flip((unsigned)num_vertices(graph));
}

TwoTree::~TwoTree()
{

}

void TwoTree::print_vertices() const
{
    boost::print_vertices(graph, get(vertex_index, graph));
}

void TwoTree::print_edges() const
{
    boost::print_edges2(graph, get(vertex_index, graph), get(&Link::edge_type, graph));
}

void TwoTree::print_graph() const
{
    boost::print_graph(graph, get(vertex_index, graph));
}

void TwoTree::generateDRplan()
{
    graph[GraphBundle].reflex = new Reflex(graph);
    graph[GraphBundle].tt = this;
    graph[GraphBundle].generateDRplan();
}

void TwoTree::printDRplan() const
{
    graph[GraphBundle].printDRplan();
}

void TwoTree::realize(std::unordered_map<unsigned, double> valMap, std::string suffix)
{
    for(const auto &kvPair : valMap) {
        std::cout << " x_" << kvPair.first << " = " << std::setw(5) << kvPair.second << "\t";
    }
    std::cout << std::endl;
    try {
        graph[GraphBundle].realize(valMap);
        VerIter<graph_t> vi, vi_end;
        EdgeIter<graph_t> ei, ei_end;
        for(tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
            std::cout << "Vertex " << *vi << ": "
                      << "(x, y) = " << graph[*vi].toString() << std::endl;
        }
        for(tie(ei, ei_end) = edges(graph); ei != ei_end; ++ei) {
            std::cout << graph[*ei].edge_type << " Edge " << *ei << ": "
                      << "Expect Length:" << graph[*ei].distance
                      << "\tActual Length:" << Point::distance(graph[source(*ei, graph)], graph[target(*ei, graph)])
                      << std::endl;
        }
        graph[GraphBundle].exportGraphviz(suffix);
    } catch(const char *msg) {
        std::cout << msg << std::endl;
    }
    std::cout << std::endl;

}

Node &TwoTree::getRoot()
{
    return graph[GraphBundle];
}

void TwoTree::getSupportEdges()
{
    if(num_edges(graph) < 3) {
        throw ("Not enough vertices.");
    }
    auto eIndexMap = get(edge_index_t(), graph);
    OutEdgeIter<TTGT> oe_start, oe, oe_end;
    VerIter<TTGT> vi, vi_end;
    tie(vi, vi_end) = vertices(graph);
    ++vi;
    tie(oe_start, oe_end) = out_edges(*vi, graph);
    for(oe = oe_start; oe != oe_end; ++oe) {
        if(target(*oe, graph) < *vi) {
            graph[*vi].pointReflex = new PointReflex();
            graph[*vi].pointReflex->e1 = *oe;
            break;
        }
    }
    ++vi;
    for(; vi != vi_end; ++vi) {
        bool firstEdge = true;
        tie(oe_start, oe_end) = out_edges(*vi, graph);
        for(oe = oe_start; oe != oe_end; ++oe) {
            if(target(*oe, graph) < *vi) {
                if(graph[*oe].edge_type == EdgeType::DROPPED) {
                    continue;
                }
                if(firstEdge) {
                    graph[*vi].pointReflex = new PointReflex();
                    graph[*vi].pointReflex->e1 = *oe; ///< First edge
                    firstEdge = false;
                } else {
                    graph[*vi].pointReflex->e2 = *oe; ///< First edge
                    break;
                }
            }
        }
    }
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
