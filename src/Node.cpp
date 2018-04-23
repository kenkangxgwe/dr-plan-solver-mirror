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
 * The definitions of functions in TwoTree::Node.
 */
namespace DRPLAN
{

void Node::realize(std::unordered_map<unsigned, double> valMap)
{
    TTGT &subG = reflex->graphRef;
    if(num_edges(subG) < 3) {
        throw ("Not enough vertices.");
    }
    auto eIndexMap = get(edge_index_t(), subG);
    OutEdgeIter<TTGT> oe_start, oe, oe_end;
    VerIter<TTGT> vi, vi_end;
    tie(vi, vi_end) = vertices(subG);
    VerIter<TTGT> vfirst = vertices(subG).first;
    auto xMap = get(&Point::x, subG);
    auto yMap = get(&Point::y, subG);
    put(xMap, *vfirst, 0);
    put(yMap, *vfirst, 0);
    subG[*vfirst].setXY(0, 0); ///< First vertex.
    vi++;
    tie(oe_start, oe_end) = out_edges(*vi, subG);
    for(oe = oe_start; oe != oe_end; ++oe) {
        if(target(*oe, subG) < *vi) {
            subG[*vi].setXY(subG[*oe].distance, 0); ///< Second vertex
//            VerDesc<TTGT> vd = *vi;
//            std::cout << "vd: " << vd << std::endl;
//            put(xMap, *vi, subG[*oe].distance);
//            put(yMap, *vi, 0);
            break;
        }
    }
    vi++;
    //    if(get(eIndexMap, reflex->droppedEdge) == 58) {
    //        std::cout << "This is Edge 58." << std::endl;
    //        printDRplan();
    //    }
    for(; vi != vi_end; ++vi) {
        VerDesc<TTGT> v1, v2;
        EdgeDesc<TTGT> e1, e2;
        double d1, d2;
        bool firstEdge = true;
        tie(oe_start, oe_end) = out_edges(*vi, subG);
        for(oe = oe_start; oe != oe_end; ++oe) {
            if(target(*oe, subG) < *vi) {
                if(subG[*oe].edge_type == EdgeType::dropped) {
                    continue;
                }
                if(firstEdge) {
                    e1 = *oe;
                    v1 = target(*oe, subG); ///< First vertex
                    if(subG[e1].edge_type == EdgeType::added) {
                        d1 = valMap[get(eIndexMap, *oe)];
                    } else {
                        d1 = subG[e1].distance;
                    }
                    firstEdge = false;
                } else {
                    e2 = *oe;
                    v2 = target(*oe, subG); ///< Second vertex
                    if(subG[e2].edge_type == EdgeType::added) {
                        d2 = valMap[get(eIndexMap, *oe)];
                    } else {
                        d2 = subG[e2].distance;
                    }
                    break;
                }
            }
        }

        double dx = subG[v1].x - subG[v2].x;
        double dy = subG[v1].y - subG[v2].y;
        double d0 = sqrt(dx * dx + dy * dy);
        double mx = (subG[v1].x + subG[v2].x) / 2;
        double my = (subG[v1].y + subG[v2].y) / 2;
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
        int sign = (tt->flip[*vi]) ? 1 : -1;
        double x = (-dd * md * dx - sign * dy * sqrt(delta)) / d0 / d0 + mx;
        double y = (-dd * md * dy + sign * dx * sqrt(delta)) / d0 / d0 + my;
        subG[*vi].setXY(x, y);
    }
}

std::pair<double, double> Node::dropFlip()
{
    if(isCayleyNode) {
        throw ("This is a Cayley node.");
    }
    TTGT &subG = reflex->graphRef;
    auto v1 = source(reflex->droppedEdge, subG);
    auto v2 = target(reflex->droppedEdge, subG);
    auto v3 = source(reflex->targetEdge, subG);
    auto v4 = target(reflex->targetEdge, subG);
    double sourceFlip, targetFlip;
    sourceFlip = Point::CCW(subG[v1], subG[v2], subG[v3]);
    targetFlip = Point::CCW(subG[v1], subG[v2], subG[v4]);

    return std::make_pair(sourceFlip, targetFlip);
}

double Node::dropDiff()
{
    if(isCayleyNode) {
        throw ("This is a Cayley node.");
    }
    TTGT &subG = reflex->graphRef;
    double actualLength = Point::distance(subG[source(reflex->droppedEdge, subG)],
                                          subG[target(reflex->droppedEdge, subG)]);
    //            if (isnan(res)){
    //                throw("The result is not a number.");
    //            }
    return (actualLength - targetLength);
}

/**
 * Calculates the interval of the Cayley edge.
 */
void Node::calcInterval()
{
    auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
    TTGT &subG = reflex->graphRef;
    auto eIndexMap = get(edge_index_t(), subG);
    EdgeIter<TTGT> ei;
    ei = edges(subG).first;
    TTGT &rootGraph = subG.root();
    VerDesc<TTGT> vs = subG.local_to_global(source(*ei, subG)), vt = subG.local_to_global(
            target(*ei, subG)), va;
    OutEdgeIter<TTGT> oe, oe_end;
    std::unordered_map<VerDesc<TTGT>, EdgeDesc<TTGT>> veMap;
    double lb = 0, ub = 0;
    bool firsTrig = true; ///< if this is the first triangle adjacent to vs and vt.
    for(tie(oe, oe_end) = out_edges(vs, rootGraph); oe != oe_end; ++oe) {
        va = target(*oe, rootGraph);
        veMap[va] = *oe;
    }
    for(tie(oe, oe_end) = out_edges(vt, rootGraph); oe != oe_end; ++oe) {
        va = target(*oe, rootGraph);
        if(veMap.count(va)) {
            EdgeDesc<TTGT> e1 = veMap.at(va);
            double tempLb = abs(rootGraph[e1].distance - rootGraph[*oe].distance);
            double tempUb = rootGraph[e1].distance + rootGraph[*oe].distance;
            if(firsTrig) {
                lb = tempLb;
                ub = tempUb;
                firsTrig = false;
            } else {
                lb = (lb > tempLb) ? lb : tempLb;
                ub = (ub < tempUb) ? ub : tempUb;
            }
        }
    }
    interval.first = lb + Link::getEps();
    interval.second = ub - Link::getEps();
    targetCayley = get(eIndexMap, *ei);
    freeCayley.push_back(targetCayley);
    allCayley = freeCayley;
    return;
}

void Node::generateDRplan()
{
    auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
    TTGT &subG = reflex->graphRef;
    isCayleyNode = num_edges(subG) == 1;
    if(isCayleyNode) {
        calcInterval();
        return;
    }

    /**
     * Finds the subnodes.
     */
    auto vIndexMap = get(vertex_index_t(), subG);
    auto eIndexMap = get(edge_index_t(), subG);
    EdgeIter<TTGT> ei_start, ei, ei_end;
    tie(ei_start, ei_end) = edges(subG);
    ei = ei_end;
    VerIter<TTGT> v1 = vertices(subG).first;
    unsigned dropCounter = 0;
    /**
     * Finds sub DR-Nodes.
     */
    do{
        --ei;
        if(subG[*ei].edge_type != EdgeType::dropped) {
            continue;
        }
        dropCounter++;
        if(dropCounter == 1) {
            targetDrop = get(eIndexMap, *ei);
            targetLength = subG[*ei].distance;
            reflex->droppedEdge = *ei;
        } else {
            unsigned vs = get(vIndexMap, source(*ei, subG));
            unsigned vt = get(vIndexMap, target(*ei, subG));
            TTGT &subNode = subG.create_subgraph(v1, v1 + (vs > vt ? vs : vt) + 1);
            subNodes.push_back(&subNode[GraphBundle]);
            subNode[GraphBundle].reflex = new Reflex(subNode);
            subNode[GraphBundle].tt = tt;
            subNode[GraphBundle].generateDRplan();
            std::vector<unsigned> subCayley = subNode[GraphBundle].freeCayley;
            freeCayley.insert(freeCayley.end(), subCayley.begin(), subCayley.end());
            subCayley = subNode[GraphBundle].allCayley;
            allCayley.insert(allCayley.end(), subCayley.begin(), subCayley.end());
            break;
        }
    } while(ei != ei_start);
    /**
     * Finds Cayley nodes.
     */
    unsigned addCounter = 0;
    do {
        if(subG[*ei].edge_type != EdgeType::added) {
            continue;
        }
        addCounter++;
        /**
         * Always solves for the last Cayley edge for the current node.
         */
        if(addCounter > 1) {
            freeCayley.push_back(targetCayley);
        }
        targetCayley = get(eIndexMap, *ei);
        allCayley.push_back(targetCayley);
        reflex->targetEdge = *ei;

        TTGT &cayleyNode = subG.create_subgraph();
        add_vertex(source(*ei, subG), cayleyNode);
        add_vertex(target(*ei, subG), cayleyNode);
        subNodes.push_back(&cayleyNode[GraphBundle]);
        cayleyNode[GraphBundle].reflex = new Reflex(cayleyNode);
        cayleyNode[GraphBundle].tt = tt;
        cayleyNode[GraphBundle].generateDRplan();
    } while(++ei != ei_end);
    if(!addCounter) {
        targetCayley = freeCayley.back();
        freeCayley.pop_back();
        /**
         * Retrieves target edge iterator
         */
        ei = ei_start;
        do {
            if(get(eIndexMap, *ei) == targetCayley) {
                reflex->targetEdge = *ei;
                break;
            }
        } while(++ei != ei_end);
    }
    findFlip();
    return;
}

void Node::findFlip()
{
    TTGT &subG = reflex->graphRef;
    auto d1 = source(reflex->droppedEdge, subG);
    auto d2 = target(reflex->droppedEdge, subG);
    auto t1 = source(reflex->targetEdge, subG);
    auto t2 = target(reflex->targetEdge, subG);
    double sourceFlip, targetFlip;
    if(t1 == d1 || t1 == d2) {
        auto tf = t2;
        t2 = t1;
        t1 = tf;
    } else if(t2 == d1 || t2 == d2) {
        auto tf = t1;
        t1 = t2;
        t2 = tf;
    } else {
        return;
    }
    OutEdgeIter<TTGT> eo, eo_end;
    std::unordered_map<VerDesc<TTGT>, EdgeDesc<TTGT>> veMap;
    std::tie(eo, eo_end) = out_edges(t1, subG);
    for(; eo != eo_end; ++eo) {
        auto vt = target(*eo, subG);
        if(vt < t2) {
            veMap[vt] = *eo;
        }
    }
    AdjVerIter<TTGT> va, va_end;
    std::tie(va, va_end) = adjacent_vertices(t2, subG);
    for(; va != va_end; ++va) {
        if(veMap.count(*va)) {
            reflex->targetEdge = veMap.at(*va);
            break;
        }
    }
    return;
}

std::string Node::toString() const
{
    return "tv" + std::to_string(targetCayley);
}

template<typename Graph>
struct posWriter
{
    posWriter(Graph &graph)
            : g(graph)
    {
    }

    template<class Vertex>
    void operator()(std::ostream &out, const Vertex &v) const
    {
        out << "[pos = \"" << g[v].x << ", " << g[v].y << "!\"]";
    }

    Graph &g;
};


template<typename Graph>
struct colorWriter
{
    colorWriter(Graph &graph)
            : g(graph)
    {
    }

    template<typename Edge>
    void operator()(std::ostream &out, const Edge &e) const
    {
        std::string color;
        switch(g[e].edge_type) {
            case partial:
                color = "black";
                break;
            case dropped:
                color = "red";
                break;
            case added:
                color = "green";
                break;
            default:
                throw ("unknown edge type.");
        }
        out << "[color=\"" << color << "\", penwidth = \"1\"]";
    }

    Graph &g;
};

void Node::exportGraphviz(std::string suffix) const
{
    TTGT &subG = reflex->graphRef;
    auto vIndexMap = get(vertex_index_t(), subG);
    auto eIndexMap = get(edge_index_t(), subG);
    std::string timestamp = to_string(std::time(NULL));
    std::ofstream out("exports/" + this->toString() + "t" + timestamp + suffix + ".dot");
    write_graphviz(out, subG, posWriter<TTGT>(subG), colorWriter<TTGT>(subG));
}

void Node::printDRplan() const
{
    if(isCayleyNode) {
        return;
    }
    auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
    TTGT &subG = reflex->graphRef;
    auto vIndexMap = get(vertex_index_t(), subG.root());
    auto eIndexMap = get(edge_index_t(), subG.root());
    VerIter<TTGT> vi, vi_end;
    if(subG.is_root()) {

        EdgeIter<TTGT> ei, ei_end;
        for(tie(ei, ei_end) = edges(subG); ei != ei_end; ++ei) {
            switch(subG[*ei].edge_type) {
                case EdgeType::partial: {
                    std::cout << "Partial ";
                }
                    break;
                case EdgeType::added: {
                    std::cout << "Added ";
                }
                    break;
                case EdgeType::dropped: {
                    std::cout << "Dropped ";
                }
                    break;
            }
            std::cout << "Edge " << get(eIndexMap, *ei) << ":";
            std::cout << "d(" << get(vIndexMap, subG.local_to_global(source(*ei, subG))) << ", ";
            std::cout << get(vIndexMap, subG.local_to_global(target(*ei, subG))) << ") = ";
            std::cout << subG[*ei].distance << std::endl;
        }
    }
    std::cout << "Node: " << std::endl;
    std::cout << "Target Function: x_" << targetCayley << "(";
    for(auto iter = freeCayley.begin(); iter != freeCayley.end(); ++iter) {
        if(iter != freeCayley.begin()) std::cout << ", ";
        std::cout << "x_" << (*iter);
    }
    std::cout << ")" << std::endl << "Target Drop: " << targetDrop << std::endl;
    //	for(tie(vi, vi_end) = vertices(subG); vi != vi_end; ++vi) {
    //		std::cout << "Vertex " << get(vIndexMap, *vi) << ":";
    //		std::cout << "(x,y) = (" << subG[*vi].x << ",";
    //		std::cout << subG[*vi].y << ")" << std::endl;
    //	}
    typename TTGT::children_iterator gi, gi_end;
    for(tie(gi, gi_end) = subG.children(); gi != gi_end; gi++) {
        (*gi)[GraphBundle].printDRplan();
    }
}

}
