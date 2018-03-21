/**
 * This file is the defination of functions in TwoTree::Node.
 */

#include "stdafx.h"
#include "TwoTree.h"

using namespace boost;

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
    double sourceFlip = Point::CCW(subG[v1], subG[v2], subG[v3]);
    double targetFlip = Point::CCW(subG[v1], subG[v2], subG[v4]);

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

void Node::generateDRplan()
{
    auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
    TTGT &subG = reflex->graphRef;
    auto vIndexMap = get(vertex_index_t(), subG);
    auto eIndexMap = get(edge_index_t(), subG);
    isCayleyNode = num_edges(subG) == 1;
    EdgeIter<TTGT> ei_start, ei;
    VerIter<TTGT> v1 = vertices(subG).first;
    unsigned dropCounter = 0;
    unsigned addCounter = 0;
    tie(ei_start, ei) = edges(subG);
    do {
        --ei;
        if(subG[*ei].edge_type == EdgeType::dropped) {
            if(dropCounter == 0) {
                dropCounter++;
                targetDrop = get(eIndexMap, *ei);
                targetLength = subG[*ei].distance;
                reflex->droppedEdge = *ei;
            } else if(dropCounter == 1) {
                dropCounter++;
                unsigned vs = get(vIndexMap, source(*ei, subG));
                unsigned vt = get(vIndexMap, target(*ei, subG));
                subG.create_subgraph(v1, v1 + (vs > vt ? vs : vt) + 1);
            }
        } else if(subG[*ei].edge_type == EdgeType::added) {
            if(addCounter > 0) {
                freeCayley.emplace(targetCayley);
            }
            /**
             * Always solves for the latest appeared added edge in current node.
             */
            addCounter++;
            targetCayley = get(eIndexMap, *ei);
            reflex->targetEdge = *ei;
            if(isCayleyNode) {
                // interval = subG[*ei].interval;
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
                interval = std::make_pair(lb + Link::getEps(), ub - Link::getEps());
                freeCayley.emplace(targetCayley);
                break;
            }
            if(dropCounter == 1) {
                TTGT &cayleyNode = subG.create_subgraph();
                add_vertex(source(*ei, subG), cayleyNode);
                add_vertex(target(*ei, subG), cayleyNode);
            }
            if(addCounter > 1 || dropCounter > 1) {
                break;
            }
        }
    } while(ei != ei_start);

    allCayley = freeCayley;
    if(!isCayleyNode) {
        allCayley.emplace(targetCayley);
        TTGT::children_iterator gi, gi_end;
        for(tie(gi, gi_end) = subG.children(); gi != gi_end; gi++) {
            subNodes.push_back(&(*gi)[GraphBundle]);
            (*gi)[GraphBundle].reflex = new Reflex(*gi);
            (*gi)[GraphBundle].tt = tt;
            (*gi)[GraphBundle].generateDRplan();
            std::unordered_set<unsigned> subCayley = (*gi)[GraphBundle].allCayley;
            allCayley.insert(subCayley.begin(), subCayley.end());
        }
    }
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
    auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
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
