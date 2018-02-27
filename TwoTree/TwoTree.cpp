//
// Created by kenkangxgwe on 2018.1.22.
//

#include "stdafx.h"
#include "TwoTree.h"

using namespace boost;

struct position_t {
    typedef vertex_property_tag kind;
};

struct color_t {
    typedef edge_property_tag kind;
};

struct copy_nothing
{
    template <typename obj1, typename obj2>
    void operator () (const obj1&, obj2&) const {}
};

template <typename inputGraph, typename outputGraph>
struct edge_copier
{
    edge_copier(const inputGraph &ig, outputGraph &og)
		: ig(ig),
		vertex_pos_map(get(position_t(), ig)),
		edge_distance_map(get(&Link::distance, og)),
		edge_color_map(get(color_t(), ig)),
		edge_type_map(get(&Link::edge_type, og))
	{
	}

	Point parsePos(const std::string &pos) const
	{
		std::string::size_type y;
		double px = stod(pos, &y);
		double py = stod(pos.substr(y + 1));
		return Point(px, py);
	}

	template <typename inputEdge, typename outEdge>
	void operator() (const inputEdge& ie, outEdge& oe) const
	{
		EdgeType edge_type;
		std::string color = get(edge_color_map, ie);
		if(color == "black") {
			edge_type = EdgeType::partial;
		} else if(color == "red") {
			edge_type = EdgeType::dropped;
		} else if(color == "green") {
			edge_type = EdgeType::added;
		} else {
			std::cerr << "There is no corresponding type for color \""
				<< get(edge_color_map, ie) << "\"." << std::endl;
			throw;
		}
		put(edge_type_map, oe, edge_type);
		Point src = parsePos(get(vertex_pos_map, source(ie, ig)));
		Point tar = parsePos(get(vertex_pos_map, target(ie, ig)));
		double distance = Point::distance(src, tar);
//		put(edge_distance_map, oe, distance);
	}
	const inputGraph &ig;
	typename property_map<inputGraph, position_t>::const_type vertex_pos_map;
	mutable typename property_map<outputGraph, double Link::*>::type edge_distance_map;
	typename property_map<inputGraph, color_t>::const_type edge_color_map;
	mutable typename property_map<outputGraph, EdgeType Link::*>::type edge_type_map;
};

TwoTree::TwoTree(char* filePath)
	:GraphBundle(boost::local_property<boost::graph_bundle_t>(boost::graph_bundle))
{
	std::ifstream ifs(filePath);
	typedef undirected_graph <property<position_t, std::string>, property<color_t, std::string>, no_property> graphviz_t;
	graphviz_t graphviz(0);
	dynamic_properties dp(ignore_other_properties);

	dp.property("node_id", get(vertex_index_t(), graphviz));
	dp.property("pos", get(position_t(), graphviz));
	dp.property("color", get(color_t(), graphviz));
	assert(read_graphviz(ifs, graphviz, dp, "node_id"));

	copy_graph(graphviz, graph, vertex_copy(copy_nothing())
		.edge_copy(edge_copier<graphviz_t, graph_t>(graphviz, graph)));
	flip = Flip(num_vertices(graph));
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

void TwoTree::realize()
{
	std::unordered_map<unsigned, double> cayleyPara;
	cayleyPara[2] = 1.0f;
	unsigned counter = 0;
	do {
		try {
			graph[GraphBundle].targetFunc(cayleyPara);
		} catch(const std::exception&) {
			flip.next();
			continue;
		}
		flip.next();
		VerIter<graph_t> vi, vi_end;
		EdgeIter<graph_t> ei, ei_end;
		std::cout << "flip " << counter << ":" << std::endl;
		for(tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
			std::cout << "Vertex " << *vi << ": ";
			std::cout << "x = " << graph[*vi].x << ", y = " << graph[*vi].y << std::endl;
		}
		for(tie(ei, ei_end) = edges(graph); ei != ei_end; ++ei) {
			std::cout << "Edge " << *ei << ": ";
			std::cout << graph[*ei].distance << std::endl;
		}
		counter++;
		//system("pause");
	} while(!flip.isBegin());

}

Node &TwoTree::getRoot()
{
	return graph[GraphBundle];
}

void Node::realize(std::unordered_map<unsigned, double> valMap)
{
	TTGT &subG = reflex->graphRef;
	if(num_edges(subG) < 3) {
		throw("Not enough vertices.");
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
            VerDesc<TTGT> vd = *vi;
            std::cout << "vd: " << vd << std::endl;
            put(xMap, *vi, subG[*oe].distance);
            put(yMap, *vi, 0);
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

bool Node::getDropFlip() const
{
	TTGT &subG = reflex->graphRef;
	if(isVarNode) {
		throw("This is a variable node.");
	}
    auto v1 = source(reflex->droppedEdge, subG);
    auto v2 = target(reflex->droppedEdge, subG);
	auto v3 = source(reflex->targetEdge, subG);
	return Point::CCW(subG[v1], subG[v2], subG[v3]);
}

void Node::generateDRplan()
{
	auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
	TTGT &subG = reflex->graphRef;
	auto vIndexMap = get(vertex_index_t(), subG);
	auto eIndexMap = get(edge_index_t(), subG);
	if(num_edges(subG) == 1) {
		isVarNode = true;
	}
	EdgeIter<TTGT> ei_start, ei;
	VerIter<TTGT> v1 = vertices(subG).first;
	unsigned dropCounter = 0;
	unsigned addCounter = 0;
	bool isFree = true;
	tie(ei_start, ei) = edges(subG);
	do {
		--ei;
		if(subG[*ei].edge_type == EdgeType::dropped) {
			if(dropCounter == 0) {
				targetDrop = get(eIndexMap, *ei);
                targetDistance = subG[*ei].distance;
				reflex->droppedEdge = *ei;
				dropCounter++;
			} else if(dropCounter == 1) {
				unsigned vs = get(vIndexMap, source(*ei, subG));
				unsigned vt = get(vIndexMap, target(*ei, subG));
				subG.create_subgraph(v1, v1 + (vs > vt ? vs : vt) + 1);
				dropCounter++;
			}
		} else if(subG[*ei].edge_type == EdgeType::added) {
			if(addCounter == 0) {
				targetVar = get(eIndexMap, *ei);
				reflex->targetEdge = *ei;
				if(isVarNode) {
					// interval = subG[*ei].interval;
					TTGT &rootGraph = subG.root();
					VerDesc<TTGT> vs = subG.local_to_global(source(*ei, subG)), vt = subG.local_to_global(target(*ei, subG)), vc;
					OutEdgeIter<TTGT> oe_start, oe, oe_end;
					tie(oe_start, oe_end) = out_edges((vs > vt) ? vs : vt, rootGraph);
					double d1, d2;
					for(oe = oe_start; oe != oe_end; ++oe) {
						vc = target(*oe, rootGraph);
						if(vc == vs || vc == vt) {
							continue;
						}
						if(vc > vs && vc > vt) {
							continue;
						}
						d1 = rootGraph[*oe].distance;
						break;
					}
					tie(oe_start, oe_end) = out_edges((vs > vt) ? vt : vs, rootGraph);
					for(oe = oe_start; oe != oe_end; ++oe) {
						if(vc == target(*oe, rootGraph)) {
							d2 = rootGraph[*oe].distance;
							break;
						}
					}
					interval = std::make_pair(abs(d1 - d2) + Link::getEps(), d1 + d2 - Link::getEps());
					freeVars.emplace(targetVar);
					break;
				}
				TTGT &varNode = subG.create_subgraph();
				add_vertex(source(*ei, subG), varNode);
				add_vertex(target(*ei, subG), varNode);
				addCounter++;
			} else {
				/**
				 * Always solves for the latest appeared added edge.
				 */
				freeVars.emplace(targetVar);
				targetVar = get(eIndexMap, *ei);
				reflex->targetEdge = *ei;
				TTGT &varNode = subG.create_subgraph();
				add_vertex(source(*ei, subG), varNode);
				add_vertex(target(*ei, subG), varNode);
				break;
			}
		}
	} while(ei != ei_start);

	if(isVarNode) {
		targetFunc = BlackBox<>::projectFunc(*(freeVars.begin()));
	} else {
		targetFunc = BlackBox<>([=, &subG](std::unordered_map<unsigned, double> valMap) -> double {
			realize(valMap);
			double res = Point::distance(subG[source(reflex->droppedEdge, subG)], subG[target(reflex->droppedEdge, subG)]) - targetDistance;
			//            if (isnan(res)){
			//                throw("The result is not a number.");
			//            }
			return res;
		}, freeVars);
	}
	typename TTGT::children_iterator gi, gi_end;
	for(tie(gi, gi_end) = subG.children(); gi != gi_end; gi++) {
		subNodes.push_back(&(*gi)[GraphBundle]);
		(*gi)[GraphBundle].reflex = new Reflex(*gi);
		(*gi)[GraphBundle].tt = tt;
		(*gi)[GraphBundle].generateDRplan();
	}
}

std::string Node::toString() const
{
	return "tv" + std::to_string(targetVar);
}

template <typename Graph>
struct posWriter
{
	posWriter(Graph &graph)
		: g(graph)
	{
	}
	template <class Vertex>
	void operator() (std::ostream &out, const Vertex &v) const
	{
		out << "[pos = \"" << g[v].x << ", " << g[v].y << "!\"]";
	}

	Graph &g;
};


template <typename Graph>
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
		out << "[color=\"" << color << "\", penwidth = \"5\"]";
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
	auto GraphBundle = boost::local_property<boost::graph_bundle_t>(boost::graph_bundle);
	TTGT &subG = reflex->graphRef;
	auto vIndexMap = get(vertex_index_t(), subG);
	auto eIndexMap = get(edge_index_t(), subG);
	VerIter<TTGT> vi, vi_end;
	for(tie(vi, vi_end) = vertices(subG); vi != vi_end; ++vi) {
		std::cout << "Vertex " << get(vIndexMap, *vi) << ":";
		std::cout << "(" << subG[*vi].x << ",";
		std::cout << subG[*vi].y << ")" << std::endl;
	}
	EdgeIter<TTGT> ei, ei_end;
	for(tie(ei, ei_end) = edges(subG); ei != ei_end; ++ei) {
		switch(subG[*ei].edge_type) {
		case EdgeType::partial:
		{
			std::cout << "Partial ";
		} break;
		case EdgeType::added:
		{
			std::cout << "Added ";
		} break;
		case EdgeType::dropped:
		{
			std::cout << "Dropped ";
		} break;
		}
		std::cout << "Edge " << get(eIndexMap, *ei) << ":";
		std::cout << "d(" << get(vIndexMap, source(*ei, subG)) << ", ";
		std::cout << get(vIndexMap, target(*ei, subG)) << ") = ";
		std::cout << subG[*ei].distance << std::endl;
	}
	typename TTGT::children_iterator gi, gi_end;
	for(tie(gi, gi_end) = subG.children(); gi != gi_end; gi++) {
		(*gi)[GraphBundle].printDRplan();
	}
}

TwoTree::Flip::Flip()
{
}

TwoTree::Flip::Flip(unsigned n)
	:flip(n, false)
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
	if(i > 2) {
		flip[i] = !flip[i];
	}
	return flip[i];
};
