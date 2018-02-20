#pragma once

#include "stdafx.h"
#include "utils/BlackBox.hpp"

template<typename GraphType>
using VerIter = typename boost::graph_traits<GraphType>::vertex_iterator;
template<typename GraphType>
using EdgeIter = typename boost::graph_traits<GraphType>::edge_iterator;
template<typename GraphType>
using OutEdgeIter = typename boost::graph_traits<GraphType>::out_edge_iterator;
template<typename GraphType>
using VerDesc = typename boost::graph_traits<GraphType>::vertex_descriptor;
template<typename GraphType>
using EdgeDesc = typename boost::graph_traits<GraphType>::edge_descriptor;

/**
 * Vertex Bundled Properties
 */
struct Point
{
	static double distance(Point a, Point b)
	{
		return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
	};
	Point(double x = 0, double y = 0)
		:x(x), y(y)
	{
	};
	double x, y;
};

enum EdgeType { partial, dropped, added };

/**
 * Edge Bundled Properties
 */

struct Link
{
	EdgeType edge_type;
	double distance = 100;
	std::pair<double, double> interval = std::pair<double, double>(2E-8, 200 - 2E-8);
};

struct Reflex;
class TwoTree;

struct Node
{
	double targetDistance = 100;
	Reflex *reflex;
	void generateDRplan();
    void printDRplan();
    void exportGraphviz(std::string);
	//double realizeTarget(std::unordered_map<int, double>);
//	BlackBox<> realize;
	const TwoTree *tt;
    bool isVarNode = false;
    std::unordered_set<unsigned> freeVars;
    unsigned targetDrop;
    unsigned targetVar;
    BlackBox<> targetFunc;
    std::vector<Node *> subNodes;
    std::string toString();
};

typedef boost::subgraph<
	boost::adjacency_list<
	boost::vecS, ///< OutEdgeList
	boost::vecS, ///< VertexList
	boost::undirectedS, ///< Directed
	boost::property<
	boost::vertex_index_t,
	unsigned,
	Point>, ///< VertexProperties
	boost::property<
	boost::edge_index_t,
	unsigned,
	Link>, ///< EdgeProperties
	Node, ///< GraphProperties
	boost::vecS ///< EdgeList
	>> TTGT; ///< Two Tree Graph Type

struct Reflex
{
	Reflex(TTGT &g) :graphRef(g) {};
	~Reflex() {};
	TTGT &graphRef;
};

class TwoTree
{
	typedef TTGT graph_t;

public:
	/**
	 * Class for flip.
	 */
	class Flip
	{
	public:
		Flip();
		Flip(unsigned);
		~Flip();
		bool operator[] (unsigned i) const { return flip[i]; }
		void next();
		bool isBegin();
		bool flipAt(unsigned);
	private:
		std::vector<bool> flip;
	};

	TwoTree(char* filepath);
	~TwoTree();
	void print_vertices() const;
	void print_edges() const;
	void print_graph() const;
	void generateDRplan();
	void printDRplan();
	void realize();
	Flip flip;
	graph_t graph;
    std::function<std::pair<double,double>(unsigned)> getInterval;
    std::function<void(unsigned, std::pair<double,double>)> setInterval;
    void resetInterval();
    Node &getRoot();

private:
	// std::vector<boost::subgraph<graph_t>> subgraphList;
	//void generateDRplan(graph_t&);
	void printDRplan(graph_t&);
	const boost::local_property<boost::graph_bundle_t> GraphBundle;
};
