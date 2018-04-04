#pragma once

#include "stdafx.h"
#include "BlackBox.hpp"

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

namespace DRPLAN {

/**
 * Vertex Bundled Properties
 */
struct Point
{
    static double distance(Point a, Point b)
    {
        return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
    };

    static double CCW(Point a, Point b, Point c)
    {
        return (-a.y * b.x + a.x * b.y + a.y * c.x - b.y * c.x - a.x * c.y + b.x * c.y);
    };

    Point(double x = 0, double y = 0)
            :x(x), y(y)
    {
    };

    void setXY(double x, double y) {
        this->x = x;
        this->y = y;
    }

    std::string toString() {
        return "(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    }

    double x, y;
};

enum EdgeType { partial, dropped, added };

/**
 * Edge Bundled Properties
 */

struct Link
{
    static double getEps()
    {
        return 2E-8;
    }

    Link()
            :distance(100.0f), interval(std::make_pair(getEps(), 200.0f - getEps()))
    {}

    EdgeType edge_type;
    double distance;
    std::pair<double, double> interval;
};

struct Reflex;
class TwoTree;

struct Node
{
    double targetLength = 100;
    Reflex *reflex;
    void generateDRplan();
    void printDRplan() const;
    void exportGraphviz(std::string = "") const;
    void realize(std::unordered_map<unsigned, double>);
    const TwoTree *tt;
    bool isCayleyNode = false;
    std::pair<double, double> interval;
    std::unordered_set<unsigned> freeCayley;
    std::unordered_set<unsigned> allCayley;
    unsigned targetDrop;
    unsigned targetCayley;
    double dropDiff();
    std::pair<double, double> dropFlip();
    std::vector<Node *> subNodes;
    std::string toString() const;
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
    EdgeDesc<TTGT> targetEdge;
    EdgeDesc<TTGT> droppedEdge;
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

    TwoTree(std::string, bool = false);
    ~TwoTree();
    void print_vertices() const;
    void print_edges() const;
    void print_graph() const;
    void generateDRplan();
    void printDRplan() const;
    void realize(std::unordered_map<unsigned, double>, std::string);
    Node &getRoot();
    graph_t graph;
    Flip flip;
    Flip dropFlip;

private:
    const boost::local_property<boost::graph_bundle_t> GraphBundle;
};

}

