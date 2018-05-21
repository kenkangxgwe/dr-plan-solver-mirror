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
using AdjVerIter = typename boost::graph_traits<GraphType>::adjacency_iterator;
template<typename GraphType>
using VerDesc = typename boost::graph_traits<GraphType>::vertex_descriptor;
template<typename GraphType>
using EdgeDesc = typename boost::graph_traits<GraphType>::edge_descriptor;

namespace DRPLAN
{

struct PointReflex;

/**
 * Vertex Bundled Properties
 */
struct Point
{
    static inline double distance(const Point a, const Point b)
    {
        return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
    };

    static inline double CCW(const Point a, const Point b, const Point c)
    {
        return (-a.y * b.x + a.x * b.y + a.y * c.x - b.y * c.x - a.x * c.y + b.x * c.y);
    };

    Point(double x = 0, double y = 0)
            : x(x), y(y)
    {
    };

    void setXY(const double x, const double y)
    {
        this->x = x;
        this->y = y;
    }

    std::string toString() const
    {
        return "(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    }

    PointReflex *pointReflex;
    double x, y;
};

inline std::ostream& operator<<(std::ostream & os, const Point pt) {
    return os << pt.toString();
}

enum class EdgeType:unsigned
{
    PARTIAL, DROPPED, ADDED, DISPLACEMENT
};

inline std::ostream &operator<<(std::ostream &os, const EdgeType type)
{
    switch(type) {
        case EdgeType::PARTIAL: {
            return os << "Partial";
        }
        case EdgeType::ADDED: {
            return os << "Added";
        }
        case EdgeType::DROPPED: {
            return os << "Dropped";
        }
        case EdgeType::DISPLACEMENT: {
            return os << "Displacement";
        }
    }
}


/**
 * Edge Bundled Properties
 */
struct Link
{
    static constexpr const char* PARTIAL_COLOR = "black";
    static constexpr const char* DROPPED_COLOR = "red";
    static constexpr const char* ADDED_COLOR = "green";
    static constexpr const char* DISPLACEMENT_COLOR = "blue";

    static inline double getEps()
    {
        return 2E-8;
    }

    static inline EdgeType getEdgeType(const std::string color)
    {
        if(color == PARTIAL_COLOR) {
            return EdgeType::PARTIAL;
        }
        if(color == DROPPED_COLOR) {
            return EdgeType::DROPPED;
        }
        if(color == ADDED_COLOR) {
            return EdgeType::ADDED;
        }
        throw("There is no corresponding type for color \"" + color + "\".");
    }

    static inline std::string getEdgeColor(EdgeType type)
    {
        switch(type) {
            case EdgeType::PARTIAL: {
                return PARTIAL_COLOR;
            }
            case EdgeType::DROPPED: {
                return DROPPED_COLOR;
            }
            case EdgeType::ADDED: {
                return ADDED_COLOR;
            }
            case EdgeType::DISPLACEMENT: {
                return DISPLACEMENT_COLOR;
            }
        }
    }

    Link()
            : distance(100.0f), interval(std::make_pair(getEps(), 200.0f - getEps())) {}

    EdgeType edge_type;
    double distance;
    std::pair<double, double> interval;
};

struct Reflex;

class TwoTree;

struct Node
{
    void generateDRplan(); ///< Generates the DR-Plan beneath current DR-Node.
    void printDRplan() const; ///< Prints the generated DR-Plan.
    void exportGraphviz(std::string = "") const; ///< Exports GraphViz dot file.
    void realize(std::unordered_map<unsigned, double>);  ///< Realizes the DR-Plan.
    void calcInterval(); ///< Calculates the interval of Cayley node.
    double dropDiff(); ///< Calculates the difference in length between actual and expect target dropped edge.
    std::pair<double, double> dropFlip(); ///< Determines the dropped edge's flip.
    void findFlip(); ///< Find the vertices that determine the dropped edge's flip.
    std::string toString() const; ///< Describes current DR-Node.

    Reflex *reflex; ///< A pointer avoiding cross-reference
    const TwoTree *tt; ///< A Pointer to the root two-tree
    bool isCayleyNode = false; ///< A predication whether the DR-Node is a Cayley node
    std::pair<double, double> interval; ///< The interval of sampling
    std::vector<unsigned> freeCayley; ///< All free Cayley parameters in this this and sub nodes
    std::vector<unsigned> allCayley; ///< All cayley parameters in this this and sub nodes
    double targetLength = 100; ///< The length of the target dropped edge
    unsigned targetDrop; ///< The index of dropped edge
    unsigned targetCayley; ///< The index of target Cayley parameter
    std::vector<Node *> subNodes; ///< A list of sub DR-Nodes pointers.
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

/**
 * The struct that saves the two edges that connect a vertex to the two-tree.
 */
struct PointReflex
{
    EdgeDesc<TTGT> e1; ///< The first edge descriptor
    EdgeDesc<TTGT> e2; ///< The second edge descriptor
};

struct Reflex
{
    Reflex(TTGT &g) : graphRef(g) {};

    ~Reflex() {};
    TTGT &graphRef; ///< The root two-tree
    EdgeDesc<TTGT> targetEdge; ///< The dropped flip edge descriptor
    EdgeDesc<TTGT> droppedEdge; /// < The target dropped edge descriptor
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
        void next();
        bool isBegin();
        bool flipAt(unsigned);

        bool operator[](unsigned i) const { return flip[i]; }

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
    void getSupportEdges();
    const boost::local_property<boost::graph_bundle_t> GraphBundle;
};

}

