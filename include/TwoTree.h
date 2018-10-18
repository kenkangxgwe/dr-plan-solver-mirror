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

#include "BlackBox.hpp"
#include "stdafx.h"

namespace DRPLAN
{

using index_t = unsigned;

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

    friend std::ostream &operator<<(std::ostream &, const Point &);

    std::string toString() const
    {
        return "(" + std::to_string(x) + ", " + std::to_string(y) + ")";
    }

    double x, y;
    index_t e1; ///< The first support edge index
    index_t e2; ///< The second support edge index
};

enum class EdgeType : unsigned
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
        default: {
            throw ("Invalide EdgeType.");
        }
    }
}


/**
 * Edge Bundled Properties
 */
struct Link
{
    static constexpr const char *PARTIAL_COLOR = "black";
    static constexpr const char *DROPPED_COLOR = "red";
    static constexpr const char *ADDED_COLOR = "green";
    static constexpr const char *DISPLACEMENT_COLOR = "blue";

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
        throw ("There is no corresponding type for color \"" + color + "\".");
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
            default: {
                throw ("Invalide EdgeType.");
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
    bool isCayleyNode = false; ///< A predication whether the DR-Node is a Cayley node
    std::pair<double, double> interval; ///< The interval of sampling
    std::vector<unsigned> freeCayley; ///< All free Cayley parameters in this this and sub nodes
    std::vector<unsigned> allCayley; ///< All cayley parameters in this this and sub nodes
    double targetLength = 100; ///< The length of the target dropped edge
    index_t targetDrop; ///< The index of dropped edge
    index_t targetCayley; ///< The index of target Cayley parameter
    index_t dropFlipEdge; ///< The index of edge to determin dropped flip
};

class TwoTree
{

public:

    typedef boost::subgraph<
        boost::adjacency_list<
            boost::vecS, ///< OutEdgeList
            boost::vecS, ///< VertexList
            boost::undirectedS, ///< Directed
            boost::property<
                boost::vertex_index_t,
                index_t,
                Point>, ///< VertexProperties
            boost::property<
                boost::edge_index_t,
                index_t,
                Link>, ///< EdgeProperties
            Node, ///< GraphProperties
            boost::vecS ///< EdgeList
        >> graph_t;

    typedef graph_t::vertex_iterator VerIter;
    typedef graph_t::edge_iterator EdgeIter;
    typedef graph_t::vertex_descriptor VerDesc;
    typedef graph_t::edge_descriptor EdgeDesc;
    typedef graph_t::out_edge_iterator OutEdgeIter;
    typedef graph_t::adjacency_iterator AdjVerIter;
    typedef graph_t::children_iterator ChildIter;
    typedef graph_t::const_children_iterator CChildIter;

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

    TwoTree();
    explicit TwoTree(std::string, bool = false);
    TwoTree(TwoTree const &);
    ~TwoTree();
    void print_vertices() const;
    void print_edges() const;
    void print_graph() const;
    void generateDRplan();
    void printDRplan() const;
    size_t dropped_num() const;
    index_t getDroppedEdge(size_t) const;
    double changeDistanceBy(index_t, double);
    EdgeDesc const &operator[](index_t) const;
    Node &operator[](graph_t &graph);
    Node const &operator[](graph_t const &graph) const;
    void realize(std::unordered_map<unsigned, double>, std::string);

    Flip flip;
    Flip dropFlip;

    friend class Tree;

private:
    void updateEdgeList();
    void copy_node(graph_t &, graph_t const &);
    void getSupportEdges();
    void calcEdgeBoundaries();
    void generateDRplan(graph_t &); ///< Generates the DR-Plan beneath a sub-graph.
    void printDRplan(graph_t const &) const; ///< Prints the generated DR-Plan beneath a sub-graph.
    void findFlip(graph_t &);

    Node &get_node(graph_t &);
    Node const &get_node(graph_t const &) const;
    std::string toString(graph_t const &) const;
    std::string toStringFull(graph_t const &) const;
    void exportGraphviz(graph_t const &graph, std::string suffix) const;

    TwoTree &realize(graph_t &, std::unordered_map<unsigned, double> valMap);
    std::pair<double, double> getDropFlip(graph_t &);
    double dropDiff(graph_t &);
    std::pair<double, double> refineInterval(index_t, std::unordered_map<unsigned, double> valMap);

    graph_t m_graph;
    std::unordered_map<index_t, EdgeDesc> edge_map;
    std::vector<index_t> dropped_list;
    std::vector<index_t> added_list;
};

}

