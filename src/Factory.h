#pragma once

#include "stdafx.h"
#include "Node.h"
#include "Tree.h"
#include "Polynomial.hpp"

namespace DRPlan {

class Factory {
	friend class Tree;
	friend class Node;

public:

	Factory();
	Factory(int sampleNum, std::unordered_map<int, std::pair<double, double>> intervalList);
	~Factory();

    void setSampleNum(int);
	void setIntervalList(std::unordered_map<int, std::pair<double, double>>);
	Tree *createTree(Node *root);
	Node *createNode(Polynomial poly, std::vector<int> freeVars, int solvedVar);
	Node *appendNode(Node* parent, Node* child);
	Node *createVar(int index);
	std::vector<Node*> subListOf(Node* parent);

private:
	int sampleNum;
	std::unordered_map<int, std::pair<double, double>> intervalList;
    Node *root;

};

}