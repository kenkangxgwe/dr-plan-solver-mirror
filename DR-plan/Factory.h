#pragma once

#include "stdafx.h"
//#include "Node.h"
#include "Tree.h"
#include "Polynomial.hpp"

namespace DRPlan {

template<typename Node>
class Factory {
//	friend class Tree<Node>;
//	friend class Node;

public:

	Factory() {};
	Factory(int sampleNum, std::unordered_map<int, std::pair<double, double>> intervalList)
			:sampleNum(sampleNum), intervalList(intervalList) {};
	~Factory() {};

	void setSampleNum(int inputSampleNum) {
		sampleNum = inputSampleNum;
		return;
	};

	void setIntervalList(std::unordered_map<int, std::pair<double, double>> inputIntervalList) {
		intervalList = inputIntervalList;
		return;
	};

	Tree<Node>* createTree(Node *root) {
		this->root = root;
		return new Tree<Node>(root, sampleNum, intervalList);
	};

	Node* createNode(Polynomial poly, std::vector<int> freeVars, int solvedVar) {
		return new Node(poly, freeVars, solvedVar);
	};

	Node* appendNode(Node *parent, Node *child) {
		parent->subNodes.push_back(child);
		child->parentNode = parent;
		return child;
	};

	Node* createVar(int index)
	{
		Node *varNode = new Node(Polynomial::projectFunc(index), { index }, -1);
		return varNode;
	};

	std::vector<Node *> subListOf(Node * parent) {
		return parent->subNodes;
	};

private:
	int sampleNum;
	std::unordered_map<int, std::pair<double, double>> intervalList;
    Node *root;

};

}