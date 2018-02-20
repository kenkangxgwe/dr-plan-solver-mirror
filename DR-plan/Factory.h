#pragma once

#include "stdafx.h"
//#include "Node.h"
#include "Tree.h"
#include "utils/BlackBox.hpp"

namespace DRPlan {

template<typename Node>
class Factory {
//	friend class Tree<Node>;
//	friend class Node;

public:

	Factory() {};
	Factory(unsigned sampleNum, std::unordered_map<unsigned, std::pair<double, double>> intervalList)
			:sampleNum(sampleNum), intervalList(intervalList) {};
	~Factory() {};

	void setSampleNum(unsigned inputSampleNum) {
		sampleNum = inputSampleNum;
		return;
	};

	void setIntervalList(std::unordered_map<unsigned, std::pair<double, double>> inputIntervalList) {
		intervalList = inputIntervalList;
		return;
	};

	Tree<Node>* createTree(Node *root) {
		this->root = root;
		return new Tree<Node>(root, sampleNum, intervalList);
	};

	Node* createNode(Polynomial poly, std::vector<unsigned> freeVars, unsigned solvedVar) {
		return new Node(poly, freeVars, solvedVar);
	};

	Node* appendNode(Node *parent, Node *child) {
		parent->subNodes.push_back(child);
		child->parentNode = parent;
		return child;
	};

	Node* createVar(unsigned index)
	{
		Node *varNode = new Node(Polynomial::projectFunc(index), { index }, -1);
		return varNode;
	};

	std::vector<Node *> subListOf(Node * parent) {
		return parent->subNodes;
	};

private:
	unsigned sampleNum;
	std::unordered_map<unsigned, std::pair<double, double>> intervalList;
    Node *root;

};

}