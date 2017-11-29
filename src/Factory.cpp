#include "stdafx.h"
#include "Factory.h"

DRPlan::Factory::Factory() {

}

DRPlan::Factory::Factory(int sampleNum, std::unordered_map<int, std::pair<double, double>> intervalList)
        :sampleNum(sampleNum), intervalList(intervalList) {
}

DRPlan::Factory::~Factory() {

}

void DRPlan::Factory::setSampleNum(int inputSampleNum) {
    sampleNum = inputSampleNum;
    return;
}

void DRPlan::Factory::setIntervalList(std::unordered_map<int, std::pair<double, double>> inputIntervalList) {
    intervalList = inputIntervalList;
    return;
}


DRPlan::Tree* DRPlan::Factory::createTree(DRPlan::Node *root) {
    this->root = root;
    return new Tree(root, sampleNum, intervalList);
}

DRPlan::Node *DRPlan::Factory::createNode(Polynomial poly, std::vector<int> freeVars, int solvedVar) {
	return new Node(poly, freeVars, solvedVar);
}

DRPlan::Node *DRPlan::Factory::appendNode(DRPlan::Node *parent, DRPlan::Node *child) {
    parent->subNodes.push_back(child);
	child->parentNode = parent;
    return child;
}

DRPlan::Node *DRPlan::Factory::createVar(int index)
{
	Node *varNode = new Node(Polynomial::projectFunc(index), { index }, -1);
	return varNode;
}

std::vector<DRPlan::Node *> DRPlan::Factory::subListOf(DRPlan::Node * parent) {
    return parent->subNodes;
}
