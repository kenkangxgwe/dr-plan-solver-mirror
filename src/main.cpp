/*
 *   main.cpp
 */

#include "stdafx.h"
#include "Factory.h"
#include "Tree.h"
#include "Node.h"
#include "Polynomial.hpp"

using namespace std;

/*
 *  A data structure to represent the ordering of (constraint, parameter)
 *  pairs still needs to be implemented.
 *  I suggest a std::pair(parameter* p, constraint* c) data structure where
 *  parameter p gets eliminated in constraint c.
 *  Then a std::vector(pairs) can represent the ordering.
 */

int main()
{
	unordered_map<int, double> valMap;
	valMap.emplace(0, -0.411438);
	valMap.emplace(1, 0.911438);

	vector<Polynomial> polyList;

	polyList.push_back(Polynomial([](unordered_map<int, double> valList) -> double {
		return pow(valList[0], 2) + pow(valList[1], 2) - 1.0;
		//return pow(valList[0], 3) + pow(valList[1], 3);
		//return valList[0] + valList[1];
	}, { 0, 1 }));
	polyList.push_back(Polynomial([](unordered_map<int, double> valList) -> double {
		return pow(valList[0] - 0.5, 2) + pow(valList[1] - 0.5, 2) - 1.0;
		//return pow(valList[0], 3) - pow(valList[1], 3);
		//return valList[0] - valList[1] + 0.1;;
	}, { 0, 1 }));

	// Evaluate at first zero:
	cout << "First Zero:" << endl;
	cout << "p1(-0.411438, 0.911438) = " << polyList[0].evaluate(valMap) << endl;
	cout << "p2(-0.411438, 0.911438) = " << polyList[1].evaluate(valMap) << endl;
	cout << endl;

	// Evaluate at second zero:
	valMap[0]=0;
	valMap[1]=0;

	cout << "Second Zero:" << endl;
	cout << "p1(0,0) = " << polyList[0].evaluate(valMap) << endl;
	cout << "p2(0,0) = " << polyList[1].evaluate(valMap) << endl;
	cout << endl;

	// intervalList for two circles
	unordered_map<int, pair<double, double>> intervalList;
	intervalList.emplace(0, pair<double, double>(-0.5, -0.3));
	intervalList.emplace(1, pair<double, double>(0.8, 1.1));

	// intervalList for two cubics
	//unordered_map<int, pair<double, double>> intervalList;
	//intervalList.emplace(0, pair<double, double>(-0.1, 0.1));
	//intervalList.emplace(1, pair<double, double>(-0.1, 0.1));

	// A simple DR-plan Factory.
	// solve x1 first, and then x0.
    DRPlan::Factory factory(10, intervalList);
    DRPlan::Node* polyNode0 = factory.createNode(polyList[0], {}, 0);
    DRPlan::Node* polyNode1 = factory.createNode(polyList[1], {0}, 1);
	DRPlan::Tree* tree0 = factory.createTree(polyNode0);
    factory.appendNode(polyNode0, polyNode1);
	DRPlan::Node* varNode0 = factory.createVar(0);
	DRPlan::Node* varNode1 = factory.createVar(1);
    factory.appendNode(polyNode1, varNode0);
    factory.appendNode(polyNode1, varNode1);
    tree0->solveTree();

	cout << endl;
	for (const auto &solution : tree0->finalSolutionLists) {
        for (int i = 0; i < valMap.size(); i++) {
			cout << " x_" << i << " = " << setw(5) << solution.at(i) << "\t";
		}
		cout << endl;
	}

	// Construct another DR-plan.
	// solve x0 first, and then the x1.
    DRPlan::Node* polyNode2 = factory.createNode(polyList[0], {}, 1);
    DRPlan::Node* polyNode3 = factory.createNode(polyList[1], {1}, 0);
	DRPlan::Tree* tree1 = factory.createTree(polyNode2);
    factory.appendNode(polyNode2, polyNode3);
	DRPlan::Node* varNode2 = factory.createVar(0);
	DRPlan::Node* varNode3 = factory.createVar(1);
    factory.appendNode(polyNode3, varNode2);
    factory.appendNode(polyNode3, varNode3);
    tree1->solveTree();

	cout << endl;
	for (const auto &solution : tree1->finalSolutionLists) {
        for (int i = 0; i < valMap.size(); i++) {
			cout << " x_" << i << " = " << setw(5) << solution.at(i) << "\t";
		}
		cout << endl;
	}

	return 0;
}
