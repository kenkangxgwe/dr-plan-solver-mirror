#include "stdafx.h"
#include "Tree.h"
#include "Enumeration.h"
#include "bspline.h"
#include "bsplinebuilder.h"

namespace DRPlan {

/**
 Constructs a DR-plan by reading the specified file and generates node.
 \param DRPlanPath the path of the DR-plan file
 */
Tree::Tree() {
}

Tree::Tree(DRPlan::Node *root, int sampleNum, std::unordered_map<int, std::pair<double, double>> intervalList) 
	: root(root), sampleNum(sampleNum), intervalList(intervalList) {
}

Tree::~Tree() {
	delete root;
}

/**
  Calls the root node to recursively solve the constraint polynomials.
  \return the list of all possible solutions of all variables
 */
std::vector<std::unordered_map<int, double>> Tree::solveTree() {
	solveNode(root);
	for(auto& solutionList : solutionMapList) {
		std::unordered_map<int, double> finalSolutionList;
		for(auto &solution : solutionList) {
			finalSolutionList.emplace(solution.first, solution.second.evaluate(std::unordered_map<int, double>()));
		}
		finalSolutionLists.push_back(finalSolutionList);
	}
	return finalSolutionLists;
}

void Tree::solveNode(Node *curNode) {

	if(curNode->targetVar == -1) {
		if(solutionMapList.empty()) {
			solutionMapList.push_back(std::unordered_map<int, Polynomial>());
		}

		if(solutionMapList[0].count(curNode->targetVar) == 0) {
			for(auto & solutionList : solutionMapList) {
				solutionList.emplace(curNode->freeVars[0], curNode->targetPoly);
			}
		}
		return;
	}

	for(auto &subNode : curNode->subNodes) {
		solveNode(subNode);
	}

	std::vector<std::unordered_map<int, Polynomial>> newSolutionLists;
	std::cout << std::endl << "Solving x_" << curNode->targetVar << "(";
	for(int i = 0; i < curNode->freeVars.size(); i++) {
		if(i != 0) std::cout << ", ";
		std::cout << "x_" << curNode->freeVars[i];
	}
	std::cout << "):" << std::endl;

	for(auto &solutionList : solutionMapList) {
		Polynomial curriedPoly = curNode->curryPoly(solutionList);
		std::vector<Polynomial> interPolys = interpolate(curriedPoly, curNode->targetVar);
		std::cout << "found " << interPolys.size() << " solution(s)." << std::endl;
		for(auto &interPoly : interPolys) {
			newSolutionLists.push_back(updateSolution(solutionList, interPoly, curNode->targetVar));
			//double begin = -0.44f;
			//double end = -0.32f;
			//for(int i = 0; i <= sampleNum; i++) {
			//	std::unordered_map<int, double> valMap;
			//	valMap.emplace(0, begin + (double)i * (end - begin) / (double)sampleNum);
			//	std::cout << "x_0(" << valMap[0] << ") = ";
			//	std::cout << interPoly.evaluate(valMap) << std::endl;
			//	std::cout << newSolutionLists.back().at(curNode->targetVar).evaluate(valMap) << std::endl;
			//}
		}
	}
	solutionMapList = newSolutionLists;
	return;
}

std::vector<Polynomial> Tree::interpolate(Polynomial poly, int targetVar) {
	std::unordered_set<int> freeVarSet = poly.getVarSet();
	freeVarSet.erase(targetVar);

	// The free degree should be no more than 5.
	assert(freeVarSet.size() <= 5); 

	std::unordered_map<int, std::vector<double>> sampleList;
	for(auto &freeVar : freeVarSet) {
		sampleList.emplace(freeVar, std::vector<double>());
		double begin = intervalList[freeVar].first;
		double end = intervalList[freeVar].second;
		for(int i = 0; i <= sampleNum; i++) {
			sampleList[freeVar].push_back(begin + (double)i * (end - begin) / (double)sampleNum);
		}
	}


	std::vector<std::unordered_map<int, double>> freeValsList;
	std::vector<std::vector<double>> rootsList;

	double targetBegin = intervalList[targetVar].first;
	double targetEnd = intervalList[targetVar].second;

	std::function<bool(std::unordered_map<int, double>)> interRoot =
		[&](std::unordered_map<int, double> freeVals) -> bool {
		std::vector<double> xList, yList;
		interpolant slicedInter;
		for(int i = 0; i <= sampleNum; i++) {
			std::unordered_map<int, double> valMap(freeVals);
			valMap.emplace(targetVar,
				targetBegin + (double)i * (targetEnd - targetBegin) / (double)sampleNum);
			try {
				double polyRes = poly.evaluate(valMap);
				xList.push_back(valMap[targetVar]);
				yList.push_back(polyRes);
			}
			catch (std::exception e) {
				continue;
			}
		}
		slicedInter.set_data(xList, yList);

		std::vector<double> roots = slicedInter.find_roots();
		if(roots.size()) {
			freeValsList.push_back(freeVals);
			rootsList.push_back(roots);
			return true;
		}
		return false;
	};

	if(freeVarSet.empty()) {
		interRoot(std::unordered_map<int, double>());
	}
	else {
		Enumeration<double, int> enumFree = Enumeration<double, int>(sampleList, freeVarSet);
		enumFree.applyFunc(interRoot);
	}

	if(rootsList.empty()) {
		return std::vector<Polynomial>();
	}

	Enumeration<double> rootsEnum(rootsList);

	if(freeVarSet.empty()) {
		std::function<Polynomial(std::vector<double>)> constProjFunc =
			[](std::vector<double> valList) -> Polynomial {
			return Polynomial::constFunc(valList[0]);
		};
		return rootsEnum.applyFunc(constProjFunc);
	}
	else {
		std::function<Polynomial(std::vector<double>)> interFunc =
			[&](std::vector<double> rootList) -> Polynomial {
			SPLINTER::DataTable dataTable;
			for(int i = 0; i < rootList.size(); i++) {
				std::vector<double> freeValList;
				for(auto &freeVarIndex : freeVarSet) {
					freeValList.push_back(freeValsList[i][freeVarIndex]);
				}
				dataTable.addSample(freeValList, rootList[i]);
			}
			SPLINTER::BSpline bspline = SPLINTER::BSpline::Builder(dataTable).degree(3).build();
			return Polynomial([=](std::unordered_map<int, double> valMap) -> double {
				std::vector<double> valList;
				for(auto &varIndex : freeVarSet) {
					valList.push_back(valMap[varIndex]);
				}
				return bspline.eval(valList);
			}, freeVarSet);
		};
		return rootsEnum.applyFunc(interFunc);
	}
}

std::unordered_map<int, Polynomial> Tree::updateSolution(std::unordered_map<int, Polynomial> &solutionMap, Polynomial reps, int targetVar) {
	std::unordered_map<int, Polynomial> newSolutionList(solutionMap);
	newSolutionList[targetVar] = reps;
	for(auto &kvPair : solutionMap) {
		if(kvPair.first != targetVar && kvPair.second.getVarSet().count(targetVar)) {
			Polynomial *newPoly = new Polynomial(reps);
			Polynomial *oldPoly = new Polynomial(kvPair.second);
			newSolutionList[kvPair.first] = Polynomial([=](std::unordered_map<int, double> valMap) -> double {
				double test = newPoly->evaluate(valMap);
				valMap.emplace(targetVar, newPoly->evaluate(valMap));
				return oldPoly->evaluate(valMap);
			}, newPoly->getVarSet());
		}
	}
	return newSolutionList;
}

}
