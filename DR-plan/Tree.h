#pragma once

#include "stdafx.h"
#include "Factory.h"
#include "BlackBox.hpp"
#include "Enumeration.h"
#include "RootFinder.h"
#include "bspline.h"
#include "bsplinebuilder.h"
#include <assert.h>

namespace DRPlan {

template<typename Node>
class Tree {
//    friend class Factory;
//	  friend class Node;

public:
    Tree() {};

	Tree(Node *root, int sampleNum, std::unordered_map<int, std::pair<double, double>> intervalList)
			: root(root), sampleNum(sampleNum), intervalList(intervalList)
	{};

    ~Tree() {};

    std::vector<std::unordered_map<int, double>> solveTree()
	{
		solveNode(root);
		for(const auto& solutionList : solutionMapList) {
			std::unordered_map<int, double> finalSolutionList;
			for(const auto &solution : solutionList) {
				finalSolutionList[solution.first] = solution.second.evaluate({});
			}
			finalSolutionLists.push_back(finalSolutionList);
		}
		return finalSolutionLists;
	};

    std::vector<std::unordered_map<int, double>> finalSolutionLists; /**< A list of maps that stores all the possible solutions.
																	 Each solution maps a variable index to its corresponding value.*/

private:
    Node *root;
    int sampleNum; ///< The number of samples for interpolation.
    std::unordered_map<int, std::pair<double, double>> intervalList; ///< A map that stores the interval of the interpolation samples.
    std::vector<std::unordered_map<int, Polynomial>> solutionMapList; /**< A list of maps that stores all current possible solutions.
																	Each solution map contains the representation for each variable
																	which only involves the free variables .
																	*/

    void solveNode(Node *curNode)
    {
        /**
         * If current node is a variable node, the index of target variable should be -1.
         * We simply add current pair of variable and its constant polynomial into every solution map.
         */
        if(curNode->targetVar == -1) {
            /**
             * There is no solution map.
             */
            if(solutionMapList.empty()) {
                solutionMapList.push_back(std::unordered_map<int, Polynomial>());
            }
            /**
             * The variable is not present in existing solution map.
             */
            if(solutionMapList[0].count(curNode->targetVar) == 0) {
                for(auto & solutionList : solutionMapList) {
                    solutionList[curNode->freeVars[0]] = curNode->targetPoly;
                }
            }
            return;
        }
        /**
         * Solve every sub node.
         */
        for(auto &subNode : curNode->subNodes) {
            solveNode(subNode);
        }
        /**
         * Solve the current node based on every possible solution.
         */
        std::cout << std::endl << "Solving x_" << curNode->targetVar << "(";
        for(int i = 0; i < curNode->freeVars.size(); i++) {
            if(i != 0) std::cout << ", ";
            std::cout << "x_" << curNode->freeVars[i];
        }
        std::cout << "):" << std::endl;
        std::vector<std::unordered_map<int, Polynomial>> newSolutionLists;
        for(const auto &solutionList : solutionMapList) {
            Polynomial curriedPoly = curNode->curryPoly(solutionList);
            std::vector<Polynomial> interPolys = interpolate(curriedPoly, curNode->targetVar);
            std::cout << "found " << interPolys.size() << " solution(s)." << std::endl;
            for(auto &interPoly : interPolys) {
                newSolutionLists.push_back(updateSolution(solutionList, interPoly, curNode->targetVar));
                //double begin = -0.44f;
                //double end = -0.32f;
                //for(int i = 0; i <= sampleNum; i++) {
                //	std::unordered_map<int, double> valMap;
                //	valMap[0] = begin + (double)i * (end - begin) / (double)sampleNum;
                //	std::cout << "x_0(" << valMap[0] << ") = ";
                //	std::cout << interPoly.evaluate(valMap) << std::endl;
                //	std::cout << newSolutionLists.back().at(curNode->targetVar).evaluate(valMap) << std::endl;
                //}
            }
        }
        solutionMapList = newSolutionLists;
        return;
    };

	/**
	 * Interpolates the polynomial to solve the roots
	 * and represent the target variable in terms of free variables.
	 * @param poly the polynomial to solve.
	 * @param solvedVar the index of the target variable.
	 * @return A list of representations of the target variable.
	 */
    std::vector<Polynomial> interpolate(Polynomial poly, int targetVar) const
    {
        std::unordered_set<int> freeVarSet = poly.getVarSet();
        freeVarSet.erase(targetVar);
        // The free degree should be no more than 5.
        assert(freeVarSet.size() <= 5);
        /**
         * Compute the sample values of all free variables.
         */
        std::unordered_map<int, std::vector<double>> sampleList;
        for(const auto &freeVar : freeVarSet) {
            // sampleList[freeVar] = std::vector<double>();
            double begin = intervalList.at(freeVar).first;
            double end = intervalList.at(freeVar).second;
            for(int i = 0; i <= sampleNum; i++) {
                sampleList[freeVar].push_back(begin + (double)i * (end - begin) / (double)sampleNum);
            }
        }
        /**
         * For each combination of the free variable values,
         * interpolate the sample values of target variable against the values of polynomial,
         * and find the root of the target variable.
         */
        std::vector<std::unordered_map<int, double>> freeValsList;
        std::vector<std::vector<double>> rootsList;
        double targetBegin = intervalList.at(targetVar).first;
        double targetEnd = intervalList.at(targetVar).second;
        std::function<bool(std::unordered_map<int, double>)> findRoot =
                [&](std::unordered_map<int, double> freeVals) -> bool {
                    SPLINTER::DataTable dataTable;
                    for(int i = 0; i <= sampleNum; i++) {
                        std::unordered_map<int, double> valMap(freeVals);
                        valMap[targetVar] = targetBegin + (double)i * (targetEnd - targetBegin) / (double)sampleNum;
                        try {
                            double polyRes = poly.evaluate(valMap);
                            dataTable.addSample(valMap[targetVar],polyRes);
                        } catch(std::exception e) {
                            continue;
                        }
                    }
                    SPLINTER::BSpline slicedInter = SPLINTER::BSpline::Builder(dataTable).degree(3).build();
                    std::vector<double> roots = RootFinder::findZeros(slicedInter, 3);
                    if(roots.size()) {
                        freeValsList.push_back(freeVals);
                        rootsList.push_back(roots);
                        return true;
                    }
                    return false;
                };
        /**
         * If there is no samples for free variable, either there is no free variables,
         * or there is no interval available for all free variables,
         * interpolate and find root with empty map.
         * Otherwise, enumerate all possible combinations of the sample values
         * and interpolate for every combination.
         */
        if(freeVarSet.empty()) {
            findRoot({});
        } else {
            Enumeration<double, int> enumFree = Enumeration<double, int>(sampleList, freeVarSet);
            enumFree.applyFunc(findRoot);
        }
        /**
         * Returns empty root list if there is no root everywhere.
         * Otherwise enumerate all possible combinations of the roots.
         */
        if(rootsList.empty()) {
            return std::vector<Polynomial>();
        }
        Enumeration<double> rootsEnum(rootsList);
        if(freeVarSet.empty()) {
            /**
            * Since there is no sample values for free variables,
            * there is only one root in each rootList.
            */
            std::function<Polynomial(std::vector<double>)> constRoot =
                    [](std::vector<double> rootList) -> Polynomial {
                        return Polynomial::constFunc(rootList.front());
                    };
            return rootsEnum.applyFunc(constRoot);
        } else {
            std::function<Polynomial(std::vector<double>)> interRoot =
                    [&](std::vector<double> rootList) -> Polynomial {
                        SPLINTER::DataTable dataTable; ///< Use SPLINTER to generate multivariant interpolant.
                        for(int i = 0; i < rootList.size(); i++) {
                            std::vector<double> freeValList;
                            /**
                             * Note that the order of iteration will not be changed
                             * as long as the set is not changed or re-hashed.
                             * @see: https://stackoverflow.com/questions/36242103/is-order-of-iteration-over-the-elements-of-stdunordered-set-guaranteed-to-be-a
                             */
                            for(auto &freeVarIndex : freeVarSet) {
                                freeValList.push_back(freeValsList[i][freeVarIndex]);
                            }
                            dataTable.addSample(freeValList, rootList[i]);
                        }
                        SPLINTER::BSpline bspline = SPLINTER::BSpline::Builder(dataTable).degree(3).build();
                        auto x = bspline.getControlPoints();
                        std::cout << x << std::endl;
                        return Polynomial([=](std::unordered_map<int, double> valMap) -> double {
                            std::vector<double> valList;
                            for(auto &varIndex : freeVarSet) {
                                valList.push_back(valMap[varIndex]);
                            }
                            return bspline.eval(valList);
                        }, freeVarSet);
                    };
            return rootsEnum.applyFunc(interRoot);
        }
    };


	/**
	 * Uses the free variables to replace the target variable wherever it occurs in the previous solutionMap.
	 * @param solutionMap the solution map where the representation was solved.
	 * @param reps the representation of the target variable in terms of the free variables.
	 * @param targetVar the index of the target variable.
	 * @return the new solutionMap with no target variable appears.
	 */
    std::unordered_map<int, Polynomial> updateSolution(const std::unordered_map<int, Polynomial> &solutionMap, const Polynomial &reps, int targetVar) const
    {
        std::unordered_map<int, Polynomial> newSolutionList(solutionMap);
        newSolutionList[targetVar] = reps;
        for(const auto &kvPair : solutionMap) {
            if(kvPair.first != targetVar && kvPair.second.getVarSet().count(targetVar)) {
                Polynomial *newPoly = new Polynomial(reps);
                Polynomial *oldPoly = new Polynomial(kvPair.second);
                newSolutionList[kvPair.first] = Polynomial([=](std::unordered_map<int, double> valMap) -> double {
                    double test = newPoly->evaluate(valMap);
                    valMap[targetVar] = newPoly->evaluate(valMap);
                    return oldPoly->evaluate(valMap);
                }, newPoly->getVarSet());
            }
        }
        return newSolutionList;
    };
};

}
