#pragma once

#include "stdafx.h"
#include "RootFinder.h"
#include "utils/BlackBox.hpp"
#include "utils/Enumeration.h"
#include "Splinter/bspline.h"
#include "Splinter/bsplinebuilder.h"

namespace DRPlan {

template<typename Node>
class Tree {
//    friend class Factory;
//	  friend class Node;

    typedef std::unordered_map<unsigned, double> DoubleMap;
    typedef std::pair<double, double> IntervalPair;
    typedef std::unordered_map<unsigned, Polynomial> SolutionMap;

public:
    Tree() {};

    Tree(Node *root, unsigned sampleNum, std::unordered_map<unsigned, IntervalPair> intervalList)
            : root(root), sampleNum(sampleNum), intervalList(intervalList)
    {
        this->getInterval = [=](unsigned index) -> IntervalPair {
            return this->intervalList[index];
        };

        this->setInterval = [=](unsigned index, IntervalPair updatePair) -> void {
            this->intervalList[index] = updatePair;
        };
    };

    Tree(Node *root, unsigned sampleNum, IntervalPair intervalList)
            : root(root), sampleNum(sampleNum)
    {
        this->getInterval = [=](unsigned) -> IntervalPair {
            return intervalList;
        };

        this->setInterval = [=](unsigned, IntervalPair updatePair) -> void {
            this->getInterval = [=](unsigned) -> IntervalPair {
                return updatePair;
            };
        };
    };

    Tree(Node *root, unsigned sampleNum,
         std::function<IntervalPair(unsigned)> getIntervalFunc,
         std::function<void(unsigned, IntervalPair)> setIntervalFunc)
            : root(root), sampleNum(sampleNum), getInterval(getIntervalFunc), setInterval(setIntervalFunc)
    {
    };

    ~Tree() {};

    bool solveTree()
    {
        solutionMapList.clear();
        finalSolutionLists.clear();
        if (!solveNode(root)) {
            return false;
        }
        for(const auto& solutionList : solutionMapList) {
            DoubleMap finalSolutionList;
            for(const auto &solution : solutionList) {
                finalSolutionList[solution.first] = solution.second.evaluate({});
            }
            finalSolutionLists.push_back(finalSolutionList);
        }
        return true;
    };

    std::vector<DoubleMap> finalSolutionLists; /**< A list of maps that stores all the possible solutions.
																	 Each solution maps a variable index to its corresponding value.*/

private:
    Node *root;
    unsigned sampleNum; ///< The number of samples for interpolation.
    std::unordered_map<unsigned,IntervalPair> intervalList; ///< A map that stores the interval of the interpolation samples.
    std::function<IntervalPair(unsigned)> getInterval;
    std::function<void(unsigned, IntervalPair)> setInterval; ///< A map that stores the interval of the interpolation samples.
    std::vector<SolutionMap> solutionMapList; /**< A list of maps that stores all current possible solutions.
																	Each solution map contains the representation for each variable
																	which only involves the free variables .
																	*/

    bool solveNode(Node *curNode)
    {
        /**
         * If current node is a variable node,
         * we simply add current pair of variable and its constant polynomial into every solution map.
         */
        if(curNode->isVarNode) {
            /**
             * There is no solution map.
             */
            if(solutionMapList.empty()) {
                solutionMapList.push_back(SolutionMap());
            }
            /**
             * The variable is not present in existing solution map.
             */
            if(solutionMapList[0].count(curNode->targetVar) == 0) {
                for(auto & solutionList : solutionMapList) {
                    solutionList[*(curNode->freeVars.begin())] = curNode->targetFunc;
                }
            }
            return true;
        }
        /**
         * Solve every sub node.
         */
        for(auto &subNode : curNode->subNodes) {
            if (!solveNode(subNode)) {
                return false;
            }
        }
        /**
         * Solve the current node based on every possible solution.
         */
        std::cout << "  Solving x_" << curNode->targetVar << "(";
        for(auto iter = curNode->freeVars.begin(); iter != curNode->freeVars.end(); ++iter) {
            if(iter != curNode->freeVars.begin()) std::cout << ", ";
            std::cout << "x_" << (*iter);
        }
        std::cout << "):" << std::endl;
        std::vector<SolutionMap> newSolutionLists;
        for(const auto &solutionList : solutionMapList) {
            Polynomial curriedPoly = curryFunc(curNode ,solutionList);
            std::vector<Polynomial> interPolys = interpolate(curriedPoly, curNode->targetVar, curNode);
            std::cout << "  Found " << interPolys.size() << " solution(s)." << std::endl;
            if (!interPolys.size()) {
                return false;
            }
            for(auto &interPoly : interPolys) {
                newSolutionLists.push_back(updateSolution(solutionList, interPoly, curNode->targetVar));
                //double begin = -0.44f;
                //double end = -0.32f;
                //for(unsigned i = 0; i <= sampleNum; i++) {
                //	DoubleMap valMap;
                //	valMap[0] = begin + (double)i * (end - begin) / (double)sampleNum;
                //	std::cout << "x_0(" << valMap[0] << ") = ";
                //	std::cout << interPoly.evaluate(valMap) << std::endl;
                //	std::cout << newSolutionLists.back().at(curNode->targetVar).evaluate(valMap) << std::endl;
                //}
            }
        }
        solutionMapList = newSolutionLists;
        return true;
    };

    Polynomial curryFunc(Node *curNode, const std::unordered_map<unsigned , Polynomial>& solutionList) const
    {
        std::unordered_set<unsigned> activeSet({curNode->targetVar}); ///< The set that stores the indices of all active variables (free variables and target variable).
        activeSet.insert(curNode->freeVars.begin(), curNode->freeVars.end());

        std::unordered_set<unsigned> solvedVars; ///< The set that stores the indices of all previously solved variables.
        for(auto &inVar : curNode->targetFunc.getVarList()) {
            if(!activeSet.count(inVar)) {
                solvedVars.emplace(inVar);
            }
        }

        return Polynomial([&, curNode, solvedVars](DoubleMap valMap) -> double {
            /**
             * Given the values of active variables, using the solutionList to calculated
             * the values of solved variables and insert them into the value map.
             */
            for(auto &solvedVar : solvedVars) {
                valMap[solvedVar] = solutionList.at(solvedVar).evaluate(valMap);
            }
            return curNode->targetFunc.evaluate(valMap);
        }, activeSet);
    }

    /**
     * Interpolates the polynomial to solve the roots
     * and represent the target variable in terms of free variables.
     * @param poly the polynomial to solve.
     * @param solvedVar the index of the target variable.
     * @return A list of representations of the target variable.
     */
    std::vector<Polynomial> interpolate(Polynomial poly, unsigned targetVar, Node *node) const
    {
        std::unordered_set<unsigned> freeVarSet = poly.getVarSet();
        freeVarSet.erase(targetVar);
        // The free degree should be no more than 5.
        assert(freeVarSet.size() <= 5);
        /**
         * Compute the sample values of all free variables.
         */
        std::unordered_map<unsigned, std::vector<double>> sampleList;
        for(const auto &freeVar : freeVarSet) {
            // sampleList[freeVar] = std::vector<double>();
            double begin = getInterval(freeVar).first;
            double end = getInterval(freeVar).second;
            std::cout<< "    Sampling x_" << freeVar << " from " << begin << " to " << end << "." << std::endl;
            for(unsigned i = 0; i <= sampleNum; i++) {
                sampleList[freeVar].push_back(begin + (double)i * (end - begin) / (double)sampleNum);
            }
        }
        /**
         * For each combination of the free variable values,
         * interpolate the sample values of target variable against the values of polynomial,
         * and find the root of the target variable.
         */
        std::vector<std::vector<DoubleMap>> freeValsGroupList;
        freeValsGroupList.push_back(std::vector<DoubleMap>());
        std::vector<std::vector<std::vector<double>>> rootsGroupList;
        rootsGroupList.push_back(std::vector<std::vector<double>>());
        double targetBegin = getInterval(targetVar).first;
        double targetEnd = getInterval(targetVar).second;
        std::function<bool(DoubleMap)> findRoot =
                [&](DoubleMap freeVals) -> bool {
                    SPLINTER::DataTable dataTable;
                    for(unsigned i = 0; i <= sampleNum; i++) {
                        DoubleMap valMap(freeVals);
                        valMap[targetVar] = targetBegin + (double)i * (targetEnd - targetBegin) / (double)sampleNum;
                        try {
                            double polyRes = poly.evaluate(valMap);
                            if(std::isnan(polyRes)){
                                continue;
                            }
                            // node->exportGraphviz("s" + std::to_string(i));
                            dataTable.addSample(valMap[targetVar],polyRes);
                        } catch(std::exception e) {
                            continue;
                        }
                    }
                    try {
                        if(dataTable.getNumSamples() <= 4) {
                            return false;
                        }
                        SPLINTER::BSpline slicedInter = SPLINTER::BSpline::Builder(dataTable).degree(3).build();
                        std::vector<double> roots = RootFinder::findZeros(slicedInter, 3);
                        if(!roots.size()) {
                            return false;
                        }
                        if (roots.size() > 2) {
                            std::cout << "    Found " << roots.size() << " roots. Quit." << std::endl;
                            return false;
                        }
                        /**
                         * If there is only one root, it will be in its own group.
                         */
                        if(roots.size() == 1) {
                            if(freeValsGroupList.back().size() != 0) {
                                freeValsGroupList.push_back(std::vector<DoubleMap>());
                                rootsGroupList.push_back(std::vector<std::vector<double>>());
                            }
                            rootsGroupList.back().push_back(roots);
                            rootsGroupList.push_back(std::vector<std::vector<double>>());
                            freeValsGroupList.back().push_back(freeVals);
                            freeValsGroupList.push_back(std::vector<DoubleMap>());
                            return true;
                        }
                        /**
                         * If two roots are close enough, they will be in their own group.
                         */
                        if(abs(roots[0] - roots[1]) < 1E-10) {
                            if(freeValsGroupList.back().size() != 0) {
                                freeValsGroupList.push_back(std::vector<DoubleMap>());
                                rootsGroupList.push_back(std::vector<std::vector<double>>(2));
                            } else {
                                rootsGroupList.back().insert(rootsGroupList.back().end(), 2, std::vector<double>());
                            }
                            rootsGroupList.back()[0].push_back(roots[0]);
                            rootsGroupList.back()[1].push_back(roots[1]);
                            rootsGroupList.push_back(std::vector<std::vector<double>>());
                            freeValsGroupList.back().push_back(freeVals);
                            freeValsGroupList.push_back(std::vector<DoubleMap>());
                            return true;
                        }
                        if(freeValsGroupList.back().size() == 0) {
                            rootsGroupList.back().insert(rootsGroupList.back().end(), 2, std::vector<double>());
                        }
                        rootsGroupList.back()[0].push_back(roots[0]);
                        rootsGroupList.back()[1].push_back(roots[1]);
                        freeValsGroupList.back().push_back(freeVals);
                        return true;
                    } catch(std::exception e) {
                        std::cout << "    Error here." << std::endl;
                        return false;
                    }
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
            Enumeration<unsigned, double> enumFree(sampleList, freeVarSet);
            enumFree.applyFunc(findRoot);
        }
        if(rootsGroupList.back().size() == 0) {
            freeValsGroupList.pop_back();
            rootsGroupList.pop_back();
        }
        /**
         * Returns empty root list if there is no root everywhere.
         * Otherwise enumerate all possible combinations of the roots.
         */
        if(rootsGroupList.empty()) {
            return std::vector<Polynomial>();
        }
        Enumeration<void, std::vector<double>> rootsEnum(rootsGroupList);
        if(freeVarSet.empty()) {
            /**
            * Since there is no sample values for free variables,
            * there is only one root in each rootList.
            */
            std::function<Polynomial(std::vector<std::vector<double>>)> constRoot =
                    [](std::vector<std::vector<double>> rootsGroup) -> Polynomial {
                        return Polynomial::constFunc(rootsGroup.front().front());
                    };
            return rootsEnum.applyFunc(constRoot);
        } else {
            unsigned rootNum = 0;
            for(unsigned i = 0; i < freeValsGroupList.size(); i++) {
                for(unsigned j = 0; j < freeValsGroupList[i].size(); j++) {
                    rootNum++;
                }
            }
            std::vector<Polynomial> solutionList;
            std::function<bool(std::vector<std::vector<double>>)> interRoot =
                    [&](std::vector<std::vector<double>> rootsGroup) -> bool {
                        SPLINTER::DataTable dataTable; ///< Use SPLINTER to generate multivariant interpolant.
                        for(unsigned i = 0; i < rootsGroup.size(); i++) {
                            for(unsigned j = 0; j < rootsGroup[i].size(); j++) {
                                /**
                                 * Note that the order of iteration will not be changed
                                 * as long as the set is not changed or re-hashed.
                                 * @see: https://stackoverflow.com/questions/36242103/is-order-of-iteration-over-the-elements-of-stdunordered-set-guaranteed-to-be-a
                                 */
                                std::vector<double> freeValList;
                                for (auto &freeVarIndex : freeVarSet) {
                                    freeValList.push_back(freeValsGroupList[i][j][freeVarIndex]);
                                }
                                dataTable.addSample(freeValList, rootsGroup[i][j]);
                            }
                        }
                        try{
                            if(dataTable.getNumSamples() <= 4) {
                                return false;
                            }
                            SPLINTER::BSpline bspline = SPLINTER::BSpline::Builder(dataTable).degree(3).build();
//                            auto x = bspline.getControlPoints();
//                            std::cout << x << std::endl;
                            solutionList.push_back(Polynomial([=](DoubleMap valMap) -> double {
                                std::vector<double> valList;
                                for(auto &varIndex : freeVarSet) {
                                    valList.push_back(valMap[varIndex]);
                                }
                                return bspline.eval(valList);
                            }, freeVarSet));
                            return true;
                        } catch(std::exception e) {
                            std::cout << "    Error here." << e.what() << std::endl;
                        }
                    };
            /**
             * Update the interval for each free variable according to the solution domain.
             */
            for(auto &freeVarIndex : freeVarSet) {
                double lb, ub;
                lb = freeValsGroupList[0][0][freeVarIndex];
                ub = lb;
                for(unsigned i = 1; i < freeValsGroupList.size(); i++) {
                    for(unsigned j = 1; j < freeValsGroupList[i].size(); j++) {
                        if (lb > freeValsGroupList[i][j][freeVarIndex]) {
                            lb = freeValsGroupList[i][j][freeVarIndex];
                        } else if (ub < freeValsGroupList[i][j][freeVarIndex]) {
                            ub = freeValsGroupList[i][j][freeVarIndex];
                        }
                    }
                }
                if(lb == ub) {
                    return std::vector<Polynomial>();
                }
                setInterval(freeVarIndex, IntervalPair(lb,ub));
            }
            std::cout << "    Interpolating " << rootsGroupList.size() << " roots." << std::endl;
            rootsEnum.applyFunc(interRoot);
            return solutionList;
        }
    };


    /**
     * Uses the free variables to replace the target variable wherever it occurs in the previous solutionMap.
     * @param solutionMap the solution map where the representation was solved.
     * @param reps the representation of the target variable in terms of the free variables.
     * @param targetVar the index of the target variable.
     * @return the new solutionMap with no target variable appears.
     */
    SolutionMap updateSolution(const SolutionMap &solutionMap, const Polynomial &reps, unsigned targetVar) const
    {
        SolutionMap newSolutionList(solutionMap);
        newSolutionList[targetVar] = reps;
        for(const auto &kvPair : solutionMap) {
            if(kvPair.first != targetVar && kvPair.second.getVarSet().count(targetVar)) {
                Polynomial *newPoly = new Polynomial(reps);
                Polynomial *oldPoly = new Polynomial(kvPair.second);
                newSolutionList[kvPair.first] = Polynomial([=](DoubleMap valMap) -> double {
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
