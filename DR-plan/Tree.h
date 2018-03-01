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
    typedef std::unordered_map<unsigned, BlackBox<>> Solution;
    typedef std::unordered_map<unsigned, std::pair<double, double>> Domain;

public:
    Tree() {};

    Tree(Node *root, unsigned sampleNum)
            : root(root), sampleNum(sampleNum)
    {
    };

    ~Tree() {};

    bool solveTree()
    {
        solutionList.clear();
        domainList.clear();
        finalSolutionList.clear();
        if (!solveNode(root)) {
            return false;
        }
        for(const auto& solution : solutionList) {
            DoubleMap finalSolution;
            for(const auto &variable : solution) {
                finalSolution[variable.first] = variable.second({});
            }
            finalSolutionList.push_back(finalSolution);
        }
        return true;
    };

    std::vector<DoubleMap> finalSolutionList; /**< A list of maps that stores all the possible solutions.
																	 Each solution maps a variable index to its corresponding value.*/

private:
    Node *root;
    unsigned sampleNum; ///< The number of samples for interpolation.
    std::vector<Domain> domainList; ///< A map that stores the interval of the interpolation samples.
    std::vector<Solution> solutionList; /**< A list of maps that stores all current possible solutions.
																	Each solution map contains the representation for each variable
																	which only involves the free variables .
																	*/

    bool solveNode(Node *curNode)
    {
        /**
         * If current node is a variable node,
         * we simply add current pair of variable and its constant function into every solution map.
         */
        if(curNode->isVarNode) {
            /**
             * There is no solution map.
             */
            if(solutionList.empty()) {
                assert(domainList.empty());
                solutionList.push_back(Solution());
                domainList.push_back(Domain());
            }
            /**
             * The variable is not present in existing solution map.
             */
            if(solutionList[0].count(curNode->targetVar) == 0) {
                assert(domainList[0].count(curNode->targetVar) == 0);
                for(auto & solution : solutionList) {
                    solution[*(curNode->freeVars.begin())] = curNode->targetFunc;
                }
                for(auto & domain : domainList) {
                    domain[*(curNode->freeVars.begin())] = curNode->interval;
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
        std::vector<Domain> newDomainList;
        std::vector<Solution> newSolutionList;
        for(unsigned i = 0; i < solutionList.size(); i++) {
            const auto &solution = solutionList[i];
            const auto &domain = domainList[i];
            BlackBox<> curriedSol = curryFunc(curNode ,solution);
            std::vector<BlackBox<>> interPolys;
            std::vector<Domain> interDomains;
            std::tie(interPolys, interDomains) = interpolate(curriedSol, domain, curNode->targetVar, curNode);
            std::cout << "  Found " << interPolys.size() << " solution(s)." << std::endl;
            if (!interPolys.size()) {
                return false;
            }
            for(const auto &interDomain : interDomains) {
                newDomainList.push_back(updateDomain(domain, interDomain));
            }
            for(const auto &interPoly : interPolys) {
                newSolutionList.push_back(updateSolution(solution, interPoly, curNode->targetVar));
                //double begin = -0.44f;
                //double end = -0.32f;
                //for(unsigned i = 0; i <= sampleNum; i++) {
                //	DoubleMap valMapList;
                //	valMap[0] = begin + (double)i * (end - begin) / (double)sampleNum;
                //	std::cout << "x_0(" << valMap[0] << ") = ";
                //	std::cout << interPoly(valMap) << std::endl;
                //	std::cout << newSolutionList.back().at(curNode->targetVar)(valMap) << std::endl;
                //}
            }
        }
        solutionList = newSolutionList;
        domainList = newDomainList;
        return true;
    };

    BlackBox<> curryFunc(Node *curNode, const Solution& solution) const
    {
        std::unordered_set<unsigned> activeSet({curNode->targetVar}); ///< The set that stores the indices of all active variables (free variables and target variable).
        activeSet.insert(curNode->freeVars.begin(), curNode->freeVars.end());

        std::unordered_set<unsigned> solvedVars; ///< The set that stores the indices of all previously solved variables.
        for(auto &inVar : curNode->targetFunc.getVarList()) {
            if(!activeSet.count(inVar)) {
                solvedVars.emplace(inVar);
            }
        }

        return BlackBox<>([=](DoubleMap valMap) -> double {
            /**
             * Given the values of active variables, using the solution to calculated
             * the values of solved variables and insert them into the value map.
             */
            for(auto &solvedVar : solvedVars) {
                valMap[solvedVar] = solution.at(solvedVar)(valMap);
            }
            return curNode->targetFunc(valMap);
        }, activeSet);
    };

    /**
     * Interpolates the sample points to solve the roots
     * and represent the target variable in terms of free variables.
     * @param blackbox blackbox to solve.
     * @param solvedVar the index of the target variable.
     * @return A list of representations of the target variable.
     */
    std::pair<std::vector<BlackBox<>>, std::vector<Domain>>
    interpolate(const BlackBox<> blackbox, const Domain domain, const unsigned targetVar, const Node *node) const
    {
        std::unordered_set<unsigned> freeVarSet = blackbox.getVarSet();
        freeVarSet.erase(targetVar);
        // The free degree should be no more than 5.
        assert(freeVarSet.size() <= 5);
        /**
         * Compute the sample values of all free variables.
         */
        std::unordered_map<unsigned, std::vector<double>> sampleList;
        for(const auto &freeVar : freeVarSet) {
            // sampleList[freeVar] = std::vector<double>();
            double begin = domain.at(freeVar).first;
            double end = domain.at(freeVar).second;
            std::cout<< "    Sampling x_" << freeVar << " from " << begin << " to " << end << "." << std::endl;
            for(unsigned i = 0; i <= sampleNum; i++) {
                sampleList[freeVar].push_back(begin + (double)i * (end - begin) / (double)sampleNum);
            }
        }
        /**
         * For each combination of the free variable values,
         * interpolate the sample values of target variable against the values of the blackbox,
         * and find the root of the target variable.
         */
		std::vector<DoubleMap> freeValsList[2];
		std::vector<double> rootList[2];
        /**
         * If there is no samples for free variable, either there is no free variables,
         * or there is no interval available for all free variables,
         * interpolate and find root with empty map.
         * Otherwise, interpolate the sample values.
         */
//        if(freeVarSet.empty()) {
//            Enumeration<unsigned, double> enumFree(sampleList, freeVarSet);
//            findRoot({});
//        } else {
        double targetBegin = domain.at(targetVar).first;
        double targetEnd = domain.at(targetVar).second;
        Enumeration<unsigned, double> enumFree(sampleList, freeVarSet);
        for(auto enumer = enumFree.begin(); enumer != enumFree.end(); ++enumer) {
            const auto &freeVals = enumFree.at(enumer);
			SPLINTER::DataTable dataTable[2];
            for(unsigned i = 0; i <= sampleNum; i++) {
                DoubleMap valMap(freeVals);
                valMap[targetVar] = targetBegin + (double)i * (targetEnd - targetBegin) / (double)sampleNum;
                try {
                    double result = blackbox(valMap);
                    if(std::isnan(result)){
                        continue;
                    }
					if(node->getDropFlip()) {
						dataTable[0].addSample(valMap[targetVar], result);
					} else {
						dataTable[1].addSample(valMap[targetVar], result);
					}
//                    node->exportGraphviz("s" + std::to_string(i));
                } catch(...) {
                    continue;
                }
            }
            for(int i = 0; i < 2; i++) {
                if(dataTable[i].getNumSamples() < 4) {
//                    std::cout << "    Not enough sample points." << std::endl;
                    continue;
                }
                SPLINTER::BSpline slicedInter = SPLINTER::BSpline::Builder(dataTable[i]).degree(3).build();
                std::vector<double> roots = RootFinder::findZeros(slicedInter, 3);
                if(!roots.size()) {
                    // std::cout << "    Found no roots" << std::endl;
                    continue;
                }
                if(roots.size() > 2) {
                    std::cout << "    Found " << roots.size() << " roots. Using the smaller one." << std::endl;
                }
                freeValsList[i].push_back(freeVals);
                rootList[i].push_back(roots.front());
            }
            /**
             * If there is only one root, it will be in its own group.
             */
            /**
             * TODO If two roots are close enough, we will try other combinations.
             */
        }

        std::vector<Domain> newDomainList;
        std::vector<BlackBox<>> newSolutionList;
        for(int i = 0; i < 2; i++) {
            /**
             * Returns empty root list if there is no root everywhere.
             * Otherwise enumerate all possible combinations of the roots.
             */
            if(rootList[i].empty()) {
                continue;
            }
            if(freeVarSet.empty()) {
                /**
                * Since there is no sample values for free variables,
                * there is only one root in each rootList.
                */
                newSolutionList.push_back(BlackBox<>::constFunc(rootList[i].front()));
                newDomainList.push_back(Domain());
                continue;
            }
            Domain newDomain;
            for(auto &freeVarIndex : freeVarSet) {
                double lb, ub;
                lb = freeValsList[i][0][freeVarIndex];
                ub = lb;
                for(unsigned j = 1; j < freeValsList[i].size(); j++) {
                    if(lb > freeValsList[i][j][freeVarIndex]) {
                        lb = freeValsList[i][j][freeVarIndex];
                    } else if(ub < freeValsList[i][j][freeVarIndex]) {
                        ub = freeValsList[i][j][freeVarIndex];
                    }
                }
                /**
                 * What happens if lb == ub?
                 */
                // if(lb == ub) {
                // 	return std::make_pair(std::vector<BlackBox<>>(), std::vector<Domain>());
                // }
                newDomain.emplace(freeVarIndex, std::make_pair(lb, ub));
            }
            SPLINTER::DataTable dataTable; ///< Use SPLINTER to generate multivariant interpolant.
            for(unsigned j = 0; j < rootList[i].size(); j++) {
                /**
                 * Note that the order of iteration will not be changed
                 * as long as the set is not changed or re-hashed.
                 * @see: https://stackoverflow.com/questions/36242103/is-order-of-iteration-over-the-elements-of-stdunordered-set-guaranteed-to-be-a
                 */
                std::vector<double> freeVals;
                for(auto &freeVarIndex : freeVarSet) {
                    freeVals.push_back(freeValsList[i][j][freeVarIndex]);
                }
                dataTable.addSample(freeVals, rootList[i][j]);
            }
            if(dataTable.getNumSamples() <= 4) {
                std::cout << "    Not enough roots to interpolating." << std::endl;
                continue;
            }
            std::cout << "    Interpolating " << dataTable.getNumSamples() << "roots." << std::endl;
            SPLINTER::BSpline *bspline = new SPLINTER::BSpline(SPLINTER::BSpline::Builder(dataTable).degree(3).build());
            // auto x = bspline.getControlPoints();
            // std::cout << x << std::endl;
            newDomainList.push_back(newDomain);
            newSolutionList.push_back(BlackBox<>([=](DoubleMap valMap) -> double {
                std::vector<double> valList;
                for(auto &varIndex : freeVarSet) {
                    valList.push_back(valMap[varIndex]);
                }
                return bspline->eval(valList);
            }, freeVarSet));
        }
        return std::make_pair(newSolutionList, newDomainList);
    };


    /**
     * Uses the free variables to replace the target variable wherever it occurs in the previous solution.
     * @param solution the solution map where the representation was solved.
     * @param reps the representation of the target variable in terms of the free variables.
     * @param targetVar the index of the target variable.
     * @return the new solution with no target variable appears.
     */
    Solution updateSolution(const Solution &solution, const BlackBox<> &reps, unsigned targetVar) const
    {
        Solution newSolutionList(solution);
        newSolutionList[targetVar] = reps;
        for(const auto &kvPair : solution) {
            if(kvPair.first != targetVar && kvPair.second.getVarSet().count(targetVar)) {
                BlackBox<> *newPoly = new BlackBox<>(reps);
                BlackBox<> *oldPoly = new BlackBox<>(kvPair.second);
                newSolutionList[kvPair.first] = BlackBox<>([=](DoubleMap valMap) -> double {
                    double test = newPoly->evaluate(valMap);
                    valMap[targetVar] = newPoly->evaluate(valMap);
                    return oldPoly->evaluate(valMap);
                }, newPoly->getVarSet());
            }
        }
        return newSolutionList;
    };

    Domain updateDomain(const Domain &domain, const Domain &interDomain) const {
        Domain newDomain(domain);
        for (const auto intervalMap : interDomain) {
            newDomain[intervalMap.first] = intervalMap.second;
        }
        return newDomain;
    }
};

}
