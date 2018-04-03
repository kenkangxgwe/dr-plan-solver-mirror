#include "Tree.h"

namespace DRPLAN
{

Tree::Tree() {}

Tree::Tree(DRPLAN::Node *root, unsigned sampleNum)
        : root(root), sampleNum(sampleNum)
{
}

Tree::~Tree() {}

bool Tree::solveTree()
{
    solutionList.clear();
    domainList.clear();
    finalSolutionList.clear();
    if(!solveNode(root)) {
        return false;
    }
    for(const auto &solution : solutionList) {
        DoubleMap finalSolution;
        for(const auto &variable : solution) {
            finalSolution[variable.first] = variable.second({});
        }
        finalSolutionList.push_back(finalSolution);
    }
    return true;
}

bool Tree::solveNode(DRPLAN::Node *curNode)
{
    /**
     * If current node is a variable node,
     * we simply add current pair of variable and its constant function into every solution map.
     */
    if(curNode->isCayleyNode) {
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
        if(solutionList[0].count(curNode->targetCayley) == 0) {
            assert(domainList[0].count(curNode->targetCayley) == 0);
            for(auto &solution : solutionList) {
                solution[curNode->targetCayley] = BlackBox<>::projectFunc(curNode->targetCayley);
            }
            for(auto &domain : domainList) {
                domain[curNode->targetCayley] = curNode->interval;
            }
        }
        return true;
    }
    /**
     * Solve every sub node.
     */
    for(auto &subNode : curNode->subNodes) {
        if(!solveNode(subNode)) {
            return false;
        }
    }
    /**
     * Solve the current node based on every possible solution.
     */
    std::cout << "  Solving x_" << curNode->targetCayley << "(";
    for(auto iter = curNode->freeCayley.begin(); iter != curNode->freeCayley.end(); ++iter) {
        if(iter != curNode->freeCayley.begin()) {
            std::cout << ", ";
        }
        std::cout << "x_" << (*iter);
    }
    std::cout << "):" << std::endl;
    std::vector<Domain> newDomainList;
    std::vector<Solution> newSolutionList;
    for(unsigned i = 0; i < solutionList.size(); i++) {
        const auto &solution = solutionList[i];
        const auto &domain = domainList[i];
        MapTransform transformedMap = transformMap(curNode, solution);
        std::vector<BlackBox<>> repList;
        std::vector<Domain> repDomains;
        std::tie(repList, repDomains) = solveTarget(curNode, transformedMap, domain);
        std::cout << "  Found " << repList.size() << " solution(s)." << std::endl;
        if(!repList.size()) {
            return false;
        }
        for(const auto &interDomain : repDomains) {
            newDomainList.push_back(updateDomain(domain, interDomain));
        }
        for(const auto &interPoly : repList) {
            newSolutionList.push_back(updateSolution(solution, interPoly, curNode->targetCayley));
            //double begin = -0.44f;
            //double end = -0.32f;
            //for(unsigned i = 0; i <= sampleNum; i++) {
            //	DoubleMap valMapList;
            //	valMap[0] = begin + (double)i * (end - begin) / (double)sampleNum;
            //	std::cout << "x_0(" << valMap[0] << ") = ";
            //	std::cout << interPoly(valMap) << std::endl;
            //	std::cout << newSolutionList.back().at(curNode->targetCayley)(valMap) << std::endl;
            //}
        }
    }
    solutionList = newSolutionList;
    domainList = newDomainList;
    return true;
}

Tree::MapTransform Tree::transformMap(DRPLAN::Node *node, const Tree::Solution &solution) const
{
    std::unordered_set<unsigned> activeSet(node->targetCayley); ///< The set that stores the indices of all active variables (free variables and target variable).
    activeSet.insert(node->freeCayley.begin(), node->freeCayley.end());

    std::unordered_set<unsigned> solvedVars; ///< The set that stores the indices of all previously solved variables.
    for(auto &var : node->allCayley) {
        if(!activeSet.count(var)) {
            solvedVars.emplace(var);
        }
    }

    return MapTransform([=](DoubleMap valMap) -> DoubleMap {
        /**
         * Given the values of active variables, using the solution to calculated
         * the values of solved variables and insert them into the value map.
         */
        for(auto &solvedVar : solvedVars) {
            valMap[solvedVar] = solution.at(solvedVar)(valMap);
        }
        return valMap;
    }, activeSet);
}

std::pair<std::vector<BlackBox<>>, std::vector<Tree::Domain>>
Tree::solveTarget(Node *node, const MapTransform varMap, const Domain domain) const
{
    std::unordered_set<unsigned> freeVarSet = varMap.getVarSet();
    const unsigned targetCayley = node->targetCayley;
    freeVarSet.erase(targetCayley);
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
        std::cout << "    Sampling x_" << freeVar << " from " << begin << " to " << end << "." << std::endl;
        for(unsigned i = 0; i <= sampleNum; i++) {
            sampleList[freeVar].push_back(begin + (double) i * (end - begin) / (double) sampleNum);
        }
    }
    /**
     * For each combination of the free variable values,
     * interpolate the sample values of target variable against the values of the blackbox,
     * and find the root of the target variable.
     */
    std::vector<DoubleMap> freeValsList[4];
    std::vector<double> rootList[4];
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
    const double targetBegin = domain.at(targetCayley).first;
    const double targetEnd = domain.at(targetCayley).second;
    double dstp = (targetEnd - targetBegin) / 40;
    double tol = (targetEnd - targetBegin) / 80;
    double maxstp = (targetEnd - targetBegin) / sampleNum;
    Enumeration<unsigned, double> enumFree(sampleList, freeVarSet);
    for(auto enumer = enumFree.begin(); enumer != enumFree.end(); ++enumer) {
        DoubleMap activeVals = enumFree.at(enumer); // std::move() needed?
        SPLINTER::DataTable dataTable[4];
        /**
         * Binary Search
        activeVals[targetCayley]= targetBegin;
        node->realize(varMap(activeVals));
        double srcFlipBegin, tarFlipBegin;
        std::tie(srcFlipBegin, tarFlipBegin) = node->dropFlip();
        activeVals[targetCayley]= targetEnd;
        node->realize(varMap(activeVals));
        double srcFlipEnd, tarFlipEnd;
        std::tie(srcFlipEnd, tarFlipEnd) = node->dropFlip();
        if(beginFlip ^ endFlip) {

        }
         */

        double stp =  maxstp;
        double lastSample = 0.0f;
        double lastResult = 0.0f;
        bool initial = true;
        bool reset = true;
        double lastSrcFlip = 0, lastTarFlip = 0;
        int i = 1;
        for(double curSample = targetBegin; curSample <= targetEnd; curSample += stp) {
            activeVals[targetCayley] = curSample;
            try {
                node->realize(varMap(activeVals));
                double curResult = node->dropDiff();
                if(std::isnan(curResult)) {
                    continue;
                }
                double srcFlip, tarFlip;
                std::tie(srcFlip, tarFlip) = node->dropFlip();
//                node->exportGraphviz("s" + std::to_string(i++));
                if (initial) {
                    initial = false;
                } else if ((srcFlip > 0.f) == (lastSrcFlip > 0.f)
                           && (tarFlip > 0.f) == (lastTarFlip > 0.f)) {
                    if(stp < 1E-3) {
                        stp = maxstp;
                        curSample = lastSample;
                        initial = true;
                    }
                    if (abs(curResult - lastResult) < (dstp - tol)) {
                        if(reset) {
                            stp = maxstp;
                            curSample = lastSample + stp;
                        } else {
                            stp /= 2.0;
                            continue;
                        }
                    }
                    else if (abs(curResult - lastResult) > (dstp + tol)) {
                        reset = false;
                        stp /= 2.0;
                        curSample = lastSample;
                        continue;
                    }
                    else {
                        reset = true;
                    }
                }
                if(srcFlip > 0.f) {
                    if(tarFlip > 0.f) {
                        dataTable[0].addSample(activeVals[targetCayley], curResult);
                    } else {
                        dataTable[1].addSample(activeVals[targetCayley], curResult);
                    }
                } else {
                    if (tarFlip > 0.f){
                        dataTable[2].addSample(activeVals[targetCayley], curResult);
                    } else {
                        dataTable[3].addSample(activeVals[targetCayley], curResult);
                    }
                }
                lastSample = curSample;
                lastResult = curResult;
                lastSrcFlip = srcFlip;
                lastTarFlip = tarFlip;
            } catch(char const* msg) {
                std::cout << msg;
                continue;
            }
        }
        for(int i = 0; i < 4; i++) {
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
            freeValsList[i].push_back(activeVals);
            if(roots.size() > 2) {
                std::cout << "    Found " << roots.size() << " roots. Using the first valid one this time." << std::endl;
                unsigned j = 0;
                for(auto const &root : roots) {
                    try{
                        activeVals[targetCayley] = root;
                        node->realize(varMap(activeVals));
                        std::cout << "Source Flip: " << node->dropFlip().first << "Target Flip: " << node->dropFlip().second << std::endl;
                    } catch(const char* msg) {
                        std::cout << msg << std::endl;
                        j++;
                        continue;
                    }
                }
                if(j < roots.size()) {
                    rootList[i].push_back(roots[j]);
                }
            } else {
                rootList[i].push_back(roots.front());
            }
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
    for(int i = 0; i < 4; i++) {
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
        auto newSolution = BlackBox<>([=](DoubleMap valMap) -> double {
            std::vector<double> valList;
            for(auto &varIndex : freeVarSet) {
                valList.push_back(valMap[varIndex]);
            }
            return bspline->eval(valList);
        }, freeVarSet);
        /*
         * Test
         */
        std::unordered_map<unsigned, std::vector<double>> testSamples;
        for(const auto &freeVar : freeVarSet) {
            double begin = newDomain.at(freeVar).first;
            double end = newDomain.at(freeVar).second;
            std::cout << "    Sampling x_" << freeVar << " from " << begin << " to " << end << "." << std::endl;
            for(unsigned i = 0; i <= sampleNum; i++) {
                testSamples[freeVar].push_back(begin + (double) i * (end - begin) / (double) sampleNum);
            }
        }
        Enumeration<unsigned, double> enumTest(testSamples, freeVarSet);
        for(auto enumer = enumTest.begin(); enumer != enumTest.end(); ++enumer) {
            DoubleMap activeVals = enumTest.at(enumer); // std::move() needed?
            double result = newSolution(activeVals);
            if (result < targetBegin || result > targetEnd) {
                // auto x =
                // std::cout << x << std::endl;
                auto x = dataTable.getTableX()[0];
                auto y =dataTable.getVectorY();
                std::cout << bspline->getControlPoints() << std::endl;
            }

        }
        newDomainList.push_back(newDomain);
        newSolutionList.push_back(newSolution);
    }
    return std::make_pair(newSolutionList, newDomainList);
}

Tree::Solution Tree::updateSolution(const Tree::Solution &solution, const BlackBox<> &reps, unsigned targetVar) const
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
}

Tree::Domain Tree::updateDomain(const Tree::Domain &domain, const Tree::Domain &interDomain) const
{
    Domain newDomain(domain);
    for(const auto intervalMap : interDomain) {
        newDomain[intervalMap.first] = intervalMap.second;
    }
    return newDomain;
}

}
