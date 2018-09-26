/**
 * This file is part of DRPLAN.
 *
 * DRPLAN is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DRPLAN is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "Tree.h"

#include "RootFinder.h"
#include "Enumeration.hpp"
#include "TwoTree.h"

namespace DRPLAN
{

Tree::Tree() {}

Tree::Tree(DRPLAN::Node *root, unsigned sampleNum, unsigned maxOffset)
    : root(root), sampleNum(sampleNum), max_offset(maxOffset)
{
}

Tree::~Tree() {}

bool Tree::solveTree()
{
    nodeSolutionList.clear();
    finalSolutionList.clear();
    if(!solveNode(root)) {
        return false;
    }
    for(const auto &nodeSolution : nodeSolutionList) {
        DoubleMap finalSolution;
        for(const auto &variable : nodeSolution.solution) {
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
        if(nodeSolutionList.empty()) {
            nodeSolutionList.push_back({{}, {}, {}, 0});
        }
        /**
         * The variable is not present in existing solution map.
         */
        if(nodeSolutionList[0].solution.count(curNode->targetCayley) == 0) {
            assert(nodeSolutionList[0].domain.count(curNode->targetCayley) == 0);
            for(auto &nodeSolution : nodeSolutionList) {
                nodeSolution.solution[curNode->targetCayley] = std::move(BlackBox<>::projectFunc(curNode->targetCayley));
                nodeSolution.domain[curNode->targetCayley] = curNode->interval;
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
    std::cout << "Solving " << curNode->toStringFull() << ":" << std::endl;
    std::vector<NodeSolution> newNodeSolutionList;
    for(auto const &nodeSolution: nodeSolutionList) {
        //for(unsigned i = 0; i < nodeSolutionList.size(); i++) {
        //    const auto &solution = solutionList[i];
        //    const auto &domain = domainList[i];
        MapTransform transformedMap = transformMap(curNode, nodeSolution.solution);
        auto curNodeSolutionList = std::move(solveTarget(curNode, transformedMap, nodeSolution));
        std::cout << "  Found " << curNodeSolutionList.size() << " representation(s)." << std::endl;
        newNodeSolutionList.insert(newNodeSolutionList.end(), curNodeSolutionList.begin(), curNodeSolutionList.end());
    }
    if(newNodeSolutionList.empty()) {
        return false;
    }

    nodeSolutionList = newNodeSolutionList;
    std::cout << "Found " << newNodeSolutionList.size() << " solution(s) for "
              << curNode->toStringFull() << "." << std::endl;
    return true;
}

MapTransform Tree::transformMap(DRPLAN::Node *node, const Solution &solution) const
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

std::vector<std::vector<std::pair<DoubleMap, double>>>
findRoots(Node *node, SPLINTER::DataTable const &dataTable, MapTransform const &varMap,
          DoubleMap &activeVals, std::vector<double> const &tolList = {0.0})
{
    std::vector<std::vector<std::pair<DoubleMap, double>>> result{
        tolList.size(),
        std::vector<std::pair<DoubleMap, double>>{}
    };
    // not enough sample points
    if(dataTable.getNumSamples() < 4) {
        //std::cout << "    Not enough sample points." << std::endl;
        return result;
    }

    try {
        SPLINTER::BSpline slicedInter = SPLINTER::BSpline::Builder(dataTable).degree(3).build();
        for(int i = 0; i < tolList.size(); i++) {
            std::vector<double> roots = RootFinder::findZeros(slicedInter, 3, node->targetLength * tolList[i]);

            if(roots.empty()) {
                continue;
            }

            if(
                //freeVarSet.empty()
                false
                ) {
                auto res = result[i];
                for(const auto &root : roots) {
                    result[i].push_back(std::make_pair(DoubleMap(), root));
                }
            } else if(roots.size() >= 2) {
                std::cout << "    Found " << roots.size() << " roots. Using the nearest one this time."
                          << std::endl;
                unsigned minIdx = 0;
                double minDiff = DBL_MAX;
                for(unsigned rootIdx = 0; rootIdx < roots.size(); rootIdx++) {
                    try {
                        activeVals[node->targetCayley] = roots[rootIdx];
                        double cur_diff = abs(node->realize(varMap(activeVals))->dropDiff());
                        //node->exportGraphviz(std::to_string(rootIdx));
                        if(minDiff >= cur_diff) {
                            minDiff = cur_diff;
                            minIdx = rootIdx;
                        }
                        //std::cout << "      Source Flip: " << node->dropFlip().first << "Target Flip: " << node->dropFlip().second << std::endl;
                    } catch(const char *msg) {
                        std::cout << msg << std::endl;
                        continue;
                    }
                }

                if(minIdx < roots.size()) {
                    if(minDiff > node->targetLength / 5) {
                        std::cout << "    The minimum diff: " << minDiff << " is not near enough" << std::endl;
                    } else {
                        result[i].push_back(std::make_pair(activeVals, roots[minIdx]));
                    }
                } else {
                    continue;
                }
            } else {
                result[i].push_back(std::make_pair(activeVals, roots.front()));
            }

        }
    } catch(const char *msg) {
        std::cout << msg << std::endl;
    } catch(...) {
        std::cout << "Error in SPLINTER occurs." << std::endl;
    }
    return result;
}

/**
 * In solution, * uses the free variables to replace the target variable
 * wherever it occurs in the previous solution.
 * In domain, * uses the new domain to replace the previous ones.
 * In dropped flip, append one dropped flip.
 * In offset, add 1 if it use the tolerance value during the sampling.
 * @return the new node solution with no target variable appears.
 */
NodeSolution *updateNodeSolution(NodeSolution &newNodeSolution, NodeSolution const &oldNodeSolution)
{
    auto &[new_solution, new_domain, new_drop_flip, new_offset] = newNodeSolution;
    auto const &[old_solution, old_domain, old_drop_flip, old_offset] = oldNodeSolution;

    // only one target Cayley is solved.
    assert(new_solution.size() == 1);

    // generate new solutions
    unsigned target_var = (*new_solution.begin()).first;
    auto const &target_rep = (*new_solution.begin()).second;
    for(const auto &[other_var, other_rep] : old_solution) {
        if(other_var != target_var && other_rep.getVarSet().count(target_var)) {
            auto *newPoly = new BlackBox<>(target_rep);
            auto *oldPoly = new BlackBox<>(other_rep);
            new_solution[other_var] = BlackBox<>([=](DoubleMap valMap) -> double {
                double test = newPoly->evaluate(valMap);
                valMap[target_var] = newPoly->evaluate(valMap);
                return oldPoly->evaluate(valMap);
            }, newPoly->getVarSet());
        }
    }
    new_solution.insert(old_solution.begin(), old_solution.end());
    /**
     * For new domain,
     * each element is inserted only if its key is not equivalent to
     * the key of any other element already in the container.
     */
    new_domain.insert(old_domain.begin(), old_domain.end());
    new_drop_flip.insert(new_drop_flip.begin(), old_drop_flip.begin(), old_drop_flip.end());
    new_offset += old_offset;
    return &newNodeSolution;
}

std::vector<NodeSolution>
Tree::solveTarget(Node *node, const MapTransform &varMap, NodeSolution const &nodeSolution) const
{
    auto const &[solution, domain, drop_flip, offset] = nodeSolution;
    std::unordered_set<unsigned> freeVarSet = varMap.getVarSet();
    unsigned const targetCayley = node->targetCayley;
    freeVarSet.erase(targetCayley);
    // The degree of freedom (flex number) should be no more than 5.
    assert(freeVarSet.size() <= 5);

    /**
     * Compute the sample values of all free variables.
     */
    std::unordered_map<unsigned, std::vector<double>> sampleList;
    for(const auto &freeVar : freeVarSet) {
        // sampleList[freeVar] = std::vector<double>();
        double begin = domain.at(freeVar).first;
        //double end = begin + 3.0f;
        double end = domain.at(freeVar).second;
        std::cout << "  Sampling x_" << freeVar << " from " << begin << " to " << end << "." << std::endl;
        for(unsigned i = 0; i <= sampleNum; i++) {
            sampleList[freeVar].push_back(begin + (double) i * (end - begin) / (double) sampleNum);
        }
    }

    /**
     * For each combination of the free variable values,
     * interpolate the sample values of target variable against the values of the blackbox,
     * and find the root of the target variable.
     */
    std::vector<double> tol_list;
    if(nodeSolution.offset >= max_offset) {
        tol_list = {0.00};
    } else {
        tol_list = {0.00, 0.05, -0.05};
    }
    std::vector<DoubleMap> freeValsList[4 * tol_list.size()];
    std::vector<double> rootList[4 * tol_list.size()];
    Enumeration<unsigned, double> enumFree(sampleList, freeVarSet);
    int suffix0 = 0;
    for(auto enumer = enumFree.begin(); enumer != enumFree.end(); ++enumer) {
        DoubleMap activeVals(std::move(enumFree.at(enumer)));
        SPLINTER::DataTable dataTable[4];
        /**
         * Binary Search (Not implemented. Needed?)
         */
        double last_sample = 0.0f;
        double last_result = 0.0f;
        bool initial = true;
        bool sample_end = false;
        bool smaller_stp = false;
        double last_src_flip = 0, last_tar_flip = 0;
        int suffix = 1; // suffix of export graph filename.
        suffix0++;
        double lb, ub;
        std::tie(lb, ub) = node->refineInterval(varMap(activeVals));
        double target_begin = domain.at(targetCayley).first;
        double target_end = domain.at(targetCayley).second;
        if(target_begin < lb) {
            target_begin = lb;
        }
        if(target_end > ub) {
            target_end = ub;
        }
        double value_stp = (target_end - target_begin) / sampleNum / 2;
        double tol = (target_end - target_begin) / sampleNum / 5;
        double maxstp = (target_end - target_begin) / sampleNum;
        double stp = maxstp;

        for(double cur_sample = target_begin; !sample_end; cur_sample += stp) {
            if(cur_sample > target_end) {
                cur_sample = target_end;
                sample_end = true;
            }
            activeVals[targetCayley] = cur_sample;
            try {
                double cur_result = node->realize(varMap(activeVals))->dropDiff();
                if(std::isnan(cur_result)) {
                    continue;
                }
                double src_flip, tar_flip;
                std::tie(src_flip, tar_flip) = node->dropFlip();
                if(
                    //freeVarSet.empty()
                    //freeVarSet.size() == 2
                    //true
                    false
                    ) {
                    node->exportGraphviz("f" + std::to_string(suffix0) + "c" + std::to_string(suffix++));
                }
                // no last sample
                if(initial) {
                    initial = false;
                } else if((src_flip > 0.f) == (last_src_flip > 0.f)
                          && (tar_flip > 0.f) == (last_tar_flip > 0.f)) {
                    //step is too small to change
                    if(stp < 1E-3) {
                        stp = maxstp;
                        cur_sample = last_sample;
                        initial = true;
                    }
                    // when the sample is near a zero point
                    if((target_end - target_begin) > node->targetLength * 0.1
                       && abs(cur_result) < node->targetLength * 0.1) {
                        // the value changes too small
                        if(abs(cur_result - last_result) < (value_stp - tol)) {
                            if(smaller_stp) {
                                stp /= 2.0;
                                continue;
                            } else {
                                stp = maxstp;
                                cur_sample = last_sample + stp;
                            }
                        }
                            // the value changes too large
                        else if(abs(cur_result - last_result) > (value_stp + tol)) {
                            smaller_stp = true;
                            stp /= 2.0;
                            cur_sample = last_sample;
                            continue;
                        }
                    }
                    smaller_stp = false;
                }

                if(src_flip > 0.f) {
                    if(tar_flip > 0.f) {
                        dataTable[0].addSample(activeVals[targetCayley], cur_result);
                    } else {
                        dataTable[1].addSample(activeVals[targetCayley], cur_result);
                    }
                } else {
                    if(tar_flip > 0.f) {
                        dataTable[2].addSample(activeVals[targetCayley], cur_result);
                    } else {
                        dataTable[3].addSample(activeVals[targetCayley], cur_result);
                    }
                }
                last_sample = cur_sample;
                last_result = cur_result;
                last_src_flip = src_flip;
                last_tar_flip = tar_flip;
            } catch(char const *msg) {
                std::cout << msg;
                continue;
            }
        }

        /**
         * find roots with every tolenrant value in tolList,
         * and construct 4 * tolList.size() rootsLists
         */
        for(int df_i = 0; df_i < 4; df_i++) {
            auto rootpairs = findRoots(node, dataTable[df_i], varMap, activeVals, tol_list);
            for(int tol_i = 0; tol_i < tol_list.size(); tol_i++) {
                if(!rootpairs[tol_i].empty()) {
                    freeValsList[df_i * tol_list.size() + tol_i].push_back(rootpairs[tol_i].front().first);
                    rootList[df_i * tol_list.size() + tol_i].push_back(rootpairs[tol_i].front().second);
                }
            }
        }
    }

    std::vector<NodeSolution> resultList;
    //std::vector<Domain> newDomainList;
    //std::vector<BlackBox<>> newSolutionList;
    for(int df_i = 0; df_i < 4; df_i++) {
        for(size_t tol_i = 0; tol_i < tol_list.size(); tol_i++) {
            size_t rt_i = df_i * tol_list.size() + tol_i;
            /**
             * Returns empty root list if there is no root everywhere.
             * Otherwise enumerate all possible combinations of the roots.
             */
            if(rootList[rt_i].empty()) {
                continue;
            }
            if(freeVarSet.empty()) {
                /**
                * Since there is no sample values for free variables,
                * there is only one root in each rootList.
                */
                for(const auto &root : rootList[rt_i]) {
                    resultList.push_back({
                                             {{targetCayley, BlackBox<>::constFunc(root)}},
                                             Domain(),
                                             {df_i},
                                             (unsigned) (tol_i > 0 ? 0 : 1)
                                         });
                }
                continue;
            }
            Domain newDomain;
            // calculate new domain
            for(auto &freeVarIndex : freeVarSet) {
                double lb, ub;
                lb = freeValsList[rt_i][0][freeVarIndex];
                ub = lb;
                for(unsigned rt_j = 1; rt_j < freeValsList[rt_i].size(); rt_j++) {
                    if(lb > freeValsList[rt_i][rt_j][freeVarIndex]) {
                        lb = freeValsList[rt_i][rt_j][freeVarIndex];
                    } else if(ub < freeValsList[rt_i][rt_j][freeVarIndex]) {
                        ub = freeValsList[rt_i][rt_j][freeVarIndex];
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
            for(unsigned rt_j = 0; rt_j < rootList[rt_i].size(); rt_j++) {
                /**
                 * Note that the order of iteration will not be changed
                 * as long as the set is not changed or re-hashed.
                 * @see: https://stackoverflow.com/questions/36242103/is-order-of-iteration-over-the-elements-of-stdunordered-set-guaranteed-to-be-a
                 */
                std::vector<double> freeVals;
                for(auto &freeVarIndex : freeVarSet) {
                    freeVals.push_back(freeValsList[rt_i][rt_j][freeVarIndex]);
                }
                dataTable.addSample(freeVals, rootList[rt_i][rt_j]);
            }
            if(dataTable.getNumSamples() <= 4) {
                std::cout << "    Not enough roots to interpolating. Only " << dataTable.getNumSamples()
                          << " was found."
                          << std::endl;
                continue;
            }
            std::cout << "    Interpolating " << dataTable.getNumSamples() << " roots." << std::endl;
            SPLINTER::BSpline *bspline = new SPLINTER::BSpline(SPLINTER::BSpline::Builder(dataTable).degree(3).build());
            // auto x = bspline.getControlPoints();
            // std::cout << x << std::endl;
            auto newSolution{std::move(
                BlackBox<>([=](DoubleMap valMap) -> double {
                    std::vector<double> valList;
                    for(auto &varIndex : freeVarSet) {
                        valList.push_back(valMap[varIndex]);
                    }
                    return bspline->eval(valList);
                }, freeVarSet)
            )};
            /*
             * Test
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
                    auto y = dataTable.getVectorY();
                    std::cout << bspline->getControlPoints() << std::endl;
                }

            }
             */
            resultList.push_back({
                                     {{targetCayley, std::move(newSolution)}},
                                     std::move(newDomain),
                                     {df_i},
                                     (unsigned) (tol_i > 0 ? 1 : 0)
                                 });
        }
    }
    for(auto &result : resultList) {
        updateNodeSolution(result, nodeSolution);
    }
    return resultList;
}

}
