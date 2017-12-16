#include "stdafx.h"
#include "Node.h"

namespace DRPlan
{

Node::Node()
{
}

Node::Node(Polynomial poly, std::vector<int> freeVars, int targetVar)
	:targetPoly(poly), freeVars(freeVars), targetVar(targetVar)
{
}

Node::~Node()
{
	for(auto &subNode : subNodes) {
		delete(subNode);
	}
}

Polynomial Node::curryPoly(const std::unordered_map<int, Polynomial>& solutionList) const
{
	std::unordered_set<int> activeSet({targetVar}); ///< The set that stores the indices of all active variables (free variables and target variable).
	activeSet.insert(freeVars.begin(), freeVars.end());

	std::unordered_set<int> solvedVars; ///< The set that stores the indices of all previously solved variables.
	for(auto &inVar : targetPoly.getVarList()) {
		if(!activeSet.count(inVar)) {
			solvedVars.emplace(inVar);
		}
	}

	return Polynomial([&, solvedVars](std::unordered_map<int, double> valMap) -> double {
		/**
		 * Given the values of active variables, using the solutionList to calculated
		 * the values of solved variables and insert them into the value map.
		 */
		for(auto &solvedVar : solvedVars) {
			valMap[solvedVar] = solutionList.at(solvedVar).evaluate(valMap);
		}
		return targetPoly.evaluate(valMap);
	}, activeSet);
}

}
