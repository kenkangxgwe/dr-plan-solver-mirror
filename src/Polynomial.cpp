/*
 *  constraint.cpp
 *
 *  This file implements test polynomials for solving
 *
 */

#include "stdafx.h"
#include "Polynomial.hpp"

Polynomial Polynomial::constFunc(double value)
{
	return Polynomial([value](std::unordered_map<int, double> valMap) -> double {
		return value;
	}, std::unordered_set<int>());
}

Polynomial Polynomial::projectFunc(int index)
{
	return Polynomial([index](std::unordered_map<int, double> valMap) -> double {
		return valMap[index];
	}, { index });
}

Polynomial::Polynomial()
{

}

//Polynomial::Polynomial(double(pExpression) (std::unordered_map<int,double> &), std::vector<int> indexList)
//{
//	expression = pExpression;
//	setVars(indexList);
//}


//Polynomial::Polynomial(std::function<double(std::unordered_map<int,double>)> pExpression, std::vector<int> indexList)
//{
//	expression = pExpression;
//	setVars(indexList);
//}

Polynomial::Polynomial(std::function<double(std::unordered_map<int, double>)> expression, std::unordered_set<int> indexSet)
{
	this->expression = expression;
	setVars(indexSet);
}

Polynomial::~Polynomial()
= default;

double Polynomial::evaluate(std::unordered_map<int, double> valMap)
{
	return expression(valMap);
}

void Polynomial::setVars(std::unordered_set<int> indexSet)
{
	this->indexSet = indexSet;

	for(auto &var : indexSet) {
		indexList.push_back(var);
	}
}

std::vector<int> Polynomial::getVarList()
{
	return indexList;
}

std::unordered_set<int> Polynomial::getVarSet()
{
	return indexSet;
}
