#pragma once

#include"stdafx.h"

/**
 * Polynomial Class
 */
class Polynomial
{
public:
	/**
	* Constructs a constant function of specified value which receives any size of variables.
	* @param value the constant value.
	* @return the constant function.
	*/
	static Polynomial constFunc(double);

	/**
	* Constructs a project function which returns the specified index of the variable list.
	* @param index the variable list index.
	* @return the value at the index.
	*/
	static Polynomial projectFunc(int index);

	Polynomial();
	// Polynomial(double(pExpression)(std::unordered_map<int, double>&), std::vector<int> indexList);
	Polynomial(std::function<double(std::unordered_map<int, double>)>, std::unordered_set<int>);
	~Polynomial();

	double evaluate(std::unordered_map<int, double>);
	std::vector<int> getVarList();
	std::unordered_set<int> getVarSet();

private:
	std::vector<int> indexList;
	std::unordered_set<int> indexSet;
	std::function<double(std::unordered_map<int, double>)> expression;
	void setVars(std::unordered_set<int>);
};