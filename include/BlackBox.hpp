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

#pragma once

#include "stdafx.h"

/**
 * Black Box Class
 */
template <typename K = unsigned, typename V = double, typename R = double>
class BlackBox
{
public:
	/**
	* Constructs a constant function of specified value which receives any size of variables.
	* @param value the constant value.
	* @return the constant function.
	*/
	static BlackBox constFunc(R value)
	{
		return BlackBox<K,V,R>([value](std::unordered_map<K, V> valMap) -> R { return value; }, std::unordered_set<K>());
	};

	/**
	* Constructs a project function which returns the specified index of the variable list.
	* @param index the variable list index.
	* @return the value at the index.
	*/
	static BlackBox projectFunc(K index)
	{
		return BlackBox<K,V>([index](std::unordered_map<K, V> valMap) -> V { return valMap[index]; }, {index});
	};

//	const static std::unordered_map<K, V> emptyValMap;

	BlackBox() {};
	BlackBox(std::function<R(std::unordered_map<K, V>)> expression, std::unordered_set<K> indexSet)
	{
		this->expression = expression;
		setVars(indexSet);
	};
	~BlackBox() {};

	R evaluate(std::unordered_map<K, V> valMap) const { return expression(valMap); };
	R operator() (std::unordered_map<K, V> valMap) const { return expression(valMap); };
	std::vector<K> getVarList() const { return indexList; };
	std::unordered_set<K> getVarSet() const { return indexSet; };

private:
	std::vector<K> indexList;
	std::unordered_set<K> indexSet;
	std::function<R(std::unordered_map<K, V>)> expression;
	void setVars(std::unordered_set<K> indexSet)
	{
		this->indexSet = indexSet;
		for(auto &var : indexSet) {
			indexList.push_back(var);
		}
	};
};

using Polynomial = BlackBox<>;
