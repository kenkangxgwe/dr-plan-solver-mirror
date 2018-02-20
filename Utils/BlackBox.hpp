#pragma once

#include <unordered_map>
#include <functional>
#include <unordered_set>
#include <vector>

/**
 * Black Box Class
 */
template <typename K = unsigned, typename V = double>
class BlackBox
{
public:
	/**
	* Constructs a constant function of specified value which receives any size of variables.
	* @param value the constant value.
	* @return the constant function.
	*/
	static BlackBox constFunc(V value)
	{
		return BlackBox<K,V>([value](std::unordered_map<K, V> valMap) -> V { return value; }, std::unordered_set<K>());
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
	BlackBox(std::function<V(std::unordered_map<K, V>)> expression, std::unordered_set<K> indexSet)
	{
		this->expression = expression;
		setVars(indexSet);
	};
	~BlackBox() {};

	V evaluate(std::unordered_map<K, V> valMap) const { return expression(valMap); };
	V operator() (std::unordered_map<K, V> valMap) const { return expression(valMap); };
	std::vector<K> getVarList() const { return indexList; };
	std::unordered_set<K> getVarSet() const { return indexSet; };

private:
	std::vector<K> indexList;
	std::unordered_set<K> indexSet;
	std::function<V(std::unordered_map<K, V>)> expression;
	void setVars(std::unordered_set<K> indexSet)
	{
		this->indexSet = indexSet;
		for(auto &var : indexSet) {
			indexList.push_back(var);
		}
	};
};

using Polynomial = BlackBox<>;