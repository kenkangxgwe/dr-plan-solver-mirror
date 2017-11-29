#pragma once
#include "stdafx.h"

/**
 The Enumeration class is a helper class to
 construct Cartesian product from a list of vectors.
*/

template <typename V, typename K = void>
class EnumerationBase
{
public:

	class Enumerator {

	public:
		Enumerator(EnumerationBase* enumeration)
		{
			sizeList = &(enumeration->sizeList);
		};
		~Enumerator() {};

		int getIndex(int index)
		{
			return indexList[index];
		};

		void begin()
		{
			for(int i = 0; i < sizeList->size(); i++) {
				indexList.push_back(0);
			}
		};

		void end()
		{
			for(int i = 0; i < sizeList->size(); i++) {
				indexList.push_back(sizeList->at(i) - 1);
			}
		};

		bool operator == (Enumerator that)
		{
			for(int i = 0; i < indexList.size(); i++) {
				if(indexList[i] != that.indexList[i]) {
					return false;
				}
			}
			return true;
		};

		bool operator != (Enumerator that)
		{
			return !(*this == that);
		};

		void operator ++ () {
			for(int i = indexList.size() - 1; i >= 0; i--) {
				if(indexList[i] == sizeList->at(i) - 1) {
					indexList[i] = 0;
				}
				else {
					indexList[i]++;
					break;
				}
			}
		};

		void operator -- ()
		{
			for(int i = indexList.size() - 1; i >= 0; i--) {
				if(indexList[i] == 0) {
					indexList[i] = sizeList->at(i) - 1;
				}
				else {
					indexList[i]--;
					break;
				}
			}
		};

	private:
		std::vector<int> indexList;
		std::vector<int> *sizeList;
	};

	EnumerationBase() {};
	~EnumerationBase() {};

	Enumerator begin() {
		Enumerator begin(this);
		begin.begin();
		return begin;
	};

	Enumerator end() {
		Enumerator end(this);
		end.end();
		return end;
	};

protected:
	std::vector<int> sizeList;
	std::vector<std::vector<V>> vectorList;
};

template <typename V, typename K = void>
class Enumeration : public EnumerationBase<V, K>
{
public:

	Enumeration() {};
	~Enumeration() {};
	Enumeration(std::unordered_map<K, std::vector<V>> mapList)
	{
		for(auto & kvPair : mapList) {
			this->keyList.push_back(kvPair.first);
			this->sizeList.push_back(kvPair.second.size());
			this->vectorList.push_back(kvPair.second);
		}
	};

	Enumeration(std::unordered_map<K, std::vector<V>> mapList, std::vector<K> inputKeyList)
	{
		keyList = inputKeyList;
		for(auto &key : keyList) {
			this->sizeList.push_back(mapList[key].size());
			this->vectorList.push_back(mapList[key]);
		}
	};

	Enumeration(std::unordered_map<K, std::vector<V>> mapList, std::unordered_set<K> keySet)
	{
		for(auto &key : keySet) {
			this->keyList.push_back(key);
			this->sizeList.push_back(mapList[key].size());
			this->vectorList.push_back(mapList[key]);
		}
	};

	std::unordered_map<K, V> at(typename Enumeration<V, K>::Enumerator enumer)
	{
		std::unordered_map<K, V> result;
		for(int i = 0; i < this->vectorList.size(); i++) {
			result.emplace(this->keyList[i], this->vectorList[i][enumer.getIndex(i)]);
		}
		return result;
	};

	/**
	 * Applys the function to each enumeratons of the list,
	 * and collects the results in a list.
	 * @param fn the function to apply.
	 * @return the list of results.
	 */
	template <typename R>
	std::vector<R> applyFunc(std::function<R(std::unordered_map<K, V>)> fn)
	{
		std::vector<R> results;
		typename Enumeration<V, K>::Enumerator enumer = this->begin();
		do {
			results.push_back(fn(this->at(enumer)));
			++enumer;
		} while(enumer != this->end());
		return results;
	};

private:
	std::vector<K> keyList;
};

template<typename V>
class Enumeration<V, void> : public EnumerationBase<V, void>
{
public:
	Enumeration() {};
	~Enumeration() {};

	Enumeration(std::vector<std::vector<V>> inputVectorList)
	{
		this->vectorList = inputVectorList;
		for(auto & vector : this->vectorList) {
			this->sizeList.push_back(vector.size());
		}
	};

	std::vector<V> at(typename Enumeration<V, void>::Enumerator enumer)
	{
		std::vector<V> result;
		for(int i = 0; i < this->vectorList.size(); i++) {
			result.push_back(this->vectorList[i][enumer.getIndex(i)]);
		}
		return result;
	};

	template <typename R>
	std::vector<R> applyFunc(std::function<R(std::vector<V>)> fn)
	{
		std::vector<R> results;
		typename Enumeration<V, void>::Enumerator enumer = this->begin();
		do {
			results.push_back(fn(this->at(enumer)));
			++enumer;
		} while(enumer != this->end());
		return results;
	};
};