#include "stdafx.h"


template<typename V, typename K>
Enumeration<V, K>::Enumeration()
{
}

//template<typename V>
//Enumeration<V, void>::Enumeration(std::vector<std::vector<V>> inputVectorList)
//{
//	vectorList = inputVectorList;
//	for(auto & vector : vectorList) {
//		sizeList.push_back(vector.size());
//	}
//}

template<typename V, typename K>
Enumeration<V, K>::Enumeration(std::unordered_map<K, std::vector<V>> mapList)
{
	for(auto & kvPair : mapList) {
		keyList.push_back(kvPair.first);
		sizeList.push_back(kvPair.second.size());
		vectorList.push_back(kvPair.second);
	}
}

template<typename V, typename K>
Enumeration<V, K>::Enumeration(std::unordered_map<K, std::vector<V>> mapList, std::vector<K> inputKeyList)
{
	keyList = inputKeyList;
	for(auto &key : keyList) {
		sizeList.push_back(mapList[key].size());
		vectorList.push_back(mapList[key]);
	}
}

template<typename V, typename K>
Enumeration<V, K>::Enumeration(std::unordered_map<K, std::vector<V>> mapList, std::unordered_set<K> keySet) {
	for(auto &key : keySet) {
		keyList.push_back(key);
		sizeList.push_back(mapList[key].size());
		vectorList.push_back(mapList[key]);
	}
}


template<typename V, typename K>
Enumeration<V, K>::~Enumeration()
{
}

template<typename V, typename K>
typename Enumeration<V, K>::Enumerator Enumeration<V, K>::begin()
{
	Enumeration<V, K>::Enumerator begin(this);
    begin.begin();
	return begin;
}

template<typename V, typename K>
typename Enumeration<V, K>::Enumerator Enumeration<V, K>::end()
{
	Enumerator end(this);
	end.end();
	return end;
}

template<typename V, typename K>
std::unordered_map<K, V> Enumeration<V, K>::at(typename Enumeration<V, K>::Enumerator enumer)
{
	std::unordered_map<K, V> result;
	for(int i = 0; i < vectorList.size(); i++) {
		result.push_back(std::pair<K, V>(keyList[i], vectorList[i][enumer.indexList[i]]));
	}
	return result;
}

//template<typename V>
//std::vector<V> Enumeration<V, void>::at(typename Enumeration<V, void>::Enumerator enumer)
//{
//	std::vector<V> result;
//	for(int i = 0; i < vectorList.size(); i++) {
//		result.push_back(vectorList[i][enumer.indexList[i]]);
//	}
//	return result;
//}
//
template<typename V, typename K>
template<typename R>
std::vector<R> Enumeration<V, K>::applyFunc(std::function<R(std::unordered_map<K, V>)> fn)
{
	std::vector<R> results;
	Enumerator enumer = begin();
	do {
		results.push_back(fn(at(enumer)));
		++enumer;
	} while(enumer != end());
	return results;
}

//template<typename V>
//template<typename R>
//std::vector<R> Enumeration<V, void>::applyFunc(std::function<R(std::vector<V>)> fn)
//{
//	std::vector<R> results;
//	Enumerator enumer = begin();
//	do {
//		results.push_back(fn(at(enumer)));
//		++enumer;
//	} while(enumer != end());
//	return results;
//}
