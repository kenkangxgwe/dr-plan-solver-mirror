#include "stdafx.h"
#include "Enumeration.h"

template<class V, typename K>
Enumeration<V, K>::Enumerator::Enumerator(Enumeration* enumeration)
{
	sizeList = &(enumeration->sizeList);
}

template<class V, typename K>
Enumeration<V, K>::Enumerator::~Enumerator()
{
}

template<class V, typename K>
bool Enumeration<V, K>::Enumerator::operator!=(Enumerator that)
{
    return !(*this==that);
}

template<class V, typename K>
bool Enumeration<V, K>::Enumerator::operator==(Enumerator that)
{
	for (int i = 0; i < indexList.size(); i++) {
		if (indexList[i] != that.indexList[i]) {
			return false;
		}
	}
	return true;
}

template<class V, typename K>
void Enumeration<V, K>::Enumerator::operator++()
{
	for (int i = indexList.size() - 1; i >= 0; i++) {
		if (indexList[i] == sizeList[i].size() - 1) {
			indexList[i] = 0;
		}
		else {
			indexList[i]++;
			break;
		}
	}
}

template<class V, typename K>
void Enumeration<V, K>::Enumerator::operator--()
{
	for (int i = indexList.size() - 1; i >= 0; i++) {
		if (indexList[i] == 0) {
			indexList[i] = sizeList[i].size() - 1;
		}
		else {
			indexList[i]--;
			break;
		}
	}
}

template<class V, typename K>
void Enumeration<V, K>::Enumerator::begin() {
    for(int i = 0; i < sizeList->size(); i++) {
        indexList.push_back(0);
    }
}

template<class V, typename K>
void Enumeration<V, K>::Enumerator::end() {
    indexList = std::vector<int>(*sizeList);
}

