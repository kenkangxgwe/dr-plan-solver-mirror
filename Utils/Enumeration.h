#pragma once
#include <vector>

/**
 * The Enumeration class is a helper class to enumerate
 * the element in Cartesian product set from a container of vectors.
*/

template <typename V>
class EnumerationBase
{
public:

	class Enumerator {

	public:
		Enumerator(const EnumerationBase* enumeration)
        :sizeList(enumeration->sizeList)
		{
		};
		~Enumerator() {};

		unsigned getIndex (const unsigned index) const
		{
			return indexList[index];
		};

		void begin()
		{
//			for(unsigned i = 0; i < sizeList.size(); i++) {
//				indexList.push_back(0);
//			}
            _isEnd = false;
            indexList = std::vector<unsigned>(sizeList.size(), 0);
		};

		void end()
		{
            _isEnd = true;
//			for(unsigned i = 0; i < sizeList.size(); i++) {
//				indexList.push_back(sizeList.at(i) - 1);
//			}
            indexList = std::vector<unsigned>(sizeList.size(), 0);
		};

		bool operator == (const Enumerator &that) const
		{
            if(this->isEnd() != that.isEnd()) {
                return false;
            }
			for(unsigned i = 0; i < indexList.size(); i++) {
				if(indexList[i] != that.indexList[i]) {
					return false;
				}
			}
			return true;
		};

		bool operator != (const Enumerator &that) const
		{
			return !(*this == that);
		};

		void operator ++ () {
            if (_isEnd) {
                _isEnd = false;
                return;
            }
			for(int i = indexList.size() - 1; i >= 0; i--) {
				if(indexList[i] == sizeList.at(i) - 1) {
					indexList[i] = 0;
				}
				else {
					indexList[i]++;
                    return;
				}
			}
            _isEnd = true;
		};

		void operator -- ()
		{
            if (_isEnd) {
                _isEnd = false;
            } else {
                bool reachEnd = true;
                for(auto &index : indexList) {
                    if(index != 0) {
                        reachEnd = false;
                        break;
                    }
                }
                if(reachEnd) {
                    _isEnd = true;
                    return;
                }
            }
			for(int i = indexList.size() - 1; i >= 0; i--) {
				if(indexList[i] == 0) {
					indexList[i] = sizeList.at(i) - 1;
				}
				else {
					indexList[i]--;
                    return;
				}
			}
		};

        bool isEnd() const {return _isEnd;};

    private:
        bool _isEnd;
		std::vector<unsigned> indexList;
		const std::vector<unsigned> &sizeList;
    };

	EnumerationBase() {};
	~EnumerationBase() {};

	Enumerator begin() const {
		Enumerator begin(this);
		begin.begin();
		return begin;
	};

	Enumerator end() const {
		Enumerator end(this);
		end.end();
		return end;
	};

protected:
	std::vector<unsigned> sizeList;
	std::vector<std::vector<V>> vectorList;
};

template <typename K = void, typename V = double>
class Enumeration : public EnumerationBase<V>
{
public:

	Enumeration() {};
	~Enumeration() {};
	Enumeration(const std::unordered_map<K, std::vector<V>> mapList)
	{
		for(const auto & kvPair : mapList) {
			this->keyList.push_back(kvPair.first);
			this->sizeList.push_back(kvPair.second.size());
			this->vectorList.push_back(kvPair.second);
		}
	};

	Enumeration(const std::unordered_map<K, std::vector<V>> mapList, const std::vector<K> inputKeyList)
	{
		keyList = inputKeyList;
		for(const auto &key : keyList) {
			this->sizeList.push_back(mapList.at(key).size());
			this->vectorList.push_back(mapList.at(key));
		}
	};

	Enumeration(const std::unordered_map<K, std::vector<V>> mapList, const std::unordered_set<K> keySet)
	{
		for(const auto &key : keySet) {
			this->keyList.push_back(key);
			this->sizeList.push_back(mapList.at(key).size());
			this->vectorList.push_back(mapList.at(key));
		}
	};

	std::unordered_map<K, V> at (const typename Enumeration<K, V>::Enumerator enumer) const
	{
        if(enumer.isEnd()) {
            throw("Reach the end of the enumerator.");
        }
		std::unordered_map<K, V> result;
		for(unsigned i = 0; i < this->vectorList.size(); i++) {
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
	std::vector<R> applyFunc(const std::function<R(std::unordered_map<K, V>)> fn) const
	{
		std::vector<R> results;
		typename Enumeration<K, V>::Enumerator enumer = this->begin();
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
class Enumeration<void, V> : public EnumerationBase<V>
{
public:
	Enumeration() {};
	~Enumeration() {};

	Enumeration(const std::vector<std::vector<V>> inputVectorList)
	{
		this->vectorList = inputVectorList;
		for(auto & vector : this->vectorList) {
			this->sizeList.push_back(vector.size());
		}
	};

	std::vector<V> at(const typename Enumeration<void, V>::Enumerator enumer) const
	{
        if(enumer.isEnd()) {
            throw("Reach the end of the enumerator.");
        }
		std::vector<V> result;
		for(unsigned i = 0; i < this->vectorList.size(); i++) {
			result.push_back(this->vectorList[i][enumer.getIndex(i)]);
		}
		return result;
	};

	template <typename R>
	std::vector<R> applyFunc(const std::function<R(std::vector<V>)> fn) const
	{
		std::vector<R> results;
		typename Enumeration<void, V>::Enumerator enumer = this->begin();
		do {
			results.push_back(fn(this->at(enumer)));
			++enumer;
		} while(enumer != this->end());
		return results;
	};
};