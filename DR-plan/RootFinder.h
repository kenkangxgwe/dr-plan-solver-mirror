#pragma once

#include "Splinter/bspline.h"

class RootFinder {
public:
    static std::vector<double> findZeros(SPLINTER::BSpline bs, unsigned deg);

private:
    static bool findAZero(SPLINTER::BSpline bs, unsigned deg, unsigned k, double &zero);

};
