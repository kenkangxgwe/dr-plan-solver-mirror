#pragma once

#include "bspline.h"

class RootFinder {
public:
    static std::vector<double> findZeros(SPLINTER::BSpline bs, int deg);

private:
    static bool findAZero(SPLINTER::BSpline bs, int deg, int k, double &zero);

};
