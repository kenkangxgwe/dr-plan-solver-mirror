#include "stdafx.h"
#include "interpolation.hpp"

/*
 * interpolation.cpp
 *
 *  Middle-man for interpolation
 *  To change behavior of interpolation, modify this file and it's header
 *  As long as the behavior of this file never changes,
 *      you should not have to modify anywhere that it is used
 */

interpolant::interpolant()
{
}

void interpolant::set_data(const std::vector<double> &x, const std::vector<double> &y)
{
    this->spl.set_points(x, y, true);
}

double interpolant::operator()(double x) const
{
    return this->spl(x);
}

std::vector<double> interpolant::find_roots()
{
    return this->spl.find_roots();
}
