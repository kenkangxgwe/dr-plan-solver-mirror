
/*
 * interpolation.hpp
 *
 *  Middle-man for interpolation
 *  To change behavior of interpolation, modify this file and it's source 
 *  As long as the behavior of this file never changes,
 *      you should not have to modify anywhere that it is used
 */

#ifndef INTERPOLATE_H
#define INTERPOLATE_H

#include <vector>
#include "spline.hpp"

class interpolant
{
public:
    interpolant();
    void set_data(const std::vector<double> &x, const std::vector<double> &y);
    double operator()(double x) const;
    std::vector<double> find_roots();
private:
    spline spl;
};

#endif /* INTERPOLATE_H */
