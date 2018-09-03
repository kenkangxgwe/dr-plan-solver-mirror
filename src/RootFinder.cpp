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

#include "stdafx.h"
#include "RootFinder.h"
#include "Splinter/bsplinebuilder.h"

using namespace SPLINTER;

std::vector<double> RootFinder::findZeros(BSpline bs, unsigned deg, double tol)
{
    std::vector<double> zeros;
    DenseMatrix c = bs.getControlPoints();
    for (unsigned k = 1; k < bs.getNumControlPoints(); k++) {
        if(((double)c(k - 1, 1) - tol) * ((double)c(k, 1) - tol) > 0) {
            continue;
        }
        std::vector<double> tempZeros;
        tempZeros.push_back((double)c(k-1, 0));
        bool keepFinding = true;
        unsigned nextK = k;
        while(keepFinding) {
            std::vector<double> t = bs.getKnotVectors()[0];
            double intervalWidth = t[k + deg] - t[k];
            bool findZero = false;
            for(unsigned i = 0; i < 2; i++) {
                if(((double)c(k + i - 1, 1) - tol) * ((double)c(k + i, 1) - tol) > 0) {
                    continue;
                }
                findZero = true;
                tempZeros.push_back((double)c(k + i, 0) - ((double)c(k + i, 1) - tol) * (t[k + i + deg] - t[k + i]) / ((double)c(k + i, 1) - (double)c(k + i - 1, 1)) / deg);
                if (tempZeros.size() >= 2 && abs(tempZeros.back() - *(tempZeros.end() - 2)) < intervalWidth * 1E-15) {
                    zeros.push_back(tempZeros.back());
                    keepFinding = false;
                } else {
                    k = k + i;
                    try{
                        bs.insertKnots(tempZeros.back(), 0);
                    } catch (std::exception e) {
                        zeros.push_back(tempZeros.back());
                        keepFinding = false;
                    } catch(...) {
                        std::cout << bs.getControlPoints() << std::endl;
                    }
                    nextK++;
                }
                break;
            }
            if(!findZero) {
                keepFinding = false;
            }
            c = bs.getControlPoints();
        }
        k = nextK;
    }
    return zeros;
}

bool RootFinder::findAZero(BSpline bs, unsigned deg, unsigned k, double &zero) {
    std::vector<double> zeros;
    DenseMatrix c = bs.getControlPoints();
    double begin = c(k - 1, 0);
    double end = c(k, 0);
    while(true) {
        c = bs.getControlPoints();
        std::vector<double> t = bs.getKnotVectors()[0];
        double intervalWidth = t[k + deg] - t[k];
        bool findZero = false;
        for(unsigned i = 0; i < 2; i++) {
            if((double)c(k + i - 1, 1) * (double)c(k + i, 1) > 0) {
                continue;
            }
            findZero = true;
            zeros.push_back((double)c(k + i, 0) - (double)c(k + i, 1) * (t[k + i + deg] - t[k + i]) / ((double)c(k + i, 1) - (double)c(k + i - 1, 1)) / deg);
            if (zeros.size() >= 2 && abs(zeros.back() - zero) < intervalWidth * 1E-15) {
                zero = zeros.back();
                return (begin < zero && zero <= end);
            } else {
                k = k + i;
                zero = zeros.back();
                bs.insertKnots(zero, 0);
            }
            break;
        }
        if(!findZero) {
            return false;
        }
    }
}
