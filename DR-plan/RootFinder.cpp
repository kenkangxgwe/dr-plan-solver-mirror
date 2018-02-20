//
// Created by kenkangxgwe on 2018.1.11.
//

#include "RootFinder.h"
#include "Splinter/bsplinebuilder.h"

using namespace SPLINTER;

std::vector<double> RootFinder::findZeros(BSpline bs, unsigned deg)
{
    std::vector<double> zeros;
    DenseMatrix c = bs.getControlPoints();
    for (unsigned k = 1; k < bs.getNumControlPoints(); k++) {
        if((double)c(k - 1, 1) * (double)c(k, 1) > 0) {
            continue;
        }
        std::vector<double> tempZeros;
        bool keepFinding = true;
        unsigned nextK = k;
        while(keepFinding) {
            std::vector<double> t = bs.getKnotVectors()[0];
            double intervalWidth = t[k + deg] - t[k];
            bool findZero = false;
            for(unsigned i = 0; i < 2; i++) {
                if((double)c(k + i - 1, 1) * (double)c(k + i, 1) > 0) {
                    continue;
                }
                findZero = true;
                tempZeros.push_back((double)c(k + i, 0) - (double)c(k + i, 1) * (t[k + i + deg] - t[k + i]) / ((double)c(k + i, 1) - (double)c(k + i - 1, 1)) / deg);
                if (tempZeros.size() >= 2 && abs(tempZeros.back() - *(tempZeros.end() - 2)) < intervalWidth * 1E-15) {
                    zeros.push_back(tempZeros.back());
                    keepFinding = false;
                } else {
                    k = k + i;
                    bs.insertKnots(tempZeros.back(), 0);
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
