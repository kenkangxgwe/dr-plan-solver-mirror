//
// Created by kenkangxgwe on 2018.1.11.
//

#include "RootFinder.h"
#include "bsplinebuilder.h"

using namespace SPLINTER;

std::vector<double> RootFinder::findZeros(BSpline bs, int deg)
{
    std::vector<double> zeros;
    DenseMatrix c = bs.getControlPoints();
    if(c(0, 1) == 0) {

    }
    for (int k = 1; k < bs.getNumControlPoints(); k++) {
        if((double)c(k - 1, 1) * (double)c(k, 1) > 0) {
            continue;
        }
        double zero;
        if(findAZero(bs, deg, k, zero)) {
            zeros.push_back(zero);
        }
    }
    return zeros;
}

bool RootFinder::findAZero(BSpline bs, int deg, int k, double &zero) {
    std::vector<double> zeros;
    DenseMatrix c = bs.getControlPoints();
    double begin = c(k - 1, 0);
    double end = c(k, 0);
    while(true) {
        DenseMatrix c = bs.getControlPoints();
        std::vector<double> t = bs.getKnotVectors()[0];
        double intervalWidth = t[k + deg] - t[k];
        bool findZero = false;
        for(int i = 0; i < 2; i++) {
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
