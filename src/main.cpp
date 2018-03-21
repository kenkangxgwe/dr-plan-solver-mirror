/*
 *   main.cpp
 */

#include "stdafx.h"
#include "Tree.h"
#include "TwoTree.h"

/*
 *  A data structure to represent the ordering of (constraint, parameter)
 *  pairs still needs to be implemented.
 *  I suggest a std::pair(parameter* p, constraint* c) data structure where
 *  parameter p gets eliminated in constraint c.
 *  Then a std::vector(pairs) can represent the ordering.
 */

using namespace DRPLAN;

int main(int argv, char* argc[])
{
	if(argv < 2) {
		std::cout << "Please input a file." << std::endl;
		return -1;
	}
    bool useDistanceinfo = true;
	TwoTree tt(argc[1], useDistanceinfo);
	tt.print_graph();
    tt.generateDRplan();
//    tt.printDRplan();

    unsigned sampleNum = 20;
    if(argv == 3) {
        sampleNum = (unsigned)std::stoul(argc[2]);
        if (sampleNum) {
            std::cout << "Using custom sample number:" << sampleNum << "." << std::endl;
        } else {
            std::cout << "Invalid sample number, using default value: 20." << std::endl;
        }
    }

    Tree drplan(&tt.getRoot(), sampleNum);
    /** HexTrig
     tt.flip.flipAt(3);
     tt.flip.flipAt(5);
     tt.flip.flipAt(6);
     */

    /** zig-zag-3
     */
    tt.flip.flipAt(3);
    tt.flip.flipAt(5);
    tt.flip.flipAt(6);
    tt.flip.flipAt(9);
    tt.flip.flipAt(12);
//    do{
        std::cout << std::endl << "Trying next flip:" << std::endl;
        if (drplan.solveTree()) {
            unsigned counter = 0;
            for (const auto &solution : drplan.finalSolutionList) {
                for (const auto &kvPair : solution) {
                    std::cout << " x_" << kvPair.first << " = " << std::setw(5) << kvPair.second << "\t";
                }
                std::cout << std::endl;
                tt.realize(solution, std::to_string(counter++));
                std::cout << std::endl;
            }
        }
//        tt.flip.next();
//    } while(!tt.flip.isBegin());
    return 0;
}

