/*
 *   main.cpp
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include "DR-plan/Factory.h"
#include "DR-plan/Tree.h"
#include "DR-plan/Node.h"
#include "utils/BlackBox.hpp"
#include "TwoTree/TwoTree.h"

/*
 *  A data structure to represent the ordering of (constraint, parameter)
 *  pairs still needs to be implemented.
 *  I suggest a std::pair(parameter* p, constraint* c) data structure where
 *  parameter p gets eliminated in constraint c.
 *  Then a std::vector(pairs) can represent the ordering.
 */

int main(int argv, char* argc[])
{
	if(argv != 2) {
		std::cout << "Please input a file." << std::endl;
		return -1;
	}
    bool useDistanceinfo = true;
	TwoTree tt = TwoTree(argc[1], useDistanceinfo);
//	tt.print_graph();
    tt.generateDRplan();
//    tt.printDRplan();
    DRPlan::Tree<Node> drplan(&tt.getRoot(), 50);
    tt.flip.flipAt(3);
    tt.flip.flipAt(5);
    tt.flip.flipAt(6);
//    do{
//        std::cout << std::endl << "Trying next flip:" << std::endl;
        if (drplan.solveTree()) {
            unsigned counter = 0;
            for (const auto &solution : drplan.finalSolutionList) {
                for (const auto &kvPair : solution) {
                    std::cout << " x_" << kvPair.first << " = " << std::setw(5) << kvPair.second << "\t";
                }
                tt.realize(solution, std::to_string(counter++));
                std::cout << std::endl;
            }
        }
//        tt.flip.next();
//    } while(!tt.flip.isBegin());
    return 0;
}

