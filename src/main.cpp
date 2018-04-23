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
#include "Tree.h"
#include "TwoTree.h"

/**
 *  A data structure to represent the ordering of (constraint, parameter)
 *  pairs still needs to be implemented.
 *  I suggest a std::pair(parameter* p, constraint* c) data structure where
 *  parameter p gets eliminated in constraint c.
 *  Then a std::vector(pairs) can represent the ordering.
 */

using namespace DRPLAN;
namespace po = boost::program_options;

std::vector<unsigned int> parseFlip(std::string inputFlip)
{
    std::vector<unsigned> flipIdx;
    std::string::size_type idx = 0;
    while(idx < inputFlip.size()) {
        inputFlip = inputFlip.substr(idx);
        unsigned curFlip = (unsigned) std::stoul(inputFlip, &idx);
        if(!curFlip) {
            break;
        }
        flipIdx.push_back(curFlip);
        idx++;
    }
    return flipIdx;
}


int main(int argc, char *argv[])
{
    /**
     * Arguments
     */
    po::options_description desc("Allowed options");
    desc.add_options()
            ("input-file", po::value<std::string>(), "input file (.dot)")
            ("flip,f", po::value<std::string>(), "vertex to be flipped (default: none)")
            ("sample,s", po::value<int>(), "sample number (default: 20)")
            ("use-length,l", po::value<bool>(), "whether use edge length from the input (default: true).")
            ("help,h", "produce help message");
    po::positional_options_description p;
    p.add("input-file", -1);
    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv).options(desc).positional(p).run(), vm);
    po::notify(vm);
    if(vm.count("help")) {
        std::cout << desc << std::endl;
        return 1;
    }
    if(!vm.count("input-file")) {
        std::cout << "Please input a file." << std::endl;
        return -1;
    }

    /**
     * Parse and decide whether use original or uniform edge lengths of the input graph.
     */
    bool useLengthinfo = true;
    if(vm.count("use-length")) {
        useLengthinfo = vm["use-length"].as<bool>();
        if(useLengthinfo) {
            std::cout << "Use the edge lengths from the input graph." << std::endl;
        } else {
            std::cout << "Apply the uniform lengths." << std::endl;
        }
    }

    /**
     * Generate DR-plan.
     */
    std::string inputFile = vm["input-file"].as<std::string>();
    TwoTree tt(inputFile, useLengthinfo);
    tt.print_graph();
    tt.generateDRplan();
    tt.printDRplan();

    /**
     * Parse and apply the sample number.
     */
    unsigned sampleNum = 20;
    if(vm.count("sample")) {
        sampleNum = (unsigned) vm["sample"].as<int>();
        if(sampleNum) {
            std::cout << "Using custom sample number:" << sampleNum << "." << std::endl;
        } else {
            sampleNum = 20;
            std::cout << "Invalid sample number. Use default value: 20." << std::endl;
        }
    } else {
        std::cout << "Use default value: 20." << std::endl;
    }
    Tree drplan(&tt.getRoot(), sampleNum);
//    std::vector<int> flipIdx({3, 5, 6}); // HexTrig
//    std::vector<int> flipIdx({3, 5, 6, 9, 12}); // zig-zag-3
//    std::vector<unsigned> flipIdx({3, 5, 6, 9, 12, 13, 15, 16, 18}); // zig-zag-4
//    std::vector<unsigned> flipIdx({3, 5, 6, 9, 12, 13, 15, 16, 18, 19, 21, 24, 26, 28}); // zig-zag-7

    /**
     * Parse and flip the edges.
     */
    bool enumerateAll = true;
    if(vm.count("flip")) {
        std::vector<unsigned> flipIdx = parseFlip(vm["flip"].as<std::string>());
        std::cout << "The flips are: ";
        for(const auto &flip : flipIdx) {
            std::cout << flip << " ";
        }
        std::cout << std::endl;
        for(const auto &idx : flipIdx) {
            tt.flip.flipAt(idx);
        }
        enumerateAll = false;
    } else {
        std::cout << "No flips specified. Try enumerating all flips." << std::endl;
    }

    /**
     * Start solve the DR-plan.
     */
    do {
        std::cout << std::endl << "Trying next flip:" << std::endl;
        if(drplan.solveTree()) {
            unsigned counter = 0;
            for(const auto &solution : drplan.finalSolutionList) {
                for(const auto &kvPair : solution) {
                    std::cout << " x_" << kvPair.first << " = " << std::setw(5) << kvPair.second << "\t";
                }
                std::cout << std::endl;
                tt.realize(solution, std::to_string(counter++));
                std::cout << std::endl;
            }
        }
        if(!enumerateAll) {
            break;
        }
        tt.flip.next();
    } while(!tt.flip.isBegin());

    return 0;
}
