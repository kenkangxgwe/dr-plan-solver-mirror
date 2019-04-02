# Wolfram DR-Plan Package

## Introduction

This is a wolfram implementation of the DR-Plan Solver originally written in
C++ (in the root folder) to provide better numerical precision and
demonstrative plots for research interests.

What it takes as input 
- a planar graph with edge distances that is minimally rigid in 2D,
- a flex DR-plan (a recursive decomposition into minimally rigid, maximal
proper subgraphs), and
- a desired flip (orientation) of a 2D realization of the vertices as points
that achieves the edge distances.

For output, it will search and return all the realization.

## Requirements

- _Wolfram Mathematica_ (tested on 11.3)
- Several graphviz (.dot) files where
    - vertices ordered in two tree construction order;
    - vertices have initial positions;
    - edges ordered in two tree construction order,
    - edges has different colors representing different types
        - __black / gray:__ partial edge
        - __green:__ added edge (Cayley parameter)
        - __red / pink:__ dropped edge
    - see example graphs in root folder for reference


## Usages

1. Open a new notebook (.nb) in Mathematica, and paste the following code.
    ```Mathematica
    (* import the code *)
    << "/path/to/DRPLAN/Wolfram/init.wls";
    (* import the graph *)
    exampleDRPlan = DRPLAN`NewDRNode["/path/to/graphviz_file.dot"];
    (* generate DR-Plan according to the graph *)
    GenerateDRPlan[exampleDRPlan];
    (* flip the clock-wise constructed vertices *)
    FlipAt[exampleDRPlan, {3, 5, 7, 8}];
    (* To see a preview of the DR-Plan *)
    PrintDRPlan[exampleDRPlan]
    (* To solve the DR-Plan *)
    SolveDRPlan[exampleDRPlan]
    ```

2. After you get the solution in `Association` form, use `AnalyzeSolution` to see the result graph.  
    ```Mathematica
    AnalyzeSolution[exampleDRPlan, 
        <|54 -> 290.85, 53 -> 299.48, 55 -> 428.20, 56 -> 627.10, 57 -> 781.22, 58 -> 283.02, 59 -> 310.26, 60 -> 450.93, 61 -> 602.52, 62 -> 756.48, 63 -> 898.29, 64 -> 1037.55, 65 -> 263.81, 66 -> 283.26, 67 -> 425.22, 68 -> 605.50, 69 -> 762.23, 70 -> 908.12, 71 -> 1046.99, 72 -> 1190.59, 73 -> 1331.90|>
    ]
    ```

3. You may also want to allow some offset for dropped edges, and perform a
priority search.
    ```Mathematica
    (*
        Start searching for solutions by allow the length of
        the dropped edge multiplied by one of the given factors
    *)
    SolveAllOffsetsStart[exampleDRPlan, {1, 0.95, 1.05}]
    ```
    After you get the result, you may continue your search for less
    prioritized results.
    ```Mathematica
    (* Continue the last search for the specified DR-Plan *)
    SolveAllOffsetsContinue[exampleDRPlan]
    ```


## Features

There are several features which the original C++ program does not have and they are important and benefit the algorithm.

### Refined Sampling

Instead of uniform sampling

### Bead Threading

### Alternative Interpolation

### Priority Search w/ Drop Offsets 


## Results

Here is the results from a graph adapted from hexagonal lattice.

### Original graph

### Input DR-Plan

### Output DR-Plan
