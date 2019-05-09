# CayMos and DR-Plan Solutions #

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [CayMos and DR-Plan Solutions](#caymos-and-dr-plan-solutions)
    - [Graph 1](#graph-1)
    - [Graph 2](#graph-2)

<!-- markdown-toc end -->


## Graph 1 ##

CayMos Dropped Edge: v7--v8

DR-Plan Dropped Edge: v2--v6, v6--v8, v2--v8

DR-Plan Added Edge: v1--v3, v3--v8, v4--v6

| No. | CayMos Flip | TwoTree Flip       | CayMos Graph                                                             | TwoTree Graph                                                                | Solved? (19/22) |
| :-- | :--         | :--                | :--                                                                      | :--                                                                          | :--             |
|   1 | {}          | {4, 6, 7, 8}       | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos..v0--v4.241.736.png)      | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,6,7,8.v0--v4.241.736.svg) | Approx (1%)     |
|   2 | {}          | {4, 6, 7 ,8}       | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos..v0--v4.256.564.png)      | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,6,7,8.v0--v4.256.564.svg) | Approx (1%)     |
|   3 | {4}         | {3, 4, 5}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.4.v0--v4.173.550.png)     | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.3,4,5.v0--v4.173.550.svg)   |                 |
|   4 | {4}         | {3, 4, 5}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.4.v0--v4.141.986.png)     | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.3,4,5.v0--v4.144.167.svg)   | Different       |
|   5 | {3, 4}      | {5, 6, 8}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4.v0--v4.173.581.png)   | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.5,6,8.v0--v4.173.581.svg)   |                 |
|   6 | {3, 4}      | {3, 5, 6}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4.v0--v4.279.855.png)   | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.3,5,6.v0--v4.279.855.svg)   |                 |
|   7 | {3, 4}      | {(3), 5, (6), (8)} | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4.v0--v4.242.516.png)   |                                                                              | No              |
|   8 | {3, 4}      | {4, 5, (6), (8)}   | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4.v0--v4.244.911.png)   |                                                                              | No              |
|   9 | {7}         | {3, 4, 6}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.7.v0--v4.230.030.png)     | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.3,4,6.v0--v4.230.030.svg)   |                 |
|  10 | {7}         | {4, 6, 8}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.7.v0--v4.259.686.png)     | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,6,8.v0--v4.259.686.svg)   |                 |
|  11 | {8}         | {4, 6, 7}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.8.v0--v4.257.344.png)     | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,6,7.v0--v4.257.344.svg)   |                 |
|  12 | {8}         | {4, 6, 7}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.8.v0--v4.236.273.png)     | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,6,7.v0--v4.236.273.svg)   |                 |
|  13 | {3, 4, 7}   | {4, 5, 7}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,7.v2--v5.284.204.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,5,7.v2--v5.284.204.svg)   |                 |
|  14 | {3, 4, 7}   | {4, 5, 7}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,7.v2--v5.201.738.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,5,7.v2--v5.201.738.svg)   |                 |
|  15 | {3, 4, 7}   | {5, 7}             | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,7.v2--v5.154.215.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.5,7.v2--v5.154.215.svg)     |                 |
|  16 | {3, 4, 7}   | {5, 6, 7}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,7.v2--v5.147.226.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.5,6,7.v2--v5.147.226.svg)   | No              |
|  17 | {3, 4, 7}   | {4, 5, 7, 8}       | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,7.v2--v5.218.511.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,5,7,8.v2--v5.218.511.svg) | Approx (1%)     |
|  18 | {3, 4, 7}   | {4, 5, 7}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,7.v2--v5.281.409.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,5,7.v2--v5.281.409.svg)   |                 |
|  19 | {3, 4, 8}   | {3, 5, 6, 8}       | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,8.v0--v4.269.802.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.3,5,6,8.v0--v4.269.802.svg) | Approx (1%)     |
|  20 | {3, 4, 8}   | {5, 8}             | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.3,4,8.v0--v4.220.974.png) | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.5,8.v0--v4.220.974.svg)     | Approx (1%)     |
|  21 | {7, 8}      | {4, 6, 8}          | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.7,8.v2--v5.227.689.png)   | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.4,6,8.v2--v5.227.689.svg)   |                 |
|  22 | {7, 8}      | {3, 4, 6, 8}       | ![CayMosGraph](./graph_1/SolutionGraphs/CayMos.7,8.v2--v5.267.490.png)   | ![TwoTreeGraph](./graph_1/SolutionGraphs/TwoTree.3,4,6,8.v2--v5.267.490.svg) |                 |

## Graph 2 ##

CayMos Dropped Edge: v7--v8

DR-Plan Dropped Edge: v2--v6, v6--v8, v2--v8

DR-Plan Added Edge: v1--v3, v3--v8, v4--v6

| No. | CayMos Flip | TwoTree Flip | CayMos Graph                                                          | TwoTree Graph                                                                 | Solved?     |
| :-- | :--         | :--          | :--                                                                   | :--                                                                           | :--         |
|   1 | {}          | {3, 4, 5, 8} | ![CayMos Graph](./graph_2/SolutionGraphs/CayMos..v0--v4.190.003.png)  | ![TwoTree Graph](./graph_2/SolutionGraphs/TwoTree.3,4,5,8.v0--v4.190.003.svg) | Approx (2%) |
|   2 | {}          | {3, 4, 5, 8} | ![CayMos Graph](./graph_2/SolutionGraphs/CayMos..v0--v4.187.560.png)  | ![TwoTree Graph](./graph_2/SolutionGraphs/TwoTree.3,4,5,8.v0--v4.187.560.svg) |             |
|   3 | {3}         | {4, 5, 8}    | ![CayMos Graph](./graph_2/SolutionGraphs/CayMos.3.v0--v4.191.425.png) | ![TwoTree Graph](./graph_2/SolutionGraphs/TwoTree.4,5,8.v0--v4.191.425.svg)   | Approx (2%) |
|   4 | {3}         | {5, 8}       | ![CayMos Graph](./graph_2/SolutionGraphs/CayMos.3.v0--v4.187.267.png) | ![TwoTree Graph](./graph_2/SolutionGraphs/TwoTree.5,8.v0--v4.187.267.svg)     |             |
