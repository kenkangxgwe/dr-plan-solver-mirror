(* ::Package:: *)

(*
  This file is part of DRPLAN.

  DRPLAN is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  DRPLAN is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
*)


(* ::Title:: *)
(*DRPLAN Solver*)


BeginPackage["DRPLAN`Solver`", {"DRPLAN`Core`"}]
ClearAll[Evaluate[Context[] <> "*"]]


SolveDRPlan::usage = "SolveDRPlan[node_DRNode] solves a DRPlan by passing in the root DR-node."
SolveNode::usage = "SolveNode[node_DRNode] solves the input node and its sub-nodes and returns all solutions.
SolveNode[node_DRNode, dFlip:(All | _List)] solves the node only for specified D-flip."
InterpolatingFunctionGroup::usage = "A group of interpolating functions for smoothing / piece-wise purpose."
ToPlanSolution::usage = "ToPlanSolution[node_DRNode, nodeSolution_NodeSolution] turns a node solution for the root node to a plan solution."
NodeSolution::usage = "An object that contains the information for a node solution."
$NodeSolutionCallback::usage = "$NodeSolutionCallback[sols_List, node_DRNode] is a hook function that will be called when a node is solved. \
The input will the solutions of the node and level that node is at. The output must be a list of solutions that will be returned to its parent node."
PlanErrorMap::usage = "PlanErrorMap[node_DRNode, planSolution_PlanSolution] returns an association that maps edge to {absoluteError, relativeError}."
PlanEdgeError::usage = "PlanEdgeError[node_DRNode, planSolution_PlanSolution, o:OptionsPattern[]] returns the errors of the dropped edges."
SimilarPlanQ::usage = "SimilarPlanQ[planSolution1_PlanSolution, planSolution2_PlanSolution] returns True if two plan solutions are similar to each other."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DRPLAN`Thread`"]
Needs["DRPLAN`Utility`"]


UnEcho = (#1&)
PrintEcho = ((Print[#1];#1)&)


InterpolatingFunctionGroup /: MakeBoxes[group:InterpolatingFunctionGroup[ifs__InterpolatingFunction], StandardForm] :=
    BoxForm`ArrangeSummaryBox[InterpolatingFunctionGroup, sol, (*icon=*)None, {
        BoxForm`SummaryItem[{"InterpolatingFunctions: ", Column@{ifs}}],
        BoxForm`SummaryItem[{"Domain: ", group["Domain"]}]
    }, {}, StandardForm, "Interpretable" -> Automatic]


NodeSolution /: MakeBoxes[sol:NodeSolution[solution_Association, domain_Association, dFlip_List, cFlip_Association], StandardForm] :=
    BoxForm`ArrangeSummaryBox[NodeSolution, sol, (*icon=*)None,
        {
            BoxForm`SummaryItem[{"Solution: ", Column @ Normal @ solution}],
            BoxForm`SummaryItem[{"Domain: ", Column @ Normal @ domain}]
        },
        {
            BoxForm`SummaryItem[{"D-Flip: ", dFlip}],
            BoxForm`SummaryItem[{"C-Flip: ", cFlip}]
        },
    StandardForm, "Interpretable" -> Automatic]


ToString[NodeSolution[solution_Association, domain_Association, dFlip_List, cFlip_Association]] ^:= StringJoin[
    "NodeSolution:\n",
    "    Solution:\n",
    {"        ", ToString[#], "\n"}& /@ (Normal @ solution),
    "    Domain:\n",
    {"        ", ToString[#], "\n"}& /@ (Normal @ domain),
    "    D-Flip: ", ToString[dFlip], "\n",
    "    C-Flip: ", ToString[cFlip]
]


PrintProgress[node_DRNode] := (
    NotebookDelete[$lastProgress];
    $lastProgress = PrintTemporary[Column[{
        Row[{"Num of solutions for node ", ToString[node], ": ", Length[node["Solutions"]]}],
        Row[{"Memory in use: ", N[UnitConvert[Quantity[MemoryInUse[], "Bytes"], "Megabytes"]]}]
    }]];
)

(* ::Section:: *)
(*Realization*)


(* ::Subsection:: *)
(*Realize*)


Realize[node_DRNode] := Module[
    {
        cayleyLength
    },

    cayleyLength = <| # -> PropertyValue[{node["Root"]["Graph"], #}, EdgeWeight]& /@ node["AllCayley"] |>;
    Realize[node, PlanSolution[cayleyLength, GetFlip[node], {}]]
]

Realize[node_DRNode, PlanSolution[cayleyLength_Association, flipVectors_List, _]] := Module[
    {
        coordsList
    },

    Check[
        coordsList = calcCoords[node, cayleyLength, flipVectors];
        Subgraph[node["Graph"], Keys[coordsList], VertexCoordinates -> Normal@coordsList, VertexLabels->"Name"],
        $Failed
    ]
]


PlanErrorMap[node_DRNode, planSolution_PlanSolution] := PlanErrorMap[node, Realize[node["Root"], planSolution]]
PlanErrorMap[node_DRNode, resultGraph_Graph] := With[
    {
        originGraph = node["Root"]["Graph"]
    },

    Select[EdgeList[originGraph], PropertyValue[{originGraph, #}, "EdgeType"] =!= "Partial"&]
    // Map[edge \[Function] (edge -> (
        {EuclideanDistance[
            PropertyValue[{resultGraph, First[edge]}, VertexCoordinates],
            PropertyValue[{resultGraph, Last[edge]}, VertexCoordinates]
        ], PropertyValue[{originGraph, edge}, EdgeWeight]}
        // Apply[{#1 - #2, #1 / #2 - 1}&]
        // Map[Chop]
    ))]
    // Apply[Association] (* Association is HoldAllComplete *)

]

Options[PlanEdgeError] = {
    "IncludeBoundary" -> True
}
PlanEdgeError[node_DRNode, planSolution_PlanSolution, o:OptionsPattern[]] := PlanEdgeError[node, PlanErrorMap[node, planSolution], o]
PlanEdgeError[node_DRNode, errorMap_Association, o:OptionsPattern[]] := With[
    {
        originGraph = node["Root"]["Graph"]
    },

    errorMap
    // Lookup[Select[EdgeList[originGraph], (
        PropertyValue[{originGraph, #}, "EdgeType"] === "Drop" &&
        (PropertyValue[{originGraph, #}, "BoundaryQ"] \[Implies] OptionValue["IncludeBoundary"])
    )&]]
    // Transpose // Last
]


Options[SimilarPlanQ] = {
    "Threshold" -> 1
}

SimilarPlanQ[
    PlanSolution[
        cayleyLength1_Association,
        flipVector1_List,
        _
    ], PlanSolution[
        cayleyLength2_Association,
        flipVector2_List,
        _
    ],
    o: OptionsPattern[]
] := Module[
    {
        threshold, cayleys(* , maxCayley *)
    },

    {threshold} = OptionValue[SimilarPlanQ, {o}, {"Threshold"}];

    If[flipVector1 =!= flipVector2,
        Return[False]
    ];

    cayleys = Keys[cayleyLength1];
    If[cayleys =!= Keys[cayleyLength2],
        Return[False]
    ];

    (* {maxDiff, maxCayley} = *) Table[
        Abs[cayleyLength1[cayley] - cayleyLength2[cayley]] / Max[cayleyLength1[cayley], cayleyLength2[cayley]],
        {cayley, cayleys}
    ] // Max
    (* {maxDiff, maxCayley} = Table[
        {Abs[cayleyLength1[cayley] - cayleyLength2[cayley]], cayley},
        {cayley, cayleys}
    ] // MaximalBy[First] // First;

    (maxDiff / Max[cayleyLength1[maxCayley], cayleyLength2[maxCayley]]) < threshold *)

]

(* Only works for flex-1 *)
PlanDiversity[
    {PlanSolution[
        cayleyLength1_Association,
        flipVector1_List,
        dFlip1_List
    ], PlanSolution[
        cayleyLength2_Association,
        flipVector1_List,
        dFlip2_List
    ]},
    root_DRNode
] := Module[
    {
        dDiversity, cDiversity, cayleyLevel
    },

    dDiversity = (dFlip1 - dFlip2)
    // Position[Except[0]?NumericQ]
    // MaximalBy[Length]
    // Replace[{} -> {{}}]
    // First
    // Length;

    cayleyLevel = (Max @@@ Part[
        EdgeList[root["Graph"]],
        Table[cayley, {cayley, root["AllCayley"]}]
    ])
    // SortBy[Minus]
    // RotateRight;

    cDiversity = {flipVector1, flipVector2}
    // Map[Keys]
    // Apply[Complement[Union[#1, #2], Intersection[#1, #2]]&]
    // Min
    // FirstPosition[cayleyLevel, #]&
    // Replace[_?MissingQ -> {0}]
    // First;

    Max[dDiversity, cDiversity]
]


(* ::Subsection:: *)
(*Immutable Type Definition*)

DataType`DeclareType[DRNode, <|
    "Root" -> _Association,
    "Graph" -> _Graph,
    "FreeCayley" -> _List,
    "TargetDrop" -> _Integer,
    "TargetCayley" -> _Integer
|>]


(*
    This function stores the subvalues of the DRNode[$id] into a immutable association
    in order to pass it for parallel solving.
    It should has the same interface as the DRNode[$id].
    If the solving algorithm changes, do not forget to add those values needed here.
*)
PersistDRNode[node_DRNode] := (
    DRNode[<|
        "Root" -> <|
            "Graph" -> node["Root"]["Graph"],
            "PlanShortestEdge" -> node["Root"]["PlanShortestEdge"]
        |>,
        "Graph" -> node["Graph"],
        "FreeCayley" -> node["FreeCayley"],
        "TargetDrop" -> node["TargetDrop"],
        "TargetCayley" -> node["TargetCayley"]
    |>]
)


(* ::Subsection:: *)
(*Solving*)


(* See options in SolveNode *)
Options[SolveDRPlan] := Options[SolveNode]
SolveDRPlan[node_DRNode, o:OptionsPattern[]] := Block[
    {
        curSolutions, droppedSolutions, flipsToSolve
    },

    flipsToSolve = If[OptionValue["Reevaluate"] === "NextFlip",
        Power[2, node["TwoTreeVertexCount"]],
        1
    ];

    If[!AssociationQ[node["FlipSolutions"]],
        node["FlipSolutions"] = <||>;
        node["DroppedFlipSolutions"] = <||>
    ];
    Table[
        Print[StringTemplate["Solving Two-tree flip: `1` / `2`"][flip, flipsToSolve]];
        curSolutions = (
            SolveNode[node, All, o]
            // Map[ToPlanSolution[node, #]&]
        );
        droppedSolutions = curSolutions // Select[(Realize[node, #] // ComputeFlipVector) =!= Part[#, 2]&];
        curSolutions = curSolutions // Select[(Realize[node, #] // ComputeFlipVector) === Part[#, 2]&];
        If[Length[curSolutions] != 0,
            Print[StringTemplate["`1` solutions found for DR-Plan at flip: `2`"][Length[curSolutions], ToString[GetFlip[node]]]];
        ];
        node["DroppedFlipSolutions"] = Append[node["DroppedFlipSolutions"], GetFlip[node] -> droppedSolutions];
        node["FlipSolutions"] = Append[node["FlipSolutions"], GetFlip[node] -> curSolutions],
        {flip, flipsToSolve}
    ];
]

ToPlanSolution[node_DRNode, nodeSolution_NodeSolution] := (
    PlanSolution[
        (#[{}]&) /@ Part[nodeSolution, 1],
        GetFlip[node, Part[nodeSolution, 4]],
        Part[nodeSolution, 3]
    ]
)


(* $NodeSolutionCallback should fall back to no-op *)
Unset[$NodeSolutionCallback] ^:= ($NodeSolutionCallback[sols_, _DRNode] := sols;)
Unset[$NodeSolutionCallback]


$SowSampleList = False


(*
    Options:
    - "Reevaluate":
        - False: use cached;
        - True: re-evaluate current node;
        - All: re-evaluate current and all sub-nodes;
        - "NextFlip": try the next two tree filps, if all tried trigger its sub nodes.
    - "AllCFlip":
        - False: only solve for dropped flips;
        - True: solve both dropped and cayley flips;
    - "SowSampleList":
        - False: do not collect sample points for visualization usage, for acceleration purpose;
        - True: the sample data are collected to $sampleLists;
    - "Parallelize":
        - False: do not leverage multiple cores;
        - True: leverage multiple cores for parallel computing;
    - "Overflip":
        - False: Sampling within current flip.
        - True: Take a few sample points on another flip to smooth the boundary.
    Options from SolveDFlip:
    - "Method":
        - "UniformSampling": use uniform sampling steps.
        - "KdTree": sampling by partitioning the space into k-d trees.
*)
Options[SolveNode] := {
    "Reevaluate" -> False,
    "AllCFlip" -> False,
    "SowSampleList" -> $SowSampleList,
    "Parallelize" -> False,
    "Overflip" -> False
} ~Join~ Options[SolveDFlip]

SolveNode::invtdf = "Invalid D-flip specified: `1`."
SolveNode[node_DRNode, o:OptionsPattern[]] := SolveNode[node, All, o]
(*
    Caveat: If you specified the D-flip, be careful that the index running among all the D-flips and (if applicable) C-flips.
    For example, let's say there are solutions in D-flip {1, {1}, {1}} and {1, {2}, {1}}.
    If you now specified D-flip to be {2, All, All}, it will return {1, {2}, {1}} because it is the second D-flip of the two above.
*)
SolveNode[node_DRNode, dFlip:(All | _List), o:OptionsPattern[]] := Module[
    {
        cayleyVertex, rootgraph,
        nodeSolutions, curDFlip, subDFlips,
        nodeI, solutions,
        (* options *)
        reevaluate, allCFlip, sowSampleList, parallelize,
        subReevaluate, overflip
    },

    {reevaluate, allCFlip, sowSampleList, parallelize, overflip} =
        OptionValue[SolveNode, {o}, {"Reevaluate", "AllCFlip", "SowSampleList", "Parallelize", "Overflip"}];

    (* Print["Solving " <> ToString[node]]; *)
    If[node["IsCayleyNode"],
        rootgraph = node["Root"]["Graph"];
        cayleyVertex = Max @@ (Part[
            EdgeList[rootgraph],
            node["TargetCayley"]
        ]);
        NodeSolution[
            <|node["TargetCayley"] -> ((node["FreeCayley"] // Map[Key]) /* Through /* First)|>, (* identity function *)
            <|node["TargetCayley"] -> node["Interval"] + If[overflip, (
                node["Interval"]
                // Apply[EuclideanDistance]
                // (# * $BoundaryRatio * {-1, 1})&
            ), 0]|>, (* domain *)
            {1}, (* D-flip index *)
            <||> (* C-flip*)
        ] // {
            Identity,
            If[allCFlip,
                ReplacePart[-1 -> <|cayleyVertex -> !PropertyValue[{rootgraph, cayleyVertex}, "Flip"]|>],
                Nothing
            ]
        } // Through
        // UnEcho[#, "nodeSolutions", Map[ToString]]&,

        (* Memoization *)
        If[reevaluate === False && !MissingQ[node["Solutions"]],
            Return[node["Solutions"]]
        ];

        If[reevaluate === "NextFlip",
            SetTwoTreeFlipIndex[node]
        ];

        subReevaluate = reevaluate // Replace[{
            All -> All,
            "NextFlip" -> (
                If[node["TwoTreeFlipIndex"] == 0,
                    "NextFlip",
                    False
                ]
            ),
            _ -> False
        }]; (* Unless All, do not re-evaluate the sub-nodes *)

        {curDFlip, subDFlips} = Replace[dFlip, {
            (* solve for all D-flips *)
            All :> {All, Table[All, Length[node["SubNodes"]]]},
            (* solve for the specified D-flips for current node, based on specifed sub-D-flips *)
            (* if not enough sub-D-flips are specified, the remainings will based on all sub-D-flips *)
            {cur_, subs:(All|_List)...} :> {{cur}, PadRight[{subs}, Length[node["SubNodes"]], All]},
            (* otherwise, invalid D-flips specifed *)
            _ :> (Message[SolveNode::invdf, dFlip]; Abort[])
        }];

        SetPlanShortestEdge[node];

        nodeSolutions = mergeNodeSolution @@ MapThread[SolveNode[#1, #2,
            "Reevaluate" -> subReevaluate,
            FilterRules[{o}, Options[SolveNode]|Except["Reevaluate"]]
        ]&, {node["SubNodes"], subDFlips}];

        (* Prepare Immutable data for parallelism*)
        nodeI = PersistDRNode[node];

        $SowSampleList = sowSampleList;

        (* Solve a flip *)
        {solutions, $sampleLists} = If[parallelize,
            ParallelTable[
                Reap[SolveDFlip[nodeI, nodeSolution, FilterRules[{o}, Options[SolveDFlip]]], "SampleList"],
                {nodeSolution, nodeSolutions},
                DistributedContexts -> {"DRPLAN`Core`", "DRPLAN`Solver`", "DRPLAN`Utility`", "DRPLAN`Thread`", "DataType`"},
                Method -> "FinestGrained"
            ],
            Table[
                Reap[SolveDFlip[nodeI, nodeSolution, FilterRules[{o}, Options[SolveDFlip]]], "SampleList"],
                {nodeSolution, nodeSolutions}
            ]
        ] // Replace[{
            {} | $Aborted -> {{}, {}},
            solList_ :> Transpose[solList]
        }];
        $sampleLists = Flatten[$sampleLists, 1];

        (* Memoization *)
        node["Solutions"] = $NodeSolutionCallback[Part[Flatten[solutions], curDFlip], node];
        PrintProgress[node];
        node["Solutions"]
    ]
]


(*
    Finds the shortest edge of the DR-Plan and sets it as a field. This is
    used to decide the number of samples according to different length of the
    edge.
*)
SetPlanShortestEdge[node_DRNode] := With[
    {
        root = node["Root"],
        rootgraph = node["Root"]["Graph"]
    },

    If[!NumericQ[node["Root"]["PlanShortestEdge"]],
        root["PlanShortestEdge"] = Table[
            If[PropertyValue[{rootgraph, edge}, "EdgeType"] == "Add",
                Nothing,
                PropertyValue[{rootgraph, edge}, EdgeWeight]
                // Replace[_?(EqualTo[0]) -> Nothing]
            ], {edge, EdgeList[rootgraph]}] // Min
    ]
]


(*
    Set the two-tree flip to next one. If all combinations of two-tree flips
    in current node are tried, reset to zero and go deep to sub nodes.
*)
SetTwoTreeFlipIndex[node_DRNode] := (

    If[!NumericQ[node["TwoTreeFlipIndex"]],
        node["TwoTreeFlipIndex"] = 0;
        Return[]
    ];

    node["TwoTreeFlipIndex"] += 1;
    If[node["TwoTreeFlipIndex"] >= Power[2, Length[node["TwoTreeVertices"]]],
        node["TwoTreeFlipIndex"] = 0;
        If[Length[node["TwoTreeVertices"]] > 0,
            Print[StringTemplate["Flipping vertices: `1` at `2`"][{Last[node["TwoTreeVertices"]]}, node]];
            FlipAt[node["Root"], {Last[node["TwoTreeVertices"]]}]
        ],
        Print[StringTemplate["Flipping vertices: `1` at `2`"][
            {Part[node["TwoTreeVertices"], IntegerExponent[2 * node["TwoTreeFlipIndex"], 2]]}, node]];
        FlipAt[node["Root"], {Part[node["TwoTreeVertices"], IntegerExponent[2 * node["TwoTreeFlipIndex"], 2]]}]
    ];
)

mergeNodeSolution::diftf = "Different T-Flips are specified for the same cayley edge."
mergeNodeSolution[nodeSolutions:PatternSequence[{___NodeSolution}..]] :=
    Outer[mergeNodeSolution, nodeSolutions] // Flatten
mergeNodeSolution[nodeSolutions__NodeSolution] := With[
    {
        nodeSolutionList = List @@@ {nodeSolutions}
    },

    NodeSolution[
        Part[nodeSolutionList, All, 1]
        // Merge[First],
        Part[nodeSolutionList, All, 2]
        // Merge[Apply[RangeIntersection]],
        Part[nodeSolutionList, All, 3]
        // Prepend[Missing["DFlipNotSolved"]],
        Part[nodeSolutionList, All, 4]
        // Merge[If[SameQ@@#, First[#], Message[mergeNodeSolution::diftf]; Abort[]]&]
    ]
]


(* ::Section:: *)
(*Solve Flip*)


(* ::Subsection:: *)
(*CalcCoords*)


calcCoords::nosol = "The graph is not realizable."
calcCoords::negdel = "The determinant `1` is negative at vertex `2` with cayleylength `3`."
calcCoords::ntwotr = "The graph is not a two tree because there are more than two base vertices `2` connected to `1`."
calcCoords::nttedge = "The edge `1` is has \"EdgeType\" `2` which is not included in the two-tree."
calcCoords::cnof = "Cannot over flip edge `1` at length `2` since it is outside `3`."
calcCoords::wrdlen = "Weird length for triangle with edges `1`(`2`), `3`(`4`), `5`(`6`)."
calcCoords[node_DRNode, cayleyLength_Association, flipVector_List] := Block[
    {
        graph = node["Root"]["Graph"],
        vertices = VertexList[node["Graph"]], v1, v2,
        overflipCayley = cayleyLength, overflipVector = flipVector
    },

    cayleyLength
    // KeyValueMap[{edge, length} \[Function] With[
        {
            interval = PropertyValue[{graph, Part[EdgeList[graph], edge]}, "Interval"],
            overflipVertex = PropertyValue[{graph, Part[EdgeList[graph], edge]}, "OverflipVertex"]
        },

        length
        // Replace[{
            _?(LessThan[Min[interval]]) :> (
                overflipVector = If[overflipVector // MemberQ[First[overflipVertex]],
                    Complement[overflipVector, {First[overflipVertex]}],
                    Union[overflipVector, {First[overflipVertex]}]
                ];
                overflipCayley[edge] = 2 * Min[interval] - length
            ),
            _?(GreaterThan[Max[interval]]) :> (
                overflipVector = If[overflipVector // MemberQ[Last[overflipVertex]],
                    Complement[overflipVector, {Last[overflipVertex]}],
                    Union[overflipVector, {Last[overflipVertex]}]
                ];
                overflipCayley[edge] = 2 * Max[interval] - length
            )
        }]
    ]];

    Replace[overflipCayley,
        err: Except[<|(_Integer -> _?NumericQ)...|>] :> (
            Echo[err, "cayleyLength"];
            Abort[]
        )
    ];

    If[Length[vertices] > 2,

        {v1, v2} = Take[vertices, 2];
        {
            Drop[vertices, 2],
            <|
                v1 -> {0, 0},
                v2 -> {PropertyValue[{node["Root"]["Graph"], v1<->v2}, EdgeWeight], 0}
            |>
        } // calcCoordsImpl[node["Root"]["Graph"], overflipCayley, overflipVector],
        <||>
    ]
] // Replace[err:Except[_Association] :> (Echo[err, "calcCoords returns"]; Abort[])]

calcCoordsImpl[rootgraph_Graph, cayleyLength_Association, flipVector_List][
    {{(* no vertices *)}, coordsList:Association[(_Integer -> {_?NumericQ, _?NumericQ})...]}
] := coordsList
calcCoordsImpl[rootgraph_Graph, cayleyLength_Association, flipVector_List][
    {{v0_Integer, restVertices___}, coordsList_Association}
] := Block[
    {
        (* vertices *) v1, v2,
        (* coordinates *) c1, c2,
        (* distances between vertices *) d0, d1, d2,
        (* difference between coordinates *) dx, dy, dd,
        (* middle point of edges *) mx, my, md,
        (* determinant of quadratic equation *) delta,
        sign
    },

    If[Length[coordsList] < 2, Return[{{(* stop recursion *)}, coordsList}]];

    {v1, v2} = AdjacencyList[rootgraph, v0]
    // Select[(# < v0 && PropertyValue[{rootgraph, # <-> v0}, "EdgeType"] != "Drop")&]
    // Replace[err:Except[{_Integer, _Integer}] :> (
        Message[calcCoords::ntwotr, v0, err];
        Abort[]
    )] // Sort;

    {c1, c2} = (coordsList /@ {v1, v2})
    // Replace[err:Except[{{_?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}] :> (
        Echo[{v0, v1, v2}, "Vertices"];
        Echo[err, "Coordinates"];
        Echo[coordsList];
        Abort[]
    )];

    {d0, d1, d2} = Table[
        PropertyValue[{rootgraph, e}, "EdgeType"]
        // Replace[{
            "Add" :> With[
                {
                    sampleLength = cayleyLength[EdgeIndex[rootgraph, e]],
                    interval = AnnotationValue[{rootgraph, e}, "Interval"]
                },

                sampleLength
                // Replace[_?(Between[interval]/*Not) :> (
                    Echo[{v0, v1, v2}, "Vertices"];
                    Echo[flipVector, "FlipVector"];
                    Echo[cayleyLength, "CaleyLength"];
                    Echo[coordsList];
                    Message[calcCoords::cnof, e, sampleLength, interval];
                    Abort[]
                )]
            ],
            "Partial" :> (PropertyValue[{rootgraph, e}, EdgeWeight]),
            err_ :> (Message[calcCoords::nttedge, e, err]; Abort[])
        }],
        {e, {v1 <-> v2, v0 <-> v1, v0 <-> v2}}
    ];

    {dx, dy} = c1 - c2;
    {mx, my} = (c1 + c2) / 2;
    dd = d1 - d2;
    md = (d1 + d2) / 2;

    delta = Max[((d0 - dd) * (md - d0 / 2) * (d0 + dd) * (md + d0 / 2)), 0]
    // Replace[err:Except[_?NumericQ] :> (
        Print["delta: ", err, {d1, d2}, cayleyLength];
        Abort[]
    )];

    If[delta < 0,
        Message[calcCoords::negdel, delta, v0, cayleyLength];
        (* Echo[cayleyLength, "CayleyLength"]; *)
        (* Echo[t`$rd, "Refined Domain"];
        Echo[d0, "d0"];
        Echo[dd, "dd"];
        Echo[md, "md"]; *)
        (* Abort[]; *)
        (* Return["Unrealizable"]; *)
        {{(* stop recursion *)}, coordsList},

        sign = If[MemberQ[flipVector, v0], 1, -1];
        {
            {restVertices},
            Append[
                coordsList,
                v0 -> Replace[(- dd * md * {dx, dy} + {-1, 1} * sign * {dy, dx} * Sqrt[delta]) / d0^2 + {mx, my}, {
                    err:Except[{_?NumericQ, _?NumericQ}] :> (
                        Echo[d0, "d0"];
                        Echo[dd, "dd"];
                        Echo[md, "md"];
                        Echo[{v0, v1, v2}, "Vertices"];
                        Echo[err, "Coordinates"];
                        Echo[flipVector, "FlipVector"];
                        Echo[cayleyLength, "CaleyLength"];
                        Echo[coordsList, "CoordsList"];
                        Echo[err, v0];
                        Abort[]
                    ),
                    err_ /; (Chop[EuclideanDistance[err, c1] - d1] != 0 ||
                        Chop[EuclideanDistance[err, c2] - d2] != 0) :> (
                        Echo[d0, "d0"];
                        Echo[dd, "dd"];
                        Echo[md, "md"];
                        Echo[delta, "delta"];
                        Echo[{v0, v1, v2}, "Vertices"];
                        Echo[err, "Coordinates"];
                        Echo[flipVector, "FlipVector"];
                        Echo[cayleyLength, "CaleyLength"];
                        Echo[coordsList, "CoordsList"];
                        Echo[err, v0];
                        Abort[]
                    )
                }]
            ]
        }

    ]

] // Replace[Return[val_] :> val] // calcCoordsImpl[rootgraph, cayleyLength, flipVector]


dropLength[node_DRNode] := With[
	{
		rootgraph = node["Root"]["Graph"]
	},

	PropertyValue[{rootgraph, Part[EdgeList[rootgraph], node["TargetDrop"]]}, EdgeWeight]

]

dropDiff[node_DRNode, graph_Graph] := Module[
    {
        rootgraph = node["Root"]["Graph"], dropEdge
    },

    dropEdge = EdgeList[rootgraph][[node["TargetDrop"]]];
    EuclideanDistance[
        PropertyValue[{graph, First[dropEdge]}, VertexCoordinates],
        PropertyValue[{graph, Last[dropEdge]}, VertexCoordinates]
    ] - dropLength[node]
]

dropDiff[node_DRNode, coordinates_Association, dropOffset:_?NumericQ:1] := (
    Part[EdgeList[node["Root"]["Graph"]], node["TargetDrop"]]
    // Replace[UndirectedEdge[v1_, v2_] :>
        EuclideanDistance[coordinates[v1], coordinates[v2]]
        - dropLength[node] * dropOffset
    ]
)


(* ::Subsection:: *)
(*Parameters*)


$SampleDivisor = 2^17 (* the minimal distance between samples should be <EdgeLength>/$SampleDivisor *)
$SampleNum = 36 (* estimated number of samples for the shortest edge if it is uniform sparse sampling. *)
$BoundaryRatio = 0.05 (* the width of the boundary outline on both sides of the interval that require dense sampling *)
$RefineSampling = True (* on for refinsampling, off for uniform sparse sampling *)
$DenseMultipler = 3 (* the ratio of dense sampling to sparse sampling *)
GetSparseSampleDistance[sampleNum_] := Max[Round[$SampleDivisor / sampleNum], 1] (* the distance between two sparse samples *)
GetDenseSampleDistance[sampleNum_] := Max[Round[$SampleDivisor / sampleNum / $DenseMultipler], 1] (* the distance between two dense samples *)
$DenseSampleDistance = GetDenseSampleDistance[$SampleNum]
$LeftBoundaryEnd = Max[Round[$SampleDivisor * $BoundaryRatio], 1]
$OverflipLeftBoundaryEnd = Max[Round[$SampleDivisor / (1 / (2 * $BoundaryRatio) + 1)], 1]
$RightBoundaryStart = Max[$SampleDivisor - Round[$SampleDivisor * $BoundaryRatio], 1]
$OverflipRightBoundaryStart = Max[$SampleDivisor - Round[$SampleDivisor / (1 / (2 * $BoundaryRatio) + 1)], 1]
$ResampleRatio = 0.15
$ZeroRatio = 0.01


(* return a list of sample indices from 0 to $SampleDivisor *)
getSamples[{left_, right_}, planShortestEdge_?NumericQ, overflip_?BooleanQ] := Module[
    {
        leftEnd = If[overflip, $OverflipLeftBoundaryEnd, $LeftBoundaryEnd],
        rightStart = If[overflip, $OverflipRightBoundaryStart, $RightBoundaryStart],
        sampleNum,
        sampleIndices
    },

    (* The wider the interval is, the more samples we take. *)
    sampleNum = Max[Ceiling[(right - left) / planShortestEdge * $SampleNum], $SampleNum];

    sampleIndices = If[$RefineSampling,
        Join[
            Range[0, leftEnd, GetDenseSampleDistance[sampleNum]], (* Left End Point & Left Boundary*)
            Range[leftEnd, rightStart, GetSparseSampleDistance[sampleNum]], (* Center *)
            Range[rightStart, $SampleDivisor, GetDenseSampleDistance[sampleNum]], (*Right Boundary*)
            {$SampleDivisor} (* Right End Point*)
        ],
        Join[
            Range[0, $SampleDivisor, GetSparseSampleDistance[sampleNum]],
            {$SampleDivisor} (* Right End Point*)
        ]
    ] // DeleteDuplicates;

    {
        sampleNum,
        SparseArray[(sampleIndices + 1) -> left + (right - left) * sampleIndices / $SampleDivisor,
            $SampleDivisor + 1, Missing["NotSampled"]
        ]
    }
]


(* ::Subsection:: *)
(*refineInterval*)


refineInterval[node_DRNode, targetCayley_, cayleyLength_Association] := Module[
    {
        graph = node["Graph"], rootgraph = node["Root"]["Graph"], targetedge,
        v1, v2, commonvertex, d1, d2
    },

    targetedge = EdgeList[rootgraph][[targetCayley]];
    {v1, v2} = List @@ targetedge;

    commonvertex = Min[AdjacencyList[graph, v1] ~Intersection~ AdjacencyList[graph, v2]];
    {d1, d2} = Function[{edge},
        If[PropertyValue[{rootgraph, edge}, "EdgeType"] == "Add",
           cayleyLength[EdgeIndex[rootgraph, edge]] // Replace[_Missing :> (Echo@cayleyLength; Abort[])],
           PropertyValue[{rootgraph, edge}, EdgeWeight]
        ]
    ] /@ {UndirectedEdge[commonvertex, v1], UndirectedEdge[commonvertex, v2]};

    {Abs[d1 - d2], d1 + d2}
]


(* ::Subsection:: *)
(*Solve D-Flip*)


Options[SolveDFlip] := {
    "Method" -> "UniformSampling" (* "KdTree" *),
    "Overflip" -> False
} ~Join~ Options[UniformSampling]


SolveDFlip[args__, o:OptionsPattern[]] := Block[
    {
        method, overflip
    },

    {method, overflip} = OptionValue[SolveDFlip, {o}, {"Method", "Overflip"}];

    If[method == "UniformSampling",
        UniformSampling[args, overflip],
        KdTreeSampling[args]
    ]
]


realizeNode[node_DRNode, solution_Association, tFlip_Association, sample_Association] := With[
    {
        cayleyLength = ((#[sample]&) /@ solution)
        // Replace[
            err: Except[<|(_Integer -> _?NumericQ)...|>] :> (
                Echo[err, "cayleyLength"];
                Echo[sample, "sample"];
                Echo[solution, "solution"];
                Abort[]
            )
        ]
    },

    Check[
        calcCoords[node, cayleyLength, GetFlip[node, tFlip]],
        $Failed
    ]
]


(* Only works for flex-1 *)
interpZeros[node_DRNode, nodeSolution_NodeSolution, samples_, sampleList:{(_?NumericQ|InterpolationPiece[_?NumericQ]|_Missing)..}, index_Integer] := Block[
    {
        solution, dflip, cflip,
        samplePoints, splitPos, zeroFunc, targetRule,
        newSolution, newDomain, newDFlip
    },

    solution = Part[nodeSolution, 1];
    dflip =  Part[nodeSolution, 3];
    cflip = Part[nodeSolution, 4];
    zeroFunc = If[Length[node["FreeCayley"]] > 0,
        (* Ci vs C1 *)
        samplePoints = DeleteMissing[Transpose[{samples, sampleList}], 1, 1];
        splitPos = {1} ~Join~ Flatten[Position[samplePoints, {_, _InterpolationPiece}, {1}]] ~Join~ {-1};
        Table[
            ClearAll[x];
            Replace[Length[interpList], {
                0|1 -> Nothing,
                _ :> Interpolation[interpList, InterpolationOrder -> 1, Method -> "Hermite"]
            }], {interpList, BlockMap[Take[Replace[samplePoints, InterpolationPiece[p_] :> p, {2}], #]&, splitPos, 2 ,1]}
        ]
        // Replace[{} :> Return[{}]]
        // Apply[InterpolatingFunctionGroup],
        (* the last Cayley C1 *)
        Function[{const}, (const &)] @@ sampleList
    ];
    (*Print[zeroFunc[1]];*)

    targetRule = ((node["TargetCayley"] // Key) -> (
        (node["FreeCayley"] // Map[Key]) /* Through /* Apply[zeroFunc]
    ));

    newSolution = (solution /. targetRule);
    newDomain = AssociationThread[node["FreeCayley"], zeroFunc["Domain"]];
    newDFlip = ReplacePart[dflip, 1 -> index];
    NodeSolution[newSolution, newDomain, newDFlip, cflip]
]


AlternativeInterpolation[list_List] := Block[
    {
        first, last, domain, midOdd, midEven, interpOdd, interpEven,
        x
    },

    first = First[list];
    last = Last[list];
    domain = Transpose[Most/@{first, last}];

    If[Length[list] < 6; Null,
        Interpolation[list, InterpolationOrder -> 3, Method -> "Hermite"],

        {midOdd, midEven} = (
            Partition[Take[list, {2, -2}], UpTo[2]]
            // {
                Map[First],
                Map[Rest] /* Catenate
            } // Through
        );

        interpOdd = Interpolation[Join[{first}, midOdd, {last}], InterpolationOrder -> 3, Method -> "Hermite"];
        interpEven = Interpolation[Join[{first}, midEven, {last}], InterpolationOrder -> 3, Method -> "Hermite"];

        x = Table[Unique["x"], Length[domain]];
        MapThread[Prepend, {domain, x}]
        // Prepend[(interpOdd@@x + interpEven@@x) / 2]
        // Apply[FunctionInterpolation[#1, ##2]&]

        (* InterpolatingFunctionGroup[interpOdd, interpEven] *),

        Interpolation[list, InterpolationOrder -> 1, Method -> "Hermite"]
    ]
]


SetAttributes[InterpolatingFunctionGroup, Flat]
InterpolatingFunctionGroup::outdom = "The input `1` is of out the domain of the interpolating functions `2`. Indterminate will be returned."
InterpolatingFunctionGroup[ifs__InterpolatingFunction][x:(_?NumericQ | {__?NumericQ})] := (
    Table[
        Quiet[Check[if[x], Nothing, {InterpolatingFunction::dmval}], {InterpolatingFunction::dmval}],
        {if, {ifs}}
    ] // Replace[{} :> {
        Message[InterpolatingFunctionGroup::outdom, x, {ifs}];
        Indeterminate
    }] // Mean
)
InterpolatingFunctionGroup[ifs__InterpolatingFunction]["Domain"] := (
    Transpose[Through[{ifs}["Domain"]]]
    // Map[Apply[RangeUnion]]
)


(* ::Subsubsection:: *)
(*Uniform Sampling*)

UniformSampling::dupz = "`1` zeros are found.";
UniformSampling::noz = "no zeros are found.";
UniformSampling::nosolplan = "no solution for the dr-plan.";
(* This function solves the given dropped flip. *)
UniformSampling[node_DRNode, nodeSolution_NodeSolution, overflip_?BooleanQ, dropOffset:_?NumericQ:1] := Module[
    {
        domain, sampleNum,
        firstSamples, firstResults,
        nearRatio = 0.30, nearZerosIntervals,
        refinedFreeSamples, refinedResults,
        finalSamples, finalResults
	},

    domain = Part[nodeSolution, 2];

    (* generate sample points for free cayleys *)
    {sampleNum, firstSamples} = If[node["FreeCayley"] =!= {},
        (* only handles flex-1 case *)
        KeyTake[domain, First[node["FreeCayley"]]]
        // Map[getSamples[#, node["Root"]["PlanShortestEdge"], overflip]&]
        // First,
        Echo["Last Cayley"];
        (* $on = True; *)
        {0, <||>}
    ];
    (* Echo["Generated " <> ToString[sampleNum] <> " samples for " <> ToString[node["FreeCayley"]]]; *)

    (* $on = False; *)
    (* If[$on, Echo[firstSamples]]; *)
    (* Echo[node["FreeCayley"]]; *)
    firstResults = If[node["FreeCayley"] === {},
        {Tuple[scanSamples[node, nodeSolution, overflip, dropOffset][<||>]]},
        (* firstSamples is a sparse array, should not use Replace[..., {1}] *)
        firstSamples // Map[Replace[freeSample:Except[_Missing] :> (
            Tuple[scanSamples[node, nodeSolution, overflip, dropOffset][
                <|First[node["FreeCayley"]] -> freeSample|>]
            ]
        )]]
    ];
    (* If[$on, Echo[firstResults]]; *)
    (* Echo[firstResults]; *)
    If[node["FreeCayley"] === {},
        finalSamples = firstSamples;
        finalResults = firstResults,
        nearZerosIntervals = findNearZerosIntervals[firstResults, domain[First[node["FreeCayley"]]], nearRatio];
        refinedFreeSamples = getDenseSamples[firstSamples, nearZerosIntervals, sampleNum];
        (* If[nearZerosIntervals =!= {}, Echo[refinedFreeSamples]];
        Abort[]; *)

        (* $on = True; *)
        refinedResults = (Replace[freeSample:Except[_Missing] :> (
            Tuple[scanSamples[node, nodeSolution, overflip, dropOffset][
                <|First[node["FreeCayley"]] -> freeSample|>]
            ]
        )] /@ refinedFreeSamples);

        finalSamples = UnEcho@SparseArray[
            Most[ArrayRules[firstSamples]] ~Join~ Most[ArrayRules[refinedFreeSamples]],
            $SampleDivisor + 1, Missing["NotSampled"]
        ];
        finalResults = UnEcho@SparseArray[
            Most[ArrayRules[firstResults]] ~Join~ Most[ArrayRules[refinedResults]],
            $SampleDivisor + 1, Missing["NotSampled"]
        ]
    ];

    (*Print[sampleLists];*)
    (* interpZeros[firstSamples, #]& /@ Transpose[firstResults] *)
    (* Echo@ThreadZeros[Identity@@@Select[finalResults, Not@*MissingQ]]; *)
    (* Abort[]; *)
    MapIndexed[
        Check[
            interpZeros[node, nodeSolution, Select[finalSamples, Not@*MissingQ], #1, First[#2]],
            Nothing,
            {Interpolation::inddp}
        ]&,
        (* see Thread.wl *)
        ThreadZeros[Identity@@@Select[finalResults, Not@*MissingQ], domain[node["TargetCayley"]]]
    ]

]


scanSamples[node_DRNode, nodeSolution_NodeSolution, overflip_?BooleanQ, dropOffset:_?NumericQ:1][
        freeSample_Association] := Module[
    {
        solution, domain, tFlip, sampleNum,
        targetDomain, targetSamples,
        sampleList, approxIntervals, targetRefinedSamples, refinedSampleList,
        threshold, zeroThreshold, zeroIntervals, approxZeros, interp, interpd, tmpZeros
    },

    solution = Part[nodeSolution, 1];
    domain = Part[nodeSolution, 2];
    tFlip = Part[nodeSolution, 4];
    targetDomain = domain[node["TargetCayley"]];

    If[EuclideanDistance@@targetDomain <= ($MachineEpsilon * $SampleDivisor),
        Echo[freeSample,"Empty Refined Interval"]; Return[{}]
    ];

    {sampleNum, targetSamples} = getSamples[targetDomain, node["Root"]["PlanShortestEdge"], overflip];
    sampleList = (Replace[targetSample:Except[_Missing] :> (
        realizeNode[node, solution, tFlip,
            Append[freeSample, node["TargetCayley"] -> targetSample]
        ]
        // Replace[{
            $Failed :> (
                (*Echo[targetSample]; AbortNow = True;*)
                Missing["NoSolution"]
            ),
            coordinates_Association :> Pair[
                targetSample,
                dropDiff[node, coordinates, dropOffset]
            ],
            err_ :> (Echo[err, "Unknown Result"]; Abort[])
        }]
    )] /@ targetSamples);

    (* If[TrueQ[AbortNow], Echo[sampleList]; Abort[]]; *)

    threshold = $ResampleRatio * dropLength[node];
    approxIntervals = findApproxIntervals[sampleList, threshold];
    targetRefinedSamples = getDenseSamples[targetSamples, approxIntervals, sampleNum];

    refinedSampleList = (Replace[targetSample:Except[_Missing] :> (
        realizeNode[node, solution, tFlip,
            Append[freeSample, node["TargetCayley"] -> targetSample]
        ]
        // Replace[{
            $Failed :> (
                (*Echo[targetSample]; AbortNow = True;*)
                Missing["NoSolution"]
            ),
            coordinates_Association :> Pair[
                targetSample,
                dropDiff[node, coordinates, dropOffset]
            ],
            err_ :> (Echo[err, "Unknown Result"]; Abort[])
        }]
    )] /@ targetRefinedSamples);

    sampleList = SparseArray[
        Most[ArrayRules[sampleList]] ~Join~ Most[ArrayRules[refinedSampleList]],
        $SampleDivisor + 1, Missing["NotSampled"]
    ];

    (* If[Length[ArrayRules[refinedSampleList]] > 1, Echo@sampleList];
    Return[] *)

    With[
        {
            interpSampleList = List@@@Select[sampleList, Not@*MissingQ] (* DeleteMissing does not work for SparseArray *)
        },

        If[Length[interpSampleList] > 0,
            Check[
                interp = Interpolation[interpSampleList, InterpolationOrder -> 3, Method -> "Spline"];
                interpd = interp';
                tmpZeros = (findZeros[interp, interpSampleList])
                // Replace[_findZeros :> (
                    (* wrong type *)
                    Echo[sampleList];
                    Abort[]
                )],
                Echo[interpSampleList, "interpSampleList"];
                Echo[interp, "interp"];
                Echo[tmpZeros, "tmpZeros"]
            ],
            tmpZeros = {}
        ]
    ];

    If[Length[node["FreeCayley"]] == 0,
        (* Echo[sampleList, "SampleList"]; *)
        (* Echo[tmpZeros, "tmpZeros"]; *)
        If[Length[tmpZeros] == 0,
            Message[DRNode::nosolplan]
            (*, Message[DRNode::noz]*)
        ]
    ];

    zeroThreshold = $ZeroRatio * dropLength[node];
    zeroIntervals = findApproxIntervals[sampleList, zeroThreshold];
    (* zeroIntervals = {}; *)
    approxZeros = DeleteDuplicates[Join[
        getApproxZeros[tmpZeros, sampleList, zeroIntervals, GetDenseSampleDistance[sampleNum] / $SampleDivisor],
        getBoundaryApproxZeros[sampleList, zeroIntervals]
    ]];
    (* If[Length[approxZeros] > 0,
        Echo[Length[approxZeros], "Num of approximated zeros"];
    ]; *)

    tmpZeros = Sort[Join[tmpZeros, List@@@Part[sampleList, approxZeros, 1]]];

    If[$SowSampleList,
        Rule[
            sampleList // ArrayRules // Most
            // Part[#, All, -1]&
            (* Unwraps Pair *)
            // Map[
                Join[
                    {First[node["FreeCayley"], Nothing]}
                    // Map[freeSample],
                    # // Apply[List]
                ]&
            ],
            {First[node["FreeCayley"], Nothing]}
            // Map[freeSample]
            // (tmpZeros // Map[Append])
            // Through
        ]
        // Sow[#, "SampleList"]&
    ];

    tmpZeros // Map[{Identity, interpd} /* Through]
]


findApproxIntervals[samplePoints_SparseArray, threshold_?NumericQ] := Module[
    {
        booleanList, seqPos
    },

    booleanList = samplePoints // ArrayRules // Most
    // Cases[({pos_} -> Pair[x_, y_]) :> {pos, Abs[y] <= threshold}]
    // SortBy[First];

    seqPos = SequencePosition[booleanList, {{_, True}..}, Overlaps -> False];

    (Part[booleanList, #, 1]&) /@ seqPos

]


findNearZerosIntervals[zeroTuples_SparseArray, {min_, max_}, threshold_?NumericQ] := Module[
    {
        booleanList, seqPos
    },

    booleanList = zeroTuples // ArrayRules // Most
    // Cases[({pos_} -> Tuple[zeroTuple_List]) :> {
        pos,
        zeroTuple
        // Replace[{
            {} :> False,
            _List :> (
                Join[{min}, Part[zeroTuple, All, 1], {max}]
                // Differences
                // Select[LessEqualThan[threshold * (max - min)]]
                // Length // Replace[{
                    0 :> False,
                    _ :> True
                }]
            )
        }]
    }]
    // SortBy[First];

    seqPos = SequencePosition[booleanList, {{_, True}..}, Overlaps -> False];

    (Part[booleanList, #, 1]&) /@ seqPos

]


getDenseSamples::misint = "Either first or last element is missing for the first argument"
getDenseSamples[targetSamples_SparseArray, intervals:{{_Integer, _Integer}...}, sampleNum_Integer] := Module[
    {
        sampleIndices, left, right
    },

    left = First[targetSamples] // Replace[_Missing :> (Message[getDenseSamples::misint]; Abort[])];
    right = Last[targetSamples] // Replace[_Missing :> (Message[getDenseSamples::misint]; Abort[])];

    sampleIndices = Complement[
        Join[Apply[{start, end} \[Function] Range[
            Max[start - GetSparseSampleDistance[sampleNum], $LeftBoundaryEnd],
            Min[end + GetSparseSampleDistance[sampleNum], $RightBoundaryStart],
            GetDenseSampleDistance[sampleNum]
        ]] /@ intervals],
        targetSamples // ArrayRules // Most
        // Cases[({pos_} -> val_) :> pos - 1] (* the index is off-by-one*)
    ];
    SparseArray[
        Thread[(sampleIndices + 1) -> left + (right - left) * sampleIndices / $SampleDivisor],
        $SampleDivisor + 1, Missing["NotSampled"]
    ]
]


(* Find zeros in a interpolating function *)
findZeros[interp_InterpolatingFunction, samplelist:{{_?NumericQ, _?NumericQ}..}] := Module[
    {
        domain, knots, controlpoints, polyform, t, zeros, zerodomain
    },

    {domain} = interp["Domain"];
    FirstCase[interp, _BSplineFunction, Missing["NotBSpline"], Infinity]
    // Replace[{
        _?MissingQ -> {},
        bsp_ :> (
            {knots} = bsp["Knots"];
            controlpoints = bsp["ControlPoints"];
            (*zerodomain= Transpose[{Most[knots[[3;;-3]]],Rest[knots[[3;;-3]]]}][[Flatten[Position[Most[controlpoints]*Rest[controlpoints],_?NonPositive]]]];*)
            zerodomain = Part[
                Transpose[{Most[samplelist[[All,1]]], Rest[samplelist[[All,1]]]}],
                Flatten[Position[Most[samplelist[[All,2]]] * Rest[samplelist[[All,2]]], _?NonPositive]]
            ];

            polyform = PiecewiseExpand[Sum[
                controlpoints[[i + 1]] * PiecewiseExpand[BSplineBasis[{Min[Length[knots] - 2, 3], knots}, i, t]],
                {i, 0, Length[controlpoints] - 1}
            ]];
            polyform = PiecewiseExpand[Piecewise[{{polyform,(Or@@((#[[1]] <= t <= #[[2]])&/@zerodomain))}},1]];
            zeros = Flatten[Solve[polyform == 0, {t}, Reals]];
            (t/.#&) /@ zeros
        )
    }]
]


getApproxZeros[trueZeros_List, sampleList_SparseArray, zeroMinima_List, toleranceRatio_?NumericQ] := Module[
    {umZeros, umMinima},

    {umZeros, umMinima} = getApproxZerosImpl[sampleList, toleranceRatio][{{trueZeros, zeroMinima}, {{},{}}}];
    (* If[Length[umMinima] > 0,
        Echo[(Part[sampleList, -1, 1] - Part[sampleList, 1, 1]) / $SampleDivisor * $DenseSampleDistance, "tolerance"];
        Echo[{umZeros, Normal[Part[sampleList, umMinima, 1]]}]
    ]; *)
    umMinima
]


getApproxZerosImpl[sampleList_SparseArray, toleranceRatio_?NumericQ][{{{}, {}}, unmatches:{{___?NumericQ}, {___Integer}}}] := unmatches
getApproxZerosImpl[sampleList_, toleranceRatio_][{{{}, intervals:{__}}, unmatches_List}] := (* continue as if there is a zero at infinity point *)
    getApproxZerosImpl[sampleList, toleranceRatio][{{{Infinity}, intervals}, unmatches}]
getApproxZerosImpl[sampleList_, toleranceRatio_][{{trueZeros:{__}, {}}, unmatches_List}] := (* continue as if there is an interval at infinity point *)
    getApproxZerosImpl[sampleList, toleranceRatio][{{trueZeros, {Infinity}}, unmatches}]
getApproxZerosImpl[sampleList_, toleranceRatio_][{{trueZeros:{__}, intervals:{__}}, {umZeros_List, umMinima_List}}] := Module[
    {
        tolerance, firstZero, firstInterval, firstMinima
    },

    tolerance = - Subtract @@ MinMax[Part[Select[sampleList, Not@*MissingQ], All, 1]] * toleranceRatio;
    (* tolerance = (Part[sampleList, -1, 1] - Part[sampleList, 1, 1]) / $SampleDivisor * $DenseSampleDistance; *)

    firstZero = First[trueZeros];
    firstInterval = Replace[intervals, {
        {interval:{_Integer, _Integer}, ___} :> Normal[Part[sampleList, interval, 1]],
        {Infinity} :> {Infinity, Infinity}
    }];
    Which[
        TrueQ[firstZero < Min[firstInterval]],
        {
            {Rest[trueZeros], intervals},
            If[Length[umMinima] > 0 && firstZero < Part[sampleList, Last[umMinima], 1] + tolerance,
                (* current Zero is >~ the minimum of the last interval *)
                {umZeros, Most[umMinima]},
                {Append[umZeros, firstZero], umMinima}
            ]
        },
        TrueQ[firstZero > Max[firstInterval]],
        {
            {trueZeros, Rest[intervals]},
            firstMinima = Take[sampleList, First[intervals]]
                // ArrayRules // Most
                // Cases[({pos_} -> Pair[x_, y_]) :> {pos, Abs[y]}]
                // MinimalBy[Last] // First // First
                // (# + Part[intervals, 1, 1] - 1&);
            If[Length[umZeros] > 0 && Last[umZeros] > Part[sampleList, firstMinima, 1] - tolerance,
                (* last Zero is <~ the minimum of the current interval *)
                {Most[umZeros], umMinima},
                {umZeros, Append[umMinima, firstMinima]}
            ]
        },
        True,
        {{Rest[trueZeros], Rest[intervals]}, {umZeros, umMinima}}
    ]
] // getApproxZerosImpl[sampleList, toleranceRatio]


getBoundaryApproxZeros::nep = "Cannot find the neighbor point of the boundary point."
getBoundaryApproxZeros[_, {}] := {}
getBoundaryApproxZeros[sampleList_SparseArray, zeroIntervals:{__}] := (
    {If[Part[zeroIntervals, 1, 1] == 1,
        Part[sampleList, (LengthWhile[Rest[sampleList], MissingQ] + 2)]
        // Replace[{
            _Missing :> (
                Message[getBoundaryApproxZeros::nep];
                Echo[sampleList];
                Abort[]
            ),
            Pair[x_, y_] :>
                If[(y - Part[sampleList, 1, 2]) * Part[sampleList, 1, 2] > 0,
                    1,
                    Nothing
                ]
        }],
        Nothing
    ],
    If[Part[zeroIntervals, -1, 1] == $SampleDivisor + 1,
        Part[Reverse[sampleList], (LengthWhile[Rest[Reverse[sampleList]], MissingQ] + 2)]
        // Replace[{
            _Missing :> (
                Message[getBoundaryApproxZeros::nep];
                Echo[sampleList];
                Abort[]
            ),
            Pair[x_, y_] :>
                If[(y - Part[sampleList, -1, 2]) * Part[sampleList, -1, 2] > 0,
                    $SampleDivisor + 1,
                    Nothing
                ]
        }],
        Nothing
    ]}
)


(* ::Subsubsection:: *)
(*KdTree Sampling*)


calcSamplePoint[node_DRNode, solution_Association, tFlip_Association,
    (*sample:*)Point[{freeCayley_?NumericQ, targetCayley_?NumericQ, _}]] := (
    realizeNode[node, solution, tFlip, <|
        node["FreeCayley"]
        // Replace[{
            {firstFree_, ___} :> (firstFree -> freeCayley),
            {} -> (Nothing)
        }],
        node["TargetCayley"] -> targetCayley
    |>]
    // Replace[{
        (* $Failed :> (
            (*Echo[targetSample]; AbortNow = True;*)
            Missing["NoSolution"]
        ), *)
        coordinates_Association :> dropDiff[node, coordinates],
        err_ :> (Echo[err, "Unknown Result"]; Abort[])
    }]
    // Point[{freeCayley, targetCayley, #}]&
)


$KdSamplingDivider = 15
$KdDenseDivider = 3


(* Only supports 2d tree (flex-1 case) for now. *)
KdTreeSampling[node_DRNode, nodeSolution_NodeSolution] := Block[
    {
        solution, domain, tFlip, freeDomain, targetDomain
    },

    solution = Part[nodeSolution, 1];
    domain = Part[nodeSolution, 2];
    tFlip = Part[nodeSolution, 4];
    freeDomain = If[node["FreeCayley"] =!= {},
        (* only handles flex-1 case *)
        domain
        // KeyTake[First[node["FreeCayley"]]]
        // First,
        (* Echo["Last Cayley"]; *)
        {0, 0}
    ];
    targetDomain = domain[node["TargetCayley"]];
    {freeDomain, targetDomain}
    // QuadSampling[node, solution, tFlip,
        node["Root"]["PlanShortestEdge"] / $KdSamplingDivider,
        EuclideanDistance@@freeDomain / $KdSamplingDivider,
        EuclideanDistance@@targetDomain / $KdSamplingDivider,
        If[node["FreeCayley"] =!= {},
            node["Root"]["Graph"]
            // AnnotationValue[{#, Part[EdgeList[#], node["FreeCayley"] // (*Flex-1*) First]}, "Interval"]&,
            {}
        ],
        node["Root"]["Graph"]
        // AnnotationValue[{#, Part[EdgeList[#], node["TargetCayley"]]}, "Interval"]&
    ]
    // Last
    // Map[FinalizeSol]
    // If[node["FreeCayley"] =!= {},
        Select[Chop[EuclideanDistance[First[#], Last[#]]] != 0&],
        Identity
    ]
    // MapIndexed[interpZeros[node, nodeSolution, Part[#1, All, 1], Part[#1, All, 2], First[#2]]&]
    (* // Replace[_Rule :> (Echo[{node, nodeSolution}]; Abort[])] *)
]


(*
    An overload of QuadSampling to calculate {ft, fT, Ft, FT} from {{f, F}, {t, T}}.
*)
(quadSampling:QuadSampling[node_DRNode, solution_Association, tFlip_Association, ___])[
        {{f_, F_}, {t_, T_}}] := (
    {{f, F}, {t, T}}
    // Apply[Outer[List/*Point, ##, {Missing["NotSolved"]}]&]
    // Flatten
    // Map[calcSamplePoint[node, solution, tFlip, #]&]
    // quadSampling
)


BoundarySampleQ[boundaries_List][range_] := With[
    {
        (*
            SetDelayed because it is only needed for non-empty boundaries.
        *)
        boundaryLength := (
            boundaries
            // Apply[EuclideanDistance]
            // (# * $BoundaryRatio)&
        )
    },

    boundaries
    // Map[(# + {-1, 1} * boundaryLength)&]
    // Map[RangeIntersection[#, range]& /* Apply[Less]]
    // Apply[Or]
]


$SampleReuses = 0


LookupOrCalcSamplePoint[node_, solution_, tFlip_, cache_][point_Point] := (
    Lookup[cache, Delete[point, {UnwrapPoint, 3}],
        calcSamplePoint[node, solution, tFlip, point],
        ReplacePart[point, {UnwrapPoint, 3} -> ($SampleReuses++;#)]&
    ]
)


SetAttributes[ReapCache, HoldAll]

ReapCache[cacheF_, cacheT_][sowable_] := (
    Reap[{
        sowable,
        (* Side effect should happen after evaluating sowable *)
        cacheF = <||>,
        cacheT = <||>
    } // First, {"CacheF", "CacheT"}, (
        Unevaluated[#1 = #2 // Apply[Association]]
        // ReplaceAt[{"CacheF" :> cacheF, "CacheT" :> cacheT}, 1]
    )&] // First
)


Options[QuadSampling] = {
    "Cache" -> <||>
}

(*
    fT -- cT -- FT
    |     ||     |
    |     ||     |
    fc == cc == Fc  targetCayley
    |     ||     |
    |     ||     |
    ft -- ct -- Ft
      freeCayley
*)
(quadSampling:QuadSampling[node_DRNode, solution_Association, tFlip_Association,
    dropDiffTolerance_?NumericQ, freeDistanceTolerance_?NumericQ, targetDistanceTolerance_?NumericQ,
    freeBoundary_List, targetBoundary_List])[
        {ft_Point, fT_Point, Ft_Point, FT_Point}, o:OptionsPattern[]] := Block[
    {
        splitBottom, splitLeft, splitTop, splitRight,
        ct, fc, cT, Fc, cc, cache
    },

    {cache} = OptionValue[QuadSampling, {o}, {"Cache"}];

    {splitBottom, splitLeft, splitTop, splitRight} = (
        {{ft, Ft}, {ft, fT}, {fT, FT}, {Ft, FT}}
        // Map[Apply[With[
            {
                denseDivider = If[(
                        (* delta < threshold *)
                        Part[{#1, #2}, All, UnwrapPoint, 3]
                        // Abs
                        // Min
                        // LessEqualThan[$ZeroRatio * dropLength[node]]
                    ) || (
                        (* range includes boundary *)
                        BoundarySampleQ[freeBoundary][Part[{#1, #2}, All, UnwrapPoint, 1]] ||
                        BoundarySampleQ[targetBoundary][Part[{#1, #2}, All, UnwrapPoint, 2]]
                    ),
                    $KdDenseDivider,
                    1
                ]
            },
            EuclideanDistance[Part[#1, UnwrapPoint, 1], Part[#2, UnwrapPoint, 1]] > (freeDistanceTolerance / denseDivider) ||
            EuclideanDistance[Part[#1, UnwrapPoint, 2], Part[#2, UnwrapPoint, 2]] > (targetDistanceTolerance / denseDivider) ||
            EuclideanDistance[Part[#1, UnwrapPoint, 3], Part[#2, UnwrapPoint, 3]] > (dropDiffTolerance / denseDivider)
        ]&]]
    );

    If[splitBottom || splitTop,
        {ct, cT} = {{ft, Ft}, {fT, FT}}
            // Map[Midpoint /* LookupOrCalcSamplePoint[node, solution, tFlip, cache]];
        Sow[Delete[cT, {UnwrapPoint, 3}] -> Part[cT, UnwrapPoint, 3], "CacheT"]
    ];
    If[splitLeft || splitRight,
        {fc, Fc} = {{ft, fT}, {Ft, FT}}
            // Map[Midpoint /* LookupOrCalcSamplePoint[node, solution, tFlip, cache]];
        Sow[Delete[Fc, {UnwrapPoint, 3}] -> Part[Fc, UnwrapPoint, 3], "CacheF"]
    ];
    If[(splitBottom || splitTop) && (splitLeft || splitRight),
        cc = {ft, FT} // Midpoint // LookupOrCalcSamplePoint[node, solution, tFlip, cache]
    ];

    {splitBottom || splitTop, splitLeft || splitRight}
    // Replace[{
        {False, False} :> Block[
            {
                cacheT1, cacheT2
            },

            {
                FindZeroBoundaries[node, solution, tFlip][{ft, fT, Ft, FT}, "Cache" -> cache]
                    // Unevaluated // ReapCache[cacheF, cacheT],
                Sow[cacheF, "CacheF"],
                Sow[cacheT, "CacheT"]
            } // First
        ],
        {True, False} :> Block[
            {
                resultf, resultF, cacheF, cacheT1, cacheT2
            },

            resultf = quadSampling[{ft, fT, ct, cT}, "Cache" -> cache]
                // Unevaluated // ReapCache[cacheF, cacheT1];
            resultF = quadSampling[{ct, cT, Ft, FT}, "Cache" -> Join[cache, cacheF]]
                // Unevaluated // ReapCache[cacheF, cacheT2];
            Sow[cacheF, "CacheF"];
            Sow[Join[cacheT1, cacheT2], "CacheT"];
            mergeZerosF[resultf, resultF] // SowSampleList
        ],
        {False, True} :> Block[
            {
                resultt, resultT, cacheT, cacheF1, cacheF2
            },

            resultt = quadSampling[{ft, fc, Ft, Fc}, "Cache" -> cache]
                // Unevaluated // ReapCache[cacheF1, cacheT];
            resultT = quadSampling[{fc, fT, Fc, FT}, "Cache" -> Join[cache, cacheT]]
                // Unevaluated // ReapCache[cacheF2, cacheT];
            Sow[cacheT, "CacheT"];
            Sow[Join[cacheF1, cacheF2], "CacheF"];
            mergeZerosT[resultt, resultT, "QuadSampling" -> quadSampling] // SowSampleList
        ],
        {True, True} :> Block[
            {
                resultft, resultfT, resultFt, resultFT,
                cacheT1, cacheT2, cacheF1, cacheF2
            },

            resultft = quadSampling[{ft, fc, ct, cc}, "Cache" -> cache]
                // Unevaluated // ReapCache[cacheF1, cacheT1];
            resultfT = quadSampling[{fc, fT, cc, cT}, "Cache" -> Join[cache, cacheT1]]
                // Unevaluated // ReapCache[cacheF2, cacheT1];
            resultFt = quadSampling[{ct, cc, Ft, Fc}, "Cache" -> Join[cache, cacheF1]]
                // Unevaluated // ReapCache[cacheF1, cacheT2];
            resultFT = quadSampling[{cc, cT, Fc, FT}, "Cache" -> Join[cache, cacheF2, cacheT2]]
                // Unevaluated // ReapCache[cacheF2, cacheT2];
            Sow[Join[cacheF1, cacheF2], "CacheF"];
            Sow[Join[cacheT1, cacheT1], "CacheT"];
            mergeZerosF[
                mergeZerosT[resultft, resultfT, "QuadSampling" -> quadSampling] // SowSampleList,
                mergeZerosT[resultFt, resultFT, "QuadSampling" -> quadSampling] // SowSampleList
            ] // SowSampleList
        ]
    }]
]


(* Internal use for getting List out of Point in Part *)
UnwrapPoint = 1


connectBoundaryZeros[actions_List] := Block[
    {
        realZeros, fakeZeros
    },

    realZeros = actions
    // Cases[{_, {"TakeZero", zero_}}:> zero];
    fakeZeros = actions
    // Cases[{_, {"TakeMinima", minima_}}:> minima];
    (* // {}&; *)

    Join[
        Outer[List, realZeros, fakeZeros]
        // Catenate
        // ReplacePart[{_, _, 1} -> Center],
        Subsets[realZeros, {2}]
    ]
    (* Subsets[Join[realZeros, fakeZeros], {2}] *)
    // Cases[{in:{_, inZero_, _}, out:{_, outZero_, _}} :> (
        {in, out}
        // If[First[inZero] > First[outZero],
            Reverse,
            Identity
        ]
    )]
    (*
        If the zeros are at Top or Bottom, make sure the ranges of each zeros
        are montonic:
        inRangeMin < inX <= inRangeMax <= outRangeMin <= outX < outRangeMax
    *)
    // Replace[#, {in:{(Top|Bottom), {inX_, __}, {_, inRangeMax_}},
            out:{(Top|Bottom), {outX_, __}, {outRangeMin_, _}}} :> {
        in // ReplacePart[{-1, 1} -> Min[{inRangeMax, outX}]],
        out // ReplacePart[{-1, 1} -> Max[{outRangeMin, inX}]]
    }, {1}]&
    (* // Replace[{} :> (
        fakeZeros
        // Cases[zero:{side_, _, _} :> (
            {zero, zero // ReplacePart[1 -> Center]}
            // {
                If[side =!= Right, Identity, Nothing],
                If[side =!= Left, Reverse, Nothing]
            } // Through
        )]
        // Catenate
    )] *)
    // Map[Apply[<|"In" -> #1, "Out" -> #2, "InternalPoints" -> {}|>&]]
]


Options[FindZeroBoundaries] = {
    "Cache" -> <||>
}


(findZeroBoundaries:FindZeroBoundaries[node_DRNode, solution_Association, tFlip_Association])[
    {ft_Point, fT_Point, Ft_Point, FT_Point}, o:OptionsPattern[]] := Block[
    {
        fc, Fc, ct, cT, cc,
        actions, cache
    },

    {cache} = OptionValue[FindZeroBoundaries, {o}, {"Cache"}];

    Block[
        {
            cacheF, cacheT
        },

        actions = (
            {
                {Left, {ft, fT}},
                {Right, {Ft, FT}},
                {Bottom, {ft, Ft}},
                {Top, {fT, FT}}
            }
            // Map[Apply[Function[{side, points},
                GetBoundaryAction[
                    GetGoalFunction[node, solution, tFlip, cache],
                    side, points, dropLength[node]
                ] // Unevaluated
                // ReapCache[cacheF, cacheT]
                // ({#, side}&
                    // ReplaceAt[{
                        Top :> Sow[cacheT, "CacheT"],
                        Right :> Sow[cacheF, "CacheF"]
                    }, {(*UnwrapFunction:*)1, 2}]
                ) // First
                // {side, #}&
            ]]]
        ) (*// Echo[#, {ft, fT, Ft, FT}]&*)
    ];

    actions
    // Partition[#, 2]&
    // Map[FirstCase[{side_, {"Split", splitFunction_}}:> {side, splitFunction}]]
    // Replace[{_?MissingQ, _?MissingQ} /; (
        actions
        // MemberQ[{side_, {"TakeZero", _}}]
    ):> (
        FirstCase[actions, {side_, {"SplitIfZeros", splitFunctions_}} :> (
            {{side, Midpoint}, {side, splitFunctions}}
            // If[side // MatchQ[Bottom|Top],
                Identity, Reverse
            ]
        ), {Missing["NotFound"], Missing["NotFound"]}]
    )]
    // Replace[{
        {_?MissingQ, _?MissingQ} :> (
            {ft, FT} -> connectBoundaryZeros[actions]
        ),
        {{side_, splitFunction_}, _?MissingQ} :> Block[
            {
                cacheF, cacheT
            },

            {fc, Fc} = (
                {{ft, fT}, {Ft, FT}}
                // Map[splitFunction]
                // MapAt[
                    calcSamplePoint[node, solution, tFlip, #]&,
                    If[side === Left, 2, 1]
                ]
            );
            mergeZerosT[
                findZeroBoundaries[{ft, fc, Ft, Fc}, "Cache" -> cache]
                // Unevaluated // ReapCache[cacheF, cacheT],
                findZeroBoundaries[{fc, fT, Fc, FT}, "Cache" -> Join[cache, cacheT]]
            ]
        ],
        {_?MissingQ, {side_, splitFunction_}} :> Block[
            {
                cacheF, cacheT
            },

            {ct, cT} = (
                {{ft, Ft}, {fT, FT}}
                // Map[splitFunction]
                // MapAt[
                    calcSamplePoint[node, solution, tFlip, #]&,
                    If[side === Bottom, 2, 1]
                ]
            );
            mergeZerosF[
                findZeroBoundaries[{ft, fT, ct, cT}, "Cache" -> cache]
                // Unevaluated // ReapCache[cacheF, cacheT],
                findZeroBoundaries[{ct, cT, Ft, FT}, "Cache" -> Join[cache, cacheF]]
            ]
        ],
        {{sideT_, splitFunctionT_}, {sideF_, splitFunctionF_}} :> Block[
            {
                cacheF1, cacheF2, cacheT
            },

            {fc, Fc} = (
                {{ft, fT}, {Ft, FT}}
                // Map[splitFunctionT]
                // MapAt[
                    calcSamplePoint[node, solution, tFlip, #]&,
                    Replace[sideT, {Left -> 2, Right -> 1, _ -> All}]
                ]
            );
            {ct, cT, cc} = (
                {{ft, Ft}, {fT, FT}, {fc, Fc}}
                // Map[splitFunctionF]
                // MapAt[
                    calcSamplePoint[node, solution, tFlip, #]&,
                    Replace[sideT, {Bottom -> {{2}, {3}}, Top -> {{1}, {3}}, _ -> All}]
                ]
            );

            mergeZerosF[
                mergeZerosT[
                    findZeroBoundaries[{ft, fc, ct, cc}, "Cache" -> cache]
                    // Unevaluated // ReapCache[cacheF1, cacheT],
                    findZeroBoundaries[{fc, fT, cc, cT}, "Cache" -> Join[cache, cacheT]]
                    // Unevaluated // ReapCache[cacheF2, cacheT]
                ] // SowSampleList,
                mergeZerosT[
                    findZeroBoundaries[{ct, cc, Ft, Fc}, "Cache" -> Join[cache, cacheF1]]
                    // Unevaluated // ReapCache[cacheF1, cacheT],
                    findZeroBoundaries[{cc, cT, Fc, FT}, "Cache" -> Join[cache, cacheF2, cacheT]]
                ] // SowSampleList
            ]
        ]
    }] // SowSampleList
]


SowSampleList := (
    If[$SowSampleList, ((*AppendTo[t`SampleList, #];*) Sow[#, "SampleList"])&, Identity]
)


GetGoalFunction[node_DRNode, solution_Association, tFlip_Association, cache_][
    side:(Left|Bottom|Top|Right), fixedCayley_?NumericQ] := Block[
    {
        (*uncaptured slot:*) runningCayley$, cayleySlot, sowFunction
    },

    cayleySlot = {fixedCayley, runningCayley$}
        // If[side // MatchQ[Left|Right],
            Identity,
            Reverse
        ];

    With[
        {
            pointSlot = cayleySlot // Point,
            cayleyLengthSlot = cayleySlot
                // Apply[{
                    node["FreeCayley"]
                    // Replace[{
                        {firstFree_, ___} :> (firstFree -> #1),
                        {} -> (Nothing)
                    }],
                    node["TargetCayley"] -> #2
                }&]
        },

        With[
            {
                sowDiff = Replace[side, {
                    Top :> ((Sow[pointSlot -> #, "CacheT"] // Last)&),
                    Right :> ((Sow[pointSlot -> #, "CacheF"] // Last)&),
                    _ -> Identity
                }]
            },

            Function[{runningCayley},
                Lookup[cache, pointSlot,
                    realizeNode[node, solution, tFlip, cayleyLengthSlot // Association]
                    // Replace[{
                        coordinates_Association :> (
                            dropDiff[node, coordinates]
                        ),
                        err_ :> (Echo[err, "Unknown Result"]; Abort[])
                    }]
                    // sowDiff,
                    ($SampleReuses++;#)&
                ]
            ]
        ]
    ]
]


FitGoalFunction[goalFunction_, {{start_, startDiff_}, {end_, endDiff_}}, order_Integer:3] := (
    If[start == end,
        If[# == start, startDiff, Indeterminate]&,
        Subdivide[start, end, Max[order, 1]]
        // Take[#, {2, 3}]&
        // Map[{#, goalFunction[#]}&]
        // Apply[{{start, startDiff}, ##, {end, endDiff}}&]
        // Interpolation[#, InterpolationOrder -> order]&
    ]
)


$FakeZeroTolerance = 0.05


GetBoundaryAction[goalFunctionGetter_, side:(Left|Bottom|Top|Right),
        points:{_Point, _Point}, targetDrop_?NumericQ] := Block[
    {
        fixedIndex = If[side // MatchQ[Left|Right], 1, 2],
        dropDiffs = Part[points, All, UnwrapPoint, -1],
        goalFunction, fixedCayley, endpoints, range, fittingFunction
    },

    fixedCayley = Part[points, 1, UnwrapPoint, fixedIndex];
    range = Part[points, All, UnwrapPoint, 3 - fixedIndex];
    goalFunction = goalFunctionGetter[side, fixedCayley];
    endpoints = Part[Part[points, All, UnwrapPoint, {3 - fixedIndex, 3}]];
    fittingFunction = FitGoalFunction[goalFunction, endpoints] (*// Echo[#, "FittingFunction"]&*);
    (
        range
        // Apply[Subdivide[##, 3]&]
        // Take[#, {2, 3}]&
        // Map[
            {
                Identity,
                fittingFunction
            } /* Through
            /* Insert[fixedCayley, fixedIndex]
            /* Point
        ]
        // (# -> {})&
        // SowSampleList
    );

    If[(dropDiffs // Apply[Times]) <= 0,
        With[
            {
                zeroPoint = Part[points, 1, UnwrapPoint, {1, 2}],
                runningIndex = 3 - fixedIndex
            },
            (* Echo[side];
            Echo[fittingFunction];
            Echo[range]; *)
            findZeros[fittingFunction, range]
            // Map[{#, goalFunction[#]}&]
            // MinimalBy[Last]
            // Replace[{
                {{root_, delta_}, ___} :> (
                    If[Abs[delta] < Min[targetDrop * $ZeroRatio, 1*^-5],
                        {"TakeZero", {
                            side,
                            zeroPoint
                            // ReplacePart[runningIndex -> root]
                            // Append[delta],
                            range
                        }},
                        (* Echo[delta, side];
                        Echo[fittingFunction, "fittingFunction"]; *)
                        {
                            "Split",
                            First
                            /* ReplacePart[{UnwrapPoint, 3 - fixedIndex} -> root]
                            /* ReplacePart[{UnwrapPoint, 3} -> delta]
                        }
                    ]
                ),
                {} -> {"DoNothing"}
            }]
        ],
        (* {"DoNothing"}, *)
        With[
            {
                dFittingFunction = D[fittingFunction[x], x] // ReplaceAll[{x -> Slot[1]}],
                (* Pick the larger diff of boundary points *)
                furtherDiff = MaximalBy[dropDiffs, Abs] // First
            },

            findZeros[Function[dFittingFunction], range]
            // Map[{
                Identity,
                goalFunction
            } /* Through]
            // MinimalBy[Last /* (# * furtherDiff&)]
            // First[#, {}]&
            // Replace[{
                {splitPoint_, splitDiff_?(# * furtherDiff& /* NonPositive)} :> {
                    "Split",
                    First
                    /* ReplacePart[{UnwrapPoint, 3 - fixedIndex} -> splitPoint]
                    /* ReplacePart[{UnwrapPoint, 3} -> splitDiff]
                },
                {minima_, minimaDiff_?(
                    Abs
                    /* LessThan[
                        dropDiffs
                        // Append[targetDrop * $FakeZeroTolerance]
                        // Abs // Min
                    ]
                )} :> (
                    (* {"DoNothing"} *)
                    If[side // MatchQ[Bottom|Top], {
                        "SplitIfZeros",
                        First
                        /* ReplacePart[{UnwrapPoint, 3 - fixedIndex} -> minima]
                        /* ReplacePart[{UnwrapPoint, 3} -> minimaDiff]
                    }, {
                        "TakeMinima",
                        {
                            side,
                            Part[points, 1, UnwrapPoint, {1, 2}]
                            // ReplacePart[(3 - fixedIndex) -> minima]
                            // Append[minimaDiff],
                            range
                        }
                    }]
                ),
                _ -> {"DoNothing"}
            }]
        ]
    ]
]


findZeros[function_, range:{min_, max_}] := Block[
    {
        x, startPoints = (Subdivide[min, max, 3]) // SortBy[function/*Abs]
    },

    FoldWhile[
        Function[{sols, startPoint},
            Quiet[Check[
                FindRoot[function[x], {x, startPoint, min, max}],
                {}, {FindRoot::reged, FindRoot::lstol}
            ], {FindRoot::reged, FindRoot::lstol}]
            // Map[x/.#&]
            // Select[function /* Abs /* LessThan[1*^-5]]
        ],
        {},
        startPoints,
        SameAs[{}]
    ]
]


(* mergeZeros[node_DRNode, solution_Association, tFlip_Association, fixedCayley_?NumericQ,
    orientation:(Right|Top), {firstSols_List, secondSols_List}] := Join[firstSols, secondSols] // Flatten // DeleteDuplicates *)

mergeZerosF[{ft_, cT_} -> solsf_List, {ct_, FT_} -> solsF_List] := (Block[
    {
        matchingIntervals = (
            d`firstSols = ({ft, cT} -> solsf);
            d`secondSols = ({ct, FT} -> solsF);
            Join[
                solsf
                // MapIndexed[List]
                // Cases[{KeyValuePattern["Out" -> {Right, {_, y_, _}, interval_}], {index_}} :>
                    {y, interval, 1} -> {1, index}
                ] // Merge[Identity],
                solsF
                // MapIndexed[List]
                // Cases[{KeyValuePattern["In" -> {Left, {_, y_, _}, interval_}], {index_}} :>
                    {y, interval, 2} -> {2, index}
                ] // Merge[Identity]
            ] // KeySort
        )
    },

	{ft, FT} -> Join[
        solsf // DeleteCases[KeyValuePattern["Out" -> {Right, __}]],
        solsF // DeleteCases[KeyValuePattern["In" -> {Left, __}]],
        matchingIntervals
        // Keys
        // matchIntervals
        // Map[Apply[Function[{tuple1, tuple2},
            Table[
                mergeSols[
                    {
                        index1
                        // Replace[{fOrF_, index_} :> (
                            Part[{solsf, solsF}, fOrF, index]
                        )],
                        index2
                        // Replace[{fOrF_, index_} :> (
                            Part[{solsf, solsF}, fOrF, index]
                        )]
                    }
                    // If[First[index1] == 1, Identity, Reverse]
                ],
                {index1, matchingIntervals[tuple1] // Replace[_?MissingQ -> {{}}]},
                {index2, matchingIntervals[tuple2] // Replace[_?MissingQ -> {{}}]}
            ] // Catenate
        ]]] // Catenate
    ]
] // Unevaluated // Check[#, Echo[{{ft, cT} -> solsf, {ct, FT} -> solsF}], mergeSols::mono]&)


mergeZerosT::noconn = "An isolated zero found at `1`"
mergeZerosT::detectgap = "Need to resample using split points `1` for range `2`"

Options[mergeZerosT] = {
    "QuadSampling" -> None
}

mergeZerosT[{ft_, Fc_} -> solst_List, {fc_, FT_} -> solsT_List, o:OptionsPattern[]] := (Block[
    {
        quadSampling, matchingIntervals, matchedIndices,
        splitPoints, originalSplitPoints
    },

    quadSampling = OptionValue[mergeZerosT, {o}, "QuadSampling"];
    d`firstSols = ({ft, Fc} -> solst);
    d`secondSols = ({fc, FT} -> solsT);

    (* first out, second in *)
    matchingIntervals = Join[
        solst
        // MapIndexed[List]
        // {
            Cases[{KeyValuePattern[(dir:"In") -> {Top, {x_, __}, interval_}], {index_}} :>
                {x, interval, 1} -> {1, index, dir}
            ],
            Cases[{KeyValuePattern[(dir:"Out") -> {Top, {x_, __}, interval_}], {index_}} :>
                {x, interval, 1} -> {1, index, dir}
            ]
        } // Through
        // Catenate
        // Merge[Identity],
        solsT
        // MapIndexed[List]
        // {
            Cases[{KeyValuePattern[(dir:"In") -> {Bottom, {x_, __}, interval_}], {index_}} :>
                {x, interval, 2} -> {2, index, dir}
            ],
            Cases[{KeyValuePattern[(dir:"Out") -> {Bottom, {x_, __}, interval_}], {index_}} :>
                {x, interval, 2} -> {2, index, dir}
            ]
        } // Through
        // Catenate
        // Merge[Identity]
    ] // KeySort;

    splitPoints = matchingIntervals
    // Keys
    // matchIntervals
    // Cases[{{_, {f_, F_}, tOrT_}, {}} :> (
        (3 - tOrT) -> {f, F}
    )] // Merge[Catenate];

    If[splitPoints =!= <||> && quadSampling =!= None,
        originalSplitPoints = matchingIntervals
        // Keys
        // matchIntervals
        // Cases[{{_, {f1_, F1_}, tOrT1_}, {_, {f2_, F2_}, tOrT2_}} :> <|
            tOrT1 -> {f1, F1},
            tOrT2 -> {f2, F2}
        |>] // Merge[Catenate];
        Return[
            mergeZerosT[
                splitPoints[1]
                // Replace[{
                    (_?MissingQ | {}) :> ({ft, Fc} -> solst),
                    points_ :> (
                        Join[points, Lookup[originalSplitPoints, 1, {}]]
                        // Sort
                        // Prepend[Part[ft, UnwrapPoint, 1]]
                        // Append[Part[FT, UnwrapPoint, 1]]
                        // DeleteDuplicates
                        // (Message[mergeZerosT::detectgap, #, {ft, Fc}];
Echo[{OptionValue[mergeZerosT, {o}, "QuadSampling"], {ft, Fc} -> solst, {fc, FT} -> solsT}];
Abort[];
                         #)&
                        // MovingMap[quadSampling[{#, Part[{ft, Fc}, All, UnwrapPoint, 2]}]&, #, 1]&
                        // Fold[mergeZerosF]
                    )
                }],
                splitPoints[2]
                // Replace[{
                    (_?MissingQ | {}) :> ({fc, FT} -> solsT),
                    points_ :> (
                        Join[points, Lookup[originalSplitPoints, 2, {}]]
                        // DeleteMissing
                        // Sort
                        // Prepend[Part[ft, UnwrapPoint, 1]]
                        // Append[Part[FT, UnwrapPoint, 1]]
                        // DeleteDuplicates
                        // (Message[mergeZerosT::detectgap, #, {fc, FT}];
Echo[{OptionValue[mergeZerosT, {o}, "QuadSampling"], {ft, Fc} -> solst, {fc, FT} -> solsT}];
Abort[];
                         #)&
                        // MovingMap[quadSampling[{#, Part[{fc, FT}, All, UnwrapPoint, 2]}]&, #, 1]&
                        // Fold[mergeZerosF]
                    )
                }],
                o
            ]
        ]
    ];

    matchedIndices = matchingIntervals
    // Keys
    // matchIntervals
    // Replace[#, {
        {tuple1_, {}} :> (
            matchingIntervals[tuple1]
            // Map[{#, {}}&]
        ),
        {tuple1_, tuple2_} :> (
            Outer[
                List,
                matchingIntervals[tuple1],
                matchingIntervals[tuple2],
                1
            ] // Catenate
        )
    }, {1}]& // Catenate;

    d`matchedIndices = matchedIndices;

	{ft, FT} -> Join[
        solst // DeleteCases[KeyValuePattern[("In"|"Out") -> {Top, __}]],
        solsT // DeleteCases[KeyValuePattern[("In"|"Out") -> {Bottom, __}]],
        matchedIndices // mergeIndices[solst, solsT]
    ]
] // Unevaluated // Check[#, Echo[{OptionValue[mergeZerosT, {o}, "QuadSampling"], {ft, Fc} -> solst, {fc, FT} -> solsT}], mergeSols::mono]&)


matchIntervals::overlap = "Overlap ranges have been detected when merging `1` and `2`."

matchIntervals = Function[{tuples},
    tuples
    // Fold[matchIntervalsImpl]
    // Replace[error_?(Last[#] === False&) :> (Echo[error]; Abort[])]
    // Replace[tuple:{_, _, _} :> (
        Sow[{tuple, {}}, "matchIntervals"]
    )]
    // Unevaluated // Reap[#, "matchIntervals"]&
    // Last // Last[#, {}]&
]

matchIntervalsImpl[{x1_, interval1_, tOrT1_, matchedQ_:False},
        tuple2:{x2_, interval2_, tOrT2_}] := (
    If[tOrT1 == tOrT2 &&
            (RangeIntersection[interval1, interval2] // Apply[Less]),
        Message[matchIntervals::overlap, interval1, interval2];
        Echo[{d`firstSols, d`secondSols}];
        Abort[];
    ];

    If[(Between[x1, interval2] || Between[x2, interval1]) && tOrT1 != tOrT2,
        Sow[{{x1, interval1, tOrT1}, tuple2}, "matchIntervals"];
        tuple2 // Append[(*matchedQ:*)True],
        If[!matchedQ, Sow[{{x1, interval1, tOrT1}, {}}, "matchIntervals"]];
        tuple2
    ]
)


mergeIndices::unmatched = "Unmatched solutions during merging `1`"

mergeIndices[sols1_List, sols2_List] := Function[{matchedIndices},
    Fold[mergeIndicesImpl[sols1, sols2], <||>, matchedIndices]
    // Replace[unmatched:Except[<||>] :> (
        Echo[{unmatched, sols1, sols2, matchedIndices}];
        Message[mergeIndices::unmatched, Part[unmatched, All, {Key["In"], Key["Out"]}]]
    )]
    // Unevaluated // Reap[#, "mergeIndices"]&
    // Last // Last[#, {}]&
    (* // Unevaluated
    // Check[#, Echo[{sols1, sols2, resampleFunction, matchedIndices}];Abort[]]& *)
]


mergeIndicesImpl[solst_List, solsT_List][incompleteSols_Association,
        {{tOrT1_, index1_, dir1_}, {tOrT2_, index2_, dir2_}|{}}] := Block[
    {
        (*
            Gets the merging solution from either:
            1. original solutions, or
            2. previously merged but incomplete solutions
        *)
        solsIn1 = {Part[{solst, solsT}, tOrT1, index1]},
        (* SetDelayed here since sols2 may not be used *)
        solsIn2 := {Part[{solst, solsT}, tOrT2, index2]},
        solsOut1 = Lookup[incompleteSols, Key[{tOrT1, index1}], {Part[{solst, solsT}, tOrT1, index1]}],
        (* SetDelayed here since sols2 may not be used *)
        solsOut2 := Lookup[incompleteSols, Key[{tOrT2, index2}], {Part[{solst, solsT}, tOrT2, index2]}],
        (*
            If the merged solution is incomplete, adds it to the association.
            Otherwise, deletes the previous incomplete one if exists.
            These are actions to be applied to `incompleteSols`.
        *)
        sowOnly = (Sow[#, "mergeIndices"]; Identity)&,
        delete1 = Delete[Key[{tOrT1, index1}]],
        delete2 = Delete[Key[{tOrT2, index2}]],
        appendOrSow1 = Function[{newSol},
            If[Part[newSol, Key["Out"], 1] === Part[{Top, Bottom}, tOrT1],
                Merge[Catenate][{#, {tOrT1, index1} -> {newSol}}]&,
                Sow[newSol, "mergeIndices"];
                Delete[Key[{tOrT1, index1}]]
            ]
        ],
        appendOrSow2 = Function[{newSol},
            If[Part[newSol, Key["Out"], 1] === Part[{Top, Bottom}, tOrT2],
                Merge[Catenate][{#, {tOrT2, index2} -> {newSol}}]&,
                Sow[newSol, "mergeIndices"];
                Delete[Key[{tOrT2, index2}]]
            ]
        ]
    },

    {dir1, dir2}
    // Replace[{
        {"In", "Out"} :> (
            (* sol2 -> sol1 *)
            Table[
                mergeSols[{sol2, sol1}] // appendOrSow1,
                {sol2, solsOut2}, {sol1, solsIn1}
            ] // Catenate // Append[delete2]
        ),
        {"Out", "In"} :> (
            (* sol1 -> sol2 *)
            Table[
                mergeSols[{sol1, sol2}] // appendOrSow2,
                {sol1, solsOut1}, {sol2, solsIn2}
            ] // Catenate // Append[delete1]
        ),
        {"In", "In"} :> (
            (*
                -> sol1
                -> sol2
            *)
            Join[
                Table[mergeSols[{{}, sol1}] // appendOrSow1, {sol1, solsIn1}],
                Table[mergeSols[{{}, sol2}] // appendOrSow2, {sol2, solsIn2}]
            ]
        ),
        {"Out", "Out"} :> (
            (*
                sol1 ->
                sol2 ->
            *)
            Join[
                Table[mergeSols[{sol1, {}}] // sowOnly, {sol1, solsOut1}],
                Table[mergeSols[{sol2, {}}] // sowOnly, {sol2, solsOut2}],
                {delete1, delete2}
            ]
        ),
        {"Out"} :> (
            Table[mergeSols[{sol1, {}}] // sowOnly, {sol1, solsOut1}]
            // Append[delete1]
        ),
        {"In"} :> (
            Table[mergeSols[{{}, sol1}] // appendOrSow1, {sol1, solsIn1}]
        )
    }] // Unevaluated
    // Check[#, Echo[{{tOrT1, index1, dir1}, {tOrT2, index2, dir2}}, "tuples"];
        Echo[incompleteSols, "incompleteSols"],
        mergeSols::mono]&
    // Prepend[incompleteSols]
    // Fold[#2[#1]&]
]


quadResampling::unhandledCase = "The directions `1` of `2` are not known cases. Falling back to linear interpolation"

quadResampling[quadSampling_QuadSampling, {f_, F_}, {t_, c_, T_}][
        {dir1_, sol1:KeyValuePattern[dir1_ -> {side1_, _, {f1_, f2_}}]},
        {dir2_, sol2:KeyValuePattern[dir2_ -> {side2_, _, {F1_, F2_}}]}
    ] := Block[
    {
        flipDir = If[# === "In", "Out", "In"]&,
        flipSide = If[# === Top, Bottom, Top]&,
        centerF = Mean[{f2, F1}],
        newSolutions
    },

    newSolutions = (
        {side1, side2} // Replace[{
            {Top, Top} :> (
                mergeZerosF[
                    mergeZerosF[
                        quadSampling[{{f, f1}, {c, T}}],
                        quadSampling[{{f1, centerF}, {c, T}}]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir1] -> {flipSide[side1], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1],
                    mergeZerosF[
                        quadSampling[{{centerF, F2}, {c, T}}],
                        quadSampling[{{F2, F}, {c, T}}]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir2] -> {flipSide[side2], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1]
                ] // Last
            ),
            {Bottom, Bottom} :> (
                mergeZerosF[
                    mergeZerosF[
                        quadSampling[{{f, f1}, {t, c}}],
                        quadSampling[{{f1, centerF}, {t, c}}]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir1] -> {flipSide[side1], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1],
                    mergeZerosF[
                        quadSampling[{{centerF, F2}, {t, c}}],
                        quadSampling[{{F2, F}, {t, c}}]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir2] -> {flipSide[side2], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1]
                ] // Last
            ),
            {Top, Bottom} :> (
                Message[quadResampling::unhandledCase, {side1, side2}, {sol1, sol2}];
                {}
                (* mergeZerosT[
                    mergeZerosF[
                        quadSampling[{{f, centerF}, {t, c}}],
                        mergeZerosF[
                            quadSampling[{{centerF, F2}, {t, c}}],
                            quadSampling[{{F2, F}, {t, c}}]
                        ]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir2] -> {flipSide[side2], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1],
                    mergeZerosF[
                        mergeZerosF[
                            quadSampling[{{f, f1}, {c, T}}],
                            quadSampling[{{f1, centerF}, {c, T}}]
                        ],
                        quadSampling[{{centerF, F}, {c, T}}]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir1] -> {flipSide[side1], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1],
                    "QuadSampling" -> quadSampling
                ] *)
            ),
            {Bottom, Top} :> (
                Message[quadResampling::unhandledCase, {side1, side2}, {sol1, sol2}];
                {}
                (* mergeZerosT[
                    mergeZerosF[
                        mergeZerosF[
                            quadSampling[{{f, f1}, {t, c}}],
                            quadSampling[{{f1, centerF}, {t, c}}]
                        ],
                        quadSampling[{{centerF, F}, {t, c}}]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir1] -> {flipSide[side1], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1],
                    mergeZerosF[
                        quadSampling[{{f, centerF}, {c, T}}],
                        mergeZerosF[
                            quadSampling[{{centerF, F2}, {c, T}}],
                            quadSampling[{{F2, F}, {c, T}}]
                        ]
                    ] // MapAt[Cases[KeyValuePattern[
                        filpDir[dir2] -> {flipSide[side2], {_?(Between[{f1, f2}]), _, _}, _}
                    ]], -1],
                    "QuadSampling" -> quadSampling
                ] *)
            )
        }]
    );

    newSolution
    // FirstCase[KeyValuePattern[{
        flipDir[dir1] -> {flipSide[side1], {_?(Between[{f1, f2}]), _, _}, _},
        flipDir[dir2] -> {flipSide[side2], {_?(Between[{F1, F2}]), _, _}, _}
    }]]
    (* Fallback to 2 separate solutions *)
    // Replace[_?MissingQ :> (
        newSolution
        // {
            FirstCase[KeyValuePattern[
                "In" -> {flipSide[side1], {_?(Between[{f1, f2}]), _, _}, _}
            ]],
            FirstCase[KeyValuePattern[
                "Out" -> {flipSide[side2], {_?(Between[{F1, F2}]), _, _}, _}
            ]]
        } // Through
    )]
    (* Fallback to linear interpolation *)
    // Replace[{_?MissingQ, _?MissingQ} :> (
        interpolateGap[sol1, sol2, InterpolationOrder -> 1]
    )]
]


Options[interpolateGap] = {
    InterpolationOrder -> 1
}

interpolateGap::order = "Unable to interpolate the gap between `1` and `2`: out point is larger than in point"

interpolateGap[outSol_Association, inSol_Association, o:OptionsPattern[]] := With[
    {
        gapPoints := (
            Subdivide[Part[outSol["Out"], 2, 1], Part[inSol["In"], 2, 1], 4]
            // Take[#, {2, -2}]&
        ),
        gapFunction = (
            Interpolation[{
                outSol["InternalPoints"] // Last[#, Part[outSol["In"], 2, {1, 2}]]&,
                Part[outSol["Out"], 2, {1, 2}],
                Part[inSol["In"], 2, {1, 2}],
                inSol["InternalPoints"] // First[#, Part[inSol["Out"], 2, {1, 2}]]&
            }, o]
        )
    },
    <|
        "In" -> outSol["Out"],
        "Out" -> inSol["In"],
        "InternalPoints" -> (
            If[Part[outSol["Out"], 2 ,1] > Part[inSol["In"], 2, 1],
                Message[interpolateGap::order, Part[outSol["Out"], 2], Part[inSol["In"], 2]];
                {},
                Table[{p, gapFunction[p]}, {p, gapPoints}]
            ]
        )
    |>
]


mergeSols[{{}, inSol_Association}] := (
    inSol
    // ReplacePart[{Key["In"], 1} -> Center]
)

mergeSols[{outSol_Association, {}}] := (
    outSol
    // ReplacePart[{Key["Out"], 1} -> Center]
)

mergeSols::mono = "Solutions is not monotonic";

mergeSols[{outSol:KeyValuePattern["Out" -> {_, {outX_, outY_, outZ_}, _}],
        inSol:KeyValuePattern["In" -> {_, {inX_, inY_, inZ_}, _}]}] := Block[
    {
        inPoint = Part[outSol, Key["In"], 2, {1, 2}],
        outPoint = Part[inSol, Key["Out"], 2, {1, 2}]
    },
    <|
        "In" -> outSol["In"],
        "Out" -> inSol["Out"],
        "InternalPoints" -> (
            If[({Part[outSol, Key["In"], 1], Part[inSol, Key["Out"], 1]}
                // MatchQ[{Top, Bottom}|{Bottom, Top}]) &&
                EuclideanDistance[inPoint, outPoint] < (1*^-5 * Sqrt[2]),
                (* Remove internal points if they are too crowded in a tiny region *)
                {},
                Join[
                    outSol["InternalPoints"],
                    {outX, inX}
                    // Map[Between[{
                        Last[outSol["InternalPoints"], inPoint] // First,
                        Last[inSol["InternalPoints"], outPoint] // First
                    }]]
                    // Replace[{
                        {True, True} :> (
                            If[Abs[outZ] < Abs[inZ],
                                {{outX, outY}},
                                {{inX, inY}}
                            ]
                        ),
                        {True, False} :> (
                            {{outX, outY}}
                        ),
                        {False, True} :> (
                            {{inX, inY}}
                        ),
                        _ :> (
                            Echo[{outSol, inSol}, "Bad match"];
                            Message[mergeSols::mono];
                            Abort[];
                        )
                    }],
                    inSol["InternalPoints"]
                ]
            ]
        )
    |>
]


FinalizeSol[KeyValuePattern[{
    "In" -> {_, inPoint_, _},
    "Out" -> {_, outPoint_, _},
    "InternalPoints" -> sol_
}]] := With[
    {
        points = Join[
            {Take[inPoint, 2]},
            sol,
            {Take[outPoint, 2]}
        ]
    },

    Part[points, All, 1]
    // BlockMap[Apply[Equal], #, 2, 1]&
    // Prepend[False]
    // Position[True]
    // Delete[points, #]&
]


End[]


EndPackage[]