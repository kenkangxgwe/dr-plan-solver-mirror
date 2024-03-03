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
Unset[$NodeSolutionCallback] ^:= ($NodeSolutionCallback = #&;)
Unset[$NodeSolutionCallback]

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
*)
Options[SolveNode] := {
    "Reevaluate" -> False,
    "AllCFlip" -> False,
    "SowSampleList" -> False,
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
        UnEcho[#, "nodeSolutions", (ToString/@#)&]& @ {
            NodeSolution[
                <|node["TargetCayley"] -> ((node["FreeCayley"] // Map[Key]) /* Through /* First)|>, (* identity function *)
                <|node["TargetCayley"] -> node["Interval"] + If[overflip, (
                    node["Interval"]
                    // Apply[EuclideanDistance]
                    // (# * $BoundaryRatio * {-1, 1})&
                ), 0]|>, (* domain *)
                {1}, (* D-flip index *)
                <||> (* overwriting the C-flip*)
            ],
            If[allCFlip,
                NodeSolution[
                    <|node["TargetCayley"] -> ((node["FreeCayley"] // Map[Key]) /* Through /* First)|>, (* identity function *)
                    <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
                    {1}, (* D-flip index *)
                    <|cayleyVertex -> !PropertyValue[{rootgraph, cayleyVertex}, "Flip"]|> (* overwriting the C-flip*)
                ],
                Nothing
            ]
        },

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
        $Overflip = overflip;

        (* Solve a flip *)
        {solutions, $sampleLists} = If[parallelize,
            ParallelTable[
                Reap[SolveDFlip[nodeI, nodeSolution, FilterRules[{o}, Options[SolveDFlip]]]],
                {nodeSolution, nodeSolutions},
                DistributedContexts -> {"DRPLAN`Core`", "DRPLAN`Solver`", "DRPLAN`Utility`", "DRPLAN`Thread`", "DataType`"},
                Method -> "FinestGrained"
            ],
            Table[
                Reap[SolveDFlip[nodeI, nodeSolution, FilterRules[{o}, Options[SolveDFlip]]]],
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
        vertices = VertexList[node["Graph"]], v1, v2
    },

    If[Length[vertices] > 2,

        {v1, v2} = Take[vertices, 2];
        {
            Drop[vertices, 2],
            <|
                v1 -> {0, 0},
                v2 -> {PropertyValue[{node["Root"]["Graph"], v1<->v2}, EdgeWeight], 0}
            |>
        } // calcCoordsImpl[node["Root"]["Graph"], cayleyLength, flipVector],
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
        sign, overflip = False, cayleyTrig = False
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
           cayleyTrig = True;
                If[sampleLength // Between[interval],
                    sampleLength,
                    If[sampleLength > Max[interval],
                        2 * Max[interval] - sampleLength,
                        2 * Min[interval] - sampleLength
                    ]
                    // Replace[Except[_?(Between[interval])] :> (
                        Echo[{v0, v1, v2}, "Vertices"];
                        Echo[flipVector, "FlipVector"];
                        Echo[cayleyLength, "CaleyLength"];
                        Echo[coordsList];
                        Message[calcCoords::cnof, e, sampleLength, interval];
                        Abort[]
                    )]
                ] // {sampleLength, #}&
            ],
            "Partial" :> (PropertyValue[{rootgraph, e}, EdgeWeight] // {#, #}&),
            err_ :> (Message[calcCoords::nttedge, e, err]; Abort[])
        }],
        {e, {v1 <-> v2, v0 <-> v1, v0 <-> v2}}
    ];

    {dx, dy} = c1 - c2;
    {mx, my} = (c1 + c2) / 2;
    dd = d1 - d2;
    md = (d1 + d2) / 2;

    overflip = (Part[{d0 - dd, md - d0 / 2, d0 + dd}, All, 1] // MemberQ[_?Negative]) && cayleyTrig;
    {d0, d1, d2, dd, md} = Part[$t={d0, d1, d2, dd, md}, All, -1];

    delta = Max[(Chop[(d0 - dd) * (md - d0 / 2) * (d0 + dd)] * (md + d0 / 2)), 0]
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

        sign = If[Xor[MemberQ[flipVector, v0], overflip], 1, -1];
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
                        Echo[coordsList];
                        Abort[]
                    ),
                    err_ /; (Chop[EuclideanDistance[err, c1] - d1] != 0 ||
                        Chop[EuclideanDistance[err, c2] - d2] != 0) :> (
                        Echo[d0, "d0"];
                        Echo[dd, "dd"];
                        Echo[md, "md"];
                        Echo[{v0, v1, v2}, "Vertices"];
                        Echo[err, "Coordinates"];
                        Echo[flipVector, "FlipVector"];
                        Echo[cayleyLength, "CaleyLength"];
                        Echo[coordsList];
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
$RightBoundaryStart = Max[$SampleDivisor - Round[$SampleDivisor * $BoundaryRatio], 1]
$ResampleRatio = 0.15
$ZeroRatio = 0.01


(* return a list of sample indices from 0 to $SampleDivisor *)
getSamples[{left_, right_}, planShortestEdge_?NumericQ] := Module[
    {
        sampleNum,
        sampleIndices
    },

    (* The wider the interval is, the more samples we take. *)
    sampleNum = Max[Ceiling[(right - left) / planShortestEdge * $SampleNum], $SampleNum];

    sampleIndices = If[$RefineSampling,
        Join[
            Range[0, $LeftBoundaryEnd, GetDenseSampleDistance[sampleNum]], (* Left End Point & Left Boundary*)
            Range[$LeftBoundaryEnd, $RightBoundaryStart, GetSparseSampleDistance[sampleNum]], (* Center *)
            Range[$RightBoundaryStart, $SampleDivisor, GetDenseSampleDistance[sampleNum]], (*Right Boundary*)
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


Options[SolveDFlip] = {
    "Method" -> "UniformSampling" (* "KdTree" *)
}

SolveDFlip[args__, o:OptionsPattern[]] := (
    If[OptionValue["Method"] == "UniformSampling",
        UniformSampling[args],
        KdTreeSampling[args]
    ]
)


realizeNode[node_DRNode, solution_Association, tFlip_Association, sample_Association] := With[
    {
        cayleyLength = ((#[sample]&) /@ solution)
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
UniformSampling[node_DRNode, nodeSolution_NodeSolution, dropOffset:_?NumericQ:1] := Module[
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
        // Map[getSamples[#, node["Root"]["PlanShortestEdge"]]&]
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
        {Tuple[scanSamples[node, nodeSolution, dropOffset][<||>]]},
        (* firstSamples is a sparse array, should not use Replace[..., {1}] *)
        firstSamples // Map[Replace[freeSample:Except[_Missing] :> (
            Tuple[scanSamples[node, nodeSolution, dropOffset][<|First[node["FreeCayley"]] -> freeSample|>]]
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
            Tuple[scanSamples[node, nodeSolution, dropOffset][<|First[node["FreeCayley"]] -> freeSample|>]]
        )] /@ refinedFreeSamples);

        finalSamples = UnEcho@SparseArray[
            Most[ArrayRules[firstSamples]] ~Join~ Most[ArrayRules[refinedFreeSamples]],
            $SampleDivisor + 1, Missing["NotSampled"]
        ];
        finalResults = UnEcho@SparseArray[
            Most[ArrayRules[firstResults]] ~Join~ Most[ArrayRules[refinedResults]],
            $SampleDivisor + 1, Missing["NotSampled"]
        ];

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


scanSamples[node_DRNode, nodeSolution_NodeSolution, dropOffset:_?NumericQ:1][freeSample_Association] := Module[
    {
        solution, domain, tFlip, sampleNum,
        refinedDomain, targetSamples,
        sampleList, approxIntervals, targetRefinedSamples, refinedSampleList,
        threshold, zeroThreshold, zeroIntervals, approxZeros, interp, interpd, tmpZeros
    },

    solution = Part[nodeSolution, 1];
    domain = Part[nodeSolution, 2];
    tFlip = Part[nodeSolution, 4];

    (* refine the domain use triangle inequalities *)
    refinedDomain = (*t`$rd =*) RangeIntersection[
        domain[node["TargetCayley"]],
        refineInterval[node, node["TargetCayley"],
            (* we require the target cayley parameter not appears in its prior vertices' solutions *)
            (#[freeSample]&) /@ KeySelect[solution, (# < node["TargetCayley"]&)]
        ]
    ];
    (* If[Head[refinedDomain] =!= Interval, Echo[t`rd]]; *)
    If[(Max[#] - Min[#]&)[refinedDomain] <= ($MachineEpsilon * $SampleDivisor),
        Echo[freeSample,"Empty Refined Interval"]; Return[{}]
    ];

    {sampleNum, targetSamples} = getSamples[refinedDomain, node["Root"]["PlanShortestEdge"]];
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

    (* sampleLists = sampleList; *)

    (* To draw 3D points plot, uncomment the following line *)
    If[$SowSampleList,
        sampleList // ArrayRules // Most
        // Cases[({pos_} -> Pair[targetSample_, dropDiff_]) :> Block[
            {
                cayleyLength = (Append[freeSample, node["TargetCayley"] -> targetSample])
                    // (sampleAssoc \[Function] ((solution // Map[#[sampleAssoc]&])))
            },
            {
                node["FreeCayley"]
                // Replace[{
                    {freeCayley_, ___} :> freeSample[freeCayley],
                    _ -> targetSample
                }],
                cayleyLength,
                dropDiff
            }
        ]]
        // Sow
    ];

    (* If[MatchQ[sampleList, {_Real, _Real}],
        Print[targets]
    ]; *)


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

    {#, interpd[#]}& /@ Sort[Join[tmpZeros, List@@@Part[sampleList, approxZeros, 1]]]

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
KdTreeSampling[node_DRNode, nodeSolution_NodeSolution, dropOffset:_?NumericQ:1] := Block[
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
    // Apply[Outer[List/*Point, ##, {Missing["NotSolved"]}]&]
    // Flatten
    // Map[calcSamplePoint[node, solution, tFlip, #]&]
    // QuadSampling[node, solution, tFlip,
        node["Root"]["PlanShortestEdge"] / $KdSamplingDivider,
        EuclideanDistance@@freeDomain / $KdSamplingDivider,
        EuclideanDistance@@targetDomain / $KdSamplingDivider
    ]
    // Map[FinalizeSol]
    // If[node["FreeCayley"] =!= {},
        Select[Chop[EuclideanDistance[First[#], Last[#]]] != 0&],
        Identity
    ]
    // MapIndexed[interpZeros[node, nodeSolution, Part[#1, All, 1], Part[#1, All, 2], First[#2]]&]
]


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
    dropDiffTolerance_?NumericQ, freeDistanceTolerance_?NumericQ, targetDistanceTolerance_?NumericQ])[
    {ft_Point, fT_Point, Ft_Point, FT_Point}] := Block[
    {
        splitt, splitf, splitT, splitF,
        ct, fc, cT, Fc, cc
    },

    {splitt, splitf, splitT, splitF} = (
        {{ft, Ft}, {ft, fT}, {fT, FT}, {Ft, FT}}
        // Map[Apply[With[
            {
                denseDivider = If[Part[{#1, #2}, All, UnwrapPoint, 3]
                    // Abs
                    // Min
                    // LessEqualThan[$ZeroRatio * dropLength[node]],
                    $KdDenseDivider,
                    1
                ]
            },
            EuclideanDistance[Part[#1, UnwrapPoint, 1], Part[#2, UnwrapPoint, 1]] > (freeDistanceTolerance / denseDivider) ||
            EuclideanDistance[Part[#1, UnwrapPoint, 2], Part[#2, UnwrapPoint, 2]] > (targetDistanceTolerance / denseDivider) ||
            EuclideanDistance[Part[#1, UnwrapPoint, 3], Part[#2, UnwrapPoint, 3]] > (dropDiffTolerance / denseDivider)
        ]&]]
    );

    {ct, fc, cT, Fc, cc} = (
        {{ft, Ft}, {ft, fT}, {fT, FT}, {Ft, FT}, {ft, FT}}
        // Map[Apply[calcSamplePoint[node, solution, tFlip, Midpoint[{#1, #2}]]&]]
    );

    {splitt || splitT, splitf || splitF}
    // Replace[{
        {False, False} :> (findZeroBoundaries[node, solution, tFlip, {ft, fT, Ft, FT}]),
        {True, False} :> (
            mergeZerosF[
                quadSampling[{ft, fT, ct, cT}],
                quadSampling[{ct, cT, Ft, FT}]
            ]
        ),
        {False, True} :> (
            mergeZerosT[
                quadSampling[{ft, fc, Ft, Fc}],
                quadSampling[{fc, fT, Fc, FT}]
            ]
        ),
        {True, True} :> (
            mergeZerosT[
                mergeZerosF[
                    quadSampling[{ft, fc, ct, cc}],
                    quadSampling[{ct, cc, Ft, Fc}]
                ]
                // (Sow[{ft, Fc} -> #]; #)&,
                mergeZerosF[
                    quadSampling[{fc, fT, cc, cT}],
                    quadSampling[{cc, cT, Fc, FT}]
                ]
                // (Sow[{fc, FT} -> #]; #)&
            ]
        )
    }]
    // (Sow[{ft, FT} -> #]; #)&
]


(* Internal use for getting List out of Point in Part *)
UnwrapPoint = 1


findZeroBoundaries[node_DRNode, solution_Association, tFlip_Association,
    {ft_Point, fT_Point, Ft_Point, FT_Point}] := (
        {ft, fT, Ft, FT}
        // Part[#, All, UnwrapPoint, -1]&
        // Apply[{ftD, fTD, FtD, FTD} \[Function]
            {
                If[ftD * fTD <= 0, {
                    Left,
                    Part[ft, UnwrapPoint, 1],
                    Part[{ft, fT}, All, UnwrapPoint, 2]
                } , Nothing],
                If[ftD * FtD <= 0, {
                    Bottom,
                    Part[ft, UnwrapPoint, 2],
                    Part[{ft, Ft}, All, UnwrapPoint, 1]
                }, Nothing],
                If[fTD * FTD <= 0, {
                    Top,
                    Part[FT, UnwrapPoint, 2],
                    Part[{fT, FT}, All, UnwrapPoint, 1]
                }, Nothing],
                If[FtD * FTD <= 0, {
                    Right,
                    Part[FT, UnwrapPoint, 1],
                    Part[{Ft, FT}, All, UnwrapPoint, 2]
                }, Nothing]
            }
        ]
        // Replace[{in:{inSide_, _, _}, out:{outSide_, _, _}} :> Block[
            {
                inZero, outZero
            },

            {inZero, outZero} = {in, out}
                // Map[Apply[
                    findZeros[node, solution, tFlip, ##]&
                ]];

            {
                in // ReplacePart[2 -> inZero],
                out // ReplacePart[2 -> outZero]
            }
            // If[inSide === Bottom &&
                outSide === Top &&
                First[inZero] > First[outZero],
                Reverse,
                Identity
            ]
            // Apply[{<|"In" -> #1, "Out" -> #2, "InternalPoints" -> {}|>}&]
        )]
)


findZeros::noncubic = "Cannot find zeros using cubic fitting for `1`, trying linear"
findZeros::nozeros = "No zeros found for `1`"
findZeros[node_DRNode, solution_Association, tFlip_Association,
    side:(Left|Bottom|Top|Right), fixedCayley_?NumericQ, {min_, max_}
] := Block[
    {
        goalFunction = (
            {fixedCayley, #}&
            /* Replace[side, {
                (Left|Right) -> Identity,
                (*Top|Bottom*)_ -> Reverse
            }]
            /* Apply[realizeNode[node, solution, tFlip, <|
                node["FreeCayley"]
                // Replace[{
                    {firstFree_, ___} :> (firstFree -> #1),
                    {} -> (Nothing)
                }],
                node["TargetCayley"] -> #2
            |>]&]
            /* Replace[{
                coordinates_Association :> dropDiff[node, coordinates],
                err_ :> (Echo[err, "Unknown Result"]; Abort[])
            }]
        )
    },

    (* Finds a zero point of goal function using cubic fitting *)
    Subdivide[min, max, 3]
    // {
        Identity,
        Map[goalFunction]
    } // Through
    // Transpose
    // Replace[{
        samples_?(MatrixRank[#] == 4&) :> Block[
            {
                x, sol
            },

            sol
            /; (
                Check[LinearModelFit[samples, x ^ Range[3], x], Echo[samples]; Abort[]]&
                // NSolve[#["BestFit"] == 0 && min <= x <= max, {x}, Reals]&
                // Map[x/.#&]
                // MinimalBy[goalFunction]
                // Replace[{
                    {root_} :> (sol = root; True),
                    {} :> (Message[findZeros::noncubic, {side, fixedCayley, {min, max}}]; False)
                }]
            )
        ],
        {{x1_, y1_}, _, _, {x2_, y2_}} :> (
            (* linear fit *)
            (x2 * y1 - x1 * y2) / (y1 - y2)
            // Replace[{
                Except[_?(Between[{min, max}])] :> (
                    Message[findZeros::nozeros, {side, fixedCayley, {min, max}}];
                    Abort[]
                )
            }]
        )
    }]
    // {fixedCayley, #}&
    // Replace[side, {
        (Left|Right) -> Identity,
        (*Top|Bottom*)_ -> Reverse
    }]
]


(* mergeZeros[node_DRNode, solution_Association, tFlip_Association, fixedCayley_?NumericQ,
    orientation:(Right|Top), {firstSols_List, secondSols_List}] := Join[firstSols, secondSols] // Flatten // DeleteDuplicates *)

mergeZerosF[firstSols_List, secondSols_List] := Block[
    {
        matchedIndices = matchIntervals[
            firstSols
            // MapIndexed[List /* Replace[{
                {KeyValuePattern["Out" -> {Right, _, interval_}], {index_}} :> (interval -> index),
                _ -> Nothing
            }]] // Merge[Identity] // KeySort,
            secondSols // MapIndexed[List /* Replace[{
                {KeyValuePattern["In" -> {Left, _, interval_}], {index_}} :> (interval -> index),
                _ -> Nothing
            }]] // Merge[Identity] // KeySort
        ] // Reap // Last[Last[#], {}]&
    },

	Join[
        Complement[firstSols // Length // Range, Part[matchedIndices, All, 2, 1] // Catenate]
        // Part[firstSols, #]&,
        Complement[secondSols // Length // Range, Part[matchedIndices, All, 2, 2] // Catenate]
        // Part[secondSols, #]& ,
        matchedIndices
        // Map[Apply[mergeSols[#1, {Part[firstSols, First[#2]], Part[secondSols, Last[#2]]}]&]]
        // Catenate
    ]
]


mergeZerosT[firstSols_List, secondSols_List] := Block[
    {
        matchedIndices1, matchedIndices2
    },

    (* first out, second in *)
    matchedIndices1 = matchIntervals[
        firstSols
        // MapIndexed[List /* Replace[{
            {KeyValuePattern["Out" -> {Top, _, interval_}], {index_}} :> (interval -> index),
            _ -> Nothing
        }]] // Merge[Identity] // KeySort,
        secondSols
        // MapIndexed[List /* Replace[{
            {KeyValuePattern["In" -> {Bottom, _, interval_}], {index_}} :> (interval -> index),
            _ -> Nothing
        }]] // Merge[Identity] // KeySort
    ] // Reap // Last[Last[#], {}]&;

    (* second out, first in *)
    matchedIndices2 = matchIntervals[
        secondSols
        // MapIndexed[List /* Replace[{
            {KeyValuePattern["Out" -> {Bottom, _, interval_}], {index_}} :> (interval -> index),
            _ -> Nothing
        }]] // Merge[Identity] // KeySort,
        firstSols
        // MapIndexed[List /* Replace[{
            {KeyValuePattern["In" -> {Top, _, interval_}], {index_}} :> (interval -> index),
            _ -> Nothing
        }]] // Merge[Identity] // KeySort
    ] // Reap // Last[Last[#], {}]&;

	Join[
        Complement[
            firstSols // Length // Range,
            Join[
                Part[matchedIndices1, All, 2, 1],
                Part[matchedIndices2, All ,2, 2]
            ] // Catenate // DeleteDuplicates
        ] // Part[firstSols, #]&,
        Complement[
            secondSols // Length // Range,
            Join[
                Part[matchedIndices1, All, 2, 2],
                Part[matchedIndices2, All ,2, 1]
            ] // Catenate // DeleteDuplicates
        ] // Part[secondSols, #]&,
        mergeIndices[firstSols, secondSols][matchedIndices1, matchedIndices2]
        // Reap // Last[Last[#], {}]&
    ]
]


matchIntervals[_?(SameAs[<||>]), _?(SameAs[<||>])] := Null

matchIntervals[<|_ -> firstIndices_, rest___|>, _?(SameAs[<||>]), firstMatched_:False] := (
    If[Not[firstMatched],
        Sow[{1, {firstIndices, {}}}]
    ];
    matchIntervals[<|rest|>, <||>]
)

matchIntervals[_?(SameAs[<||>]), <|_ -> secondIndices_, rest___|>, _:False, secondMatched_:False] := (
    If[Not[secondMatched],
        Sow[{2, {{}, secondIndices}}]
    ];
    matchIntervals[<||>, <|rest|>]
)

matchIntervals[first:<|firstInterval_ -> firstIndices_, ___|>, second:<|secondInterval_ -> secondIndices_, ___|>,
    firstMatched_:False, secondMatched_:False] := (
    RangeIntersection[firstInterval, secondInterval]
    // Replace[{
        {Infinity, -Infinity} :> If[Max[firstInterval] < Max[secondInterval],
            If[Not[firstMatched],
                Sow[{1, {firstIndices, {}}}]
            ];
            matchIntervals[Rest[first], second],
            If[Not[secondMatched],
                Sow[{2, {{}, secondIndices}}]
            ];
            matchIntervals[first, Rest[second]]
        ],
        _ :> (
            Sow[{
                If[{firstInterval, secondInterval}
                    // Map[Apply[EuclideanDistance]]
                    // Apply[Less],
                    1,2
                ],
                {firstIndices, secondIndices}
            }];
            If[Max[firstInterval] < Max[secondInterval],
                matchIntervals[Rest[first], second, False, True],
                matchIntervals[first, Rest[second], True]
            ]
        )
    }]
)


mergeIndices[firstSols_List, secondSols_List, prevUps_Association:<||>, prevDowns_Association:<||>][{}, {}] = Null;

mergeIndices[firstSols_List, secondSols_List, prevUps_Association:<||>, prevDowns_Association:<||>][ups_List, downs_List] := Block[
    {
        selectUpQ,
        upIndex, outUpIndices, inUpIndices,
        downIndex, outDownIndices, inDownIndices,
        inSols, newSols
    },

    selectUpQ = Less[
        If[Length[ups] > 0,
            {upIndex, {outUpIndices, inUpIndices}} = First[ups];
            If[upIndex == 1,
                Part[firstSols, First[outUpIndices], Key["Out"]],
                Part[secondSols, First[inUpIndices], Key["In"]]
            ] // Part[#, 2, 1]&,
            Infinity
        ],
        If[Length[downs] > 0,
            {downIndex, {outDownIndices, inDownIndices}} = First[downs];
            If[downIndex == 1,
                Part[secondSols, First[outDownIndices], Key["Out"]],
                Part[firstSols, First[inDownIndices], Key["In"]]
            ] // Part[#, 2, 1]&,
            Infinity
        ]
    ];

    If[selectUpQ,
        inSols = Part[secondSols, inUpIndices];
        newSols = prevDowns[outUpIndices]
            // Replace[_?MissingQ :> Part[firstSols, outUpIndices]]
            // mergeSols[upIndex, {#, inSols}]&;
        If[inUpIndices =!= {} && Part[inSols, 1, Key["Out"], 1] === Bottom,
            Merge[{prevUps, <|inUpIndices -> newSols|>}, Catenate],
            newSols // Map[Sow];
            prevUps
        ] // mergeIndices[firstSols, secondSols, #, prevDowns][Rest[ups], downs]&,
        inSols = Part[firstSols, inDownIndices];
        newSols = prevUps[outDownIndices]
            // Replace[_?MissingQ :> Part[secondSols, outDownIndices]]
            // mergeSols[downIndex, {#, inSols}]&;
        If[inDownIndices =!= {} && Part[inSols, 1, Key["Out"], 1] === Top,
            Merge[{prevDowns, <|inDownIndices -> newSols|>}, Catenate],
            newSols // Map[Sow];
            prevDowns
        ] // mergeIndices[firstSols, secondSols, prevUps, #][ups, Rest[downs]]&
    ]
    /; (Length[ups] + Length[downs] > 0)
]


mergeSols[_, {{}, inSols:{__Association}}] := Table[
    <|
        "In" -> (inSol["In"] // ReplacePart[1 -> Center]),
        "Out" -> inSol["Out"],
        "InternalPoints" -> Join[Part[inSol["In"], {2}], inSol["InternalPoints"]]
    |>, {inSol, inSols}
]

mergeSols[_, {outSols:{__Association}, {}}] := Table[
    <|
        "In" -> outSol["In"],
        "Out" -> (outSol["Out"] // ReplacePart[1 -> Center]),
        "InternalPoints" -> Join[outSol["InternalPoints"], Part[outSol["Out"], {2}]]
    |>, {outSol, outSols}
]

mergeSols[index_, {outSols:{__Association}, inSols:{__Association}}] := Table[
    <|
        "In" -> outSol["In"],
        "Out" -> inSol["Out"],
        "InternalPoints" -> Join[
            outSol["InternalPoints"],
            Part[{outSol["Out"], inSol["In"]}, index, {2}],
            inSol["InternalPoints"]
        ]
    |>, {outSol, outSols}, {inSol, inSols}
] // Catenate


FinalizeSol[KeyValuePattern[{
    "In" -> {inDirection_, inPoint_, _},
    "Out" -> {outDirection_, outPoint_, _},
    "InternalPoints" -> sol_
}]] := With[
    {
        points = Join[
            If[inDirection === Center, {}, {inPoint}],
            sol,
            If[outDirection === Center, {}, {outPoint}]
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