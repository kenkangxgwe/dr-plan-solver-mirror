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
(*DRPLAN OffsetSolver*)


BeginPackage["DRPLAN`OffsetSolver`", {"DRPLAN`Solver`"}]
ClearAll[Evaluate[Context[] <> "*"]]


SolveAllOffsetsStart::usage = "SolveAllOffsetsStart[root_DRNode, offset] starts a time-consuming search for different dropped edge offsets \
and pause until it found one solution."
SolveAllOffsetsContinue::usage = "SolveAllOffsetsContinue[] continues the search that SolveAllOffsetsStart started last time." 
OffsetSolution::usage = "An object that contains the information for a offset solution."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["Parallel`Queue`Priority`"]


(* ::Subsection:: *)
(*solve offsets*)


OffsetSolution /: MakeBoxes[OffsetSolution[nodeSolutions:{__NodeSolution}, offsets_Association, node_DRNode], StandardForm] := 
Construct[MakeBoxes, OffsetSolution[Panel[Grid[{
    {"NodeSolution:", SpanFromLeft},
    {Panel[Column @ Normal @ nodeSolutions], SpanFromLeft},
    {"Offsets:", offsets},
    {"Node", node}
}], Alignment -> {{Left, Left}}]], StandardForm]


(* implement comparison operators for offset priority value *)

OffsetPV /: Greater[OffsetPV[p11_, p12_, p13_], OffsetPV[p21_, p22_, p23_]] :=
    If[p11 == p21,
        If[p12 == p22,
            p13 > p23,
            p12 > p22
        ],
        p11 > p21
    ]

OffsetPV /: GreaterEqual[OffsetPV[p11_, p12_, p13_], OffsetPV[p21_, p22_, p23_]] :=
    If[p11 == p21,
        If[p12 == p22,
            p13 >= p23, (* differs from above *)
            p12 > p22
        ],
        p11 > p21
    ]


(* the priority function *)
OffsetSolution /: Priority[offsetSolution_OffsetSolution] := Module[
    {
        dFlips, offsetsNum, diverseHeight, totalHeight
    },

    dFlips = Part[offsetSolution, (*NodeSolutions*) 1, All, (*D-Flips*) 3];
    diverseHeight = ChainDiverseLevel[dFlips];
    totalHeight = Depth[First[dFlips]];
    offsetsNum = Part[offsetSolution, (*Offsets*) 2] // Length;

    (* Main priority: the earlier whose D-Flips diverse, the higher priority it has *)
    (* Tie breaker 1: the longer whose D-Flips are, the higher priority it has *)
    (* Tie breaker 2: The less whose dropped edges offsetted, the higher priority it has *)
    OffsetPV[-diverseHeight, totalHeight, -offsetsNum]

]


(*
    The function finds the earliest level where at least two of the D-Flips differ from each other
    It only works for DR-Chain, i.e., the DR-Plan is a chain ignoring all Cayley nodes.
*)
ChainDiverseLevel[dFlips_List] := ChainDiverseLevelImpl[dFlips, 0]
ChainDiverseLevelImpl[{}, height_Integer] := height
ChainDiverseLevelImpl[dFlips_List, height_Integer] := ChainDiverseLevelImpl[
    If[Length[First[dFlips]] == 1,
        (* reaches the leave, terminate recursion *)
        {},
        (*
            Recursively look for its left child.
            In this case, make sure that the right child is always a Cayley node.
        *)
        Part[dFlips, All, 2]
    ],
    If[Equal @@ Part[dFlips, All, 1],
        (*
            The D-Flips are same at this level,
            increase the height
        *)
        height + 1,
        (*
            At least two of them are different,
            reset the height
        *)
        0
    ]
]


SolveAllOffsetsContinue::nstart = "The solving for `1` has not start yet, please call SolveAllOffsetsStart[`1`, offsets]."
SolveAllOffsetsContinue[node_DRNode] := Message[SolveAllOffsetsContinue::nstart, node] 
(* Solve all offsets *)
SolveAllOffsetsStart[root_DRNode, offsets:{__?NumericQ}] := With[
    {
        offsetPQ = priorityQueue[]
    },

    SolveAllLeaves[offsetPQ, root];

    SolveAllOffsetsContinue[root] := (
        While[!SolveOneOffset[offsetPQ, DeQueue[offsetPQ], offsets],
            Echo[Size[offsetPQ], "Current Queue Size"]
        ];
        root["OffsetSolutions"]
    );

    SolveAllOffsetsContinue[root]
]


SolveAllLeaves[offsetPQ_?qQ, node_DRNode] := (

    If[node["IsCayleyNode"],
        node["OffsetSolutions"] = {OffsetSolution[
            {NodeSolution[
                <|node["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))|>, (* identity function *)
                <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
                {1}, (* D-flip index *)
                <||> (* overwriting C-flip*)
            ]}, (* One DFlip *)
            <||>, (* No Offsets *)
            node (* corresponding node *)
        ]};

        (* tell its parents that a new offset solution is born *)
        notifyParents[offsetPQ, node],

        Table[
            subNode["Parents"] = Union[subNode["Parents"], {node}];
            SolveAllLeaves[offsetPQ, subNode],
            {subNode, node["SubNodes"]}
        ];
    ]

)


SolveOneOffset[
    offsetPQ_?qQ,
    OffsetSolution[
        nodeSolutions:{__NodeSolution},
        dropOffsets_Association,
        node_DRNode
    ],
    offsets:{__?NumericQ}
] := Module[
    {
        newDropOffsets, oldOffsetSolutions
    },

    oldOffsetSolutions = node["OffsetSolutions"];
    node["OffsetSolutions"] = Echo[#, "OffsetSolutions"]& @ Table[
        newDropOffsets = If[dropOffset == 1,
            dropOffsets,
            Append[dropOffsets, node["TargetDrop"] -> dropOffset]
        ];
        Table[
            DRPLAN`Solver`Private`SolveDFlip[node, nodeSolution, dropOffset],
            {nodeSolution, nodeSolutions}
        ] // Flatten // Replace[{
            {} :> Nothing,
            ns:{__NodeSolution} :> OffsetSolution[ns, newDropOffsets, node],
            err_ :> Throw[{"Unknown Result", err}]
        }],

        {dropOffset, offsets}
    ];

    notifyParents[offsetPQ, node];
    node["OffsetSolutions"] = Join[oldOffsetSolutions, node["OffsetSolutions"]];
    node["Root"] === node

]


notifyParents[offsetPQ_?qQ, node_DRNode] :=
If[node["Root"] === node,
    (* root is solved *)
    Table[
        Replace[offsetSolution,
            OffsetSolution[nodeSolutions:{__NodeSolution}, offsets_Association, _DRNode] :> 
                OffsetSolution[getPlanSolution /@ nodeSolutions, offsets, node]
        ],
        {offsetSolution, node["OffsetSolutions"]}
    ],

    (* add to the priority queue*)
    Fold[EnQueue, offsetPQ, 
        Table[Outer[
            mergeOffsetSolution[parent, ##]&,
            Sequence @@ Through[parent["SubNodes"]["OffsetSolutions"]]
            ], {parent, node["Parents"]}
        ] // Flatten
    ]
]


mergeOffsetSolution::difofst = "Different offsets are specified for the same dropped edge."
(* mergeOffsetSolution[node_DRNode, offsetSolutions:PatternSequence[{___OffsetSolution}..]] := 
    Outer[mergeOffsetSolution, offsetSolutions] *)
mergeOffsetSolution[node_DRNode, offsetSolutions__OffsetSolution] := With[
    {
        offsetSolutionList = List @@@ {offsetSolutions}
    },

    OffsetSolution[
        DRPLAN`Solver`Private`mergeNodeSolution @@ Part[offsetSolutionList, All, 1],
        Part[offsetSolutionList, All, 2]
        // Merge[If[SameQ@@#, First[#], Message[mergeOffsetSolution::difofst]; Abort[]]&],
        node
    ]

]


End[]


EndPackage[]