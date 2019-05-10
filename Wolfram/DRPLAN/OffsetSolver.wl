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


BeginPackage["DRPLAN`OffsetSolver`", {"DRPLAN`Solver`", "DRPLAN`Core`"}]
ClearAll[Evaluate[Context[] <> "*"]]


SolveAllOffsetsStart::usage = "SolveAllOffsetsStart[root_DRNode, offset] starts a time-consuming search for different dropped edge offsets \
and pause until it found one solution."
SolveAllOffsetsContinue::usage = "SolveAllOffsetsContinue[] continues the search that SolveAllOffsetsStart started last time." 
OffsetSolution::usage = "An object that contains the information for a offset solution."
ToPlanSolution::usage = StringJoin[ToPlanSolution::usage, "\n",
    "ToPlanSolution[offsetSolution_OffsetSolution] turns a offset solution for the root node to a list of plan solutions."
]


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["Parallel`Queue`Priority`"]


QueueSerialize[queue_?qQ] := SerializedQueue @@ queue

QueueDeserialize[queue_SerializedQueue] := Copy[Parallel`Queue`Priority`Private`queue @@ queue]


$ConfigPath = "Offset_Solution_PQueue.mx"

(* Block must be used here *)
ConfigSave[queue_?qQ, options:{OptionsPattern[]}, path_String] := Block[
    {
        serializedQueue, solvingOptions
    },
    serializedQueue = QueueSerialize[queue];
    solvingOptions = options;
    DumpSave[path, {serializedQueue, solvingOptions}];
]

ConfigLoad::invcfg = "Invalid config file at `1`"
(* Block must be used here *)
ConfigLoad[path_String] := Block[
    {
        (* Variable names must be same as in ConfigSave *)
        serializedQueue, solvingOptions
    },

    Get[path];
    {
        QueueDeserialize[serializedQueue],
        solvingOptions
    } // Replace[{
        Except[{_?qQ, OptionsPattern[]}] :> (
            Message[ConfigLoad::invcfg, path];
            Abort[]
        )
    }]
]


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


(* Extends DRPLAN`Solver`ToPlanSolution *)
ToPlanSolution[OffsetSolution[{nodeSolutions__NodeSolution}, __]] :=
    ToPlanSolution /@ {nodeSolutions}


Options[SolveAllOffsetsStart] = {
    "ConfigPath" :> $ConfigPath,
    "StopAtSolution" -> True,
    "AllCFlip" -> False,
    "Offsets" -> {1} (* used in SolveAllOffsetsContinue *)
}

Options[SolveAllOffsetsContinue] = {
    "ConfigPath" :> $ConfigPath,
    "StopAtSolution" -> True
}

(* Solve all offsets *)
SolveAllOffsetsStart[root_DRNode, offsets:{__?NumericQ}, o:OptionsPattern[]] := Module[
    {
        offsetPQ = priorityQueue[],
        (* options *)
        dumpPath
    },

    {dumpPath} = OptionValue[SolveAllOffsetsStart, {o}, {"ConfigPath"}];

    SolveAllLeaves[offsetPQ, root, FilterRules[{o}, Options[SolveAllLeaves]]];

    ConfigSave[offsetPQ, {"Offsets" -> offsets, o}, dumpPath];

    SolveAllOffsetsContinue[root]
]


SolveAllOffsetsContinue::nstart = "The solving for `1` has not start yet, please call SolveAllOffsetsStart[`1`, offsets]."
SolveAllOffsetsContinue[root_DRNode, o:OptionsPattern[]] := Module[
    {
        offsetPQ, rootSolutionQ = False, solvingOptions, offsets, stopAtSolution,
        (* options *)
        dumpPath
    },

    {dumpPath} = OptionValue[SolveAllOffsetsContinue, {o}, {"ConfigPath"}];

    {offsetPQ, solvingOptions} = ConfigLoad[dumpPath];

    {offsets, stopAtSolution} = OptionValue[SolveAllOffsetsStart, {solvingOptions}, {"Offsets", "StopAtSolution"}];

    (* Overwrite options from SolveAllOffsetsStart *)
    FilterRules[{o}, "StopAtSolution"]
    // Replace[{"StopAtSolution" -> stop_?BooleanQ} :> (stopAtSolution = stop)];

    While[Size[offsetPQ] > 0 && (!stopAtSolution || !rootSolutionQ),
        rootSolutionQ = SolveOneOffset[offsetPQ, DeQueue[offsetPQ], offsets];
        NotebookDelete[$tempPrint];
        $tempPrint = PrintTemporary[
            "Current Queue Size: ", offsetPQ, "\n",
            "Current Solution Size: ", Length[root["OffsetSolutions"]]
        ];
        AbortProtect[
            ConfigSave[offsetPQ, solvingOptions, dumpPath];
        ]
    ]

]


Options[SolveAllLeaves] = {
    "AllCFlip" -> False
}

SolveAllLeaves[offsetPQ_?qQ, node_DRNode, o:OptionsPattern[]] := Module[
    {
        cayleyVertex, rootgraph, allCFlip
    },

    {allCFlip} = OptionValue[SolveAllLeaves, {o}, {"AllCFlip"}];

    If[node["IsCayleyNode"],
        rootgraph = node["Root"]["Graph"];
        cayleyVertex = Max @@ (Part[
            EdgeList[rootgraph],
            node["TargetCayley"]
        ]);
        node["OffsetSolutions"] = {OffsetSolution[
            {NodeSolution[
                <|node["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))|>, (* identity function *)
                <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
                {1}, (* D-flip index *)
                <||> (* overwriting C-flip*)
            ],
            If[allCFlip,
                NodeSolution[
                    <|node["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))|>, (* identity function *)
                    <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
                    {1}, (* D-flip index *)
                    <|cayleyVertex -> !PropertyValue[{rootgraph, cayleyVertex}, "Flip"]|> (* overwriting the C-flip*)
                ],
                Nothing
            ]}, (* One DFlip *)
            <||>, (* No Offsets *)
            node (* corresponding node *)
        ]};

        (* tell its parents that a new offset solution is born *)
        notifyParents[offsetPQ, node],

        Table[
            subNode["Parents"] = Union[subNode["Parents"], {node}];
            SolveAllLeaves[offsetPQ, subNode, o],
            {subNode, node["SubNodes"]}
        ];
    ]

]


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
    node["OffsetSolutions"] = (*Echo[#, "OffsetSolutions"]& @*) Table[
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