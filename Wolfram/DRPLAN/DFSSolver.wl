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


BeginPackage["DRPLAN`DFSSolver`", {"DRPLAN`Solver`", "DRPLAN`Core`"}]
ClearAll[Evaluate[Context[] <> "*"]]


DFSSolvingStart::usage = "DFSSolvingStart[root_DRNode] solves in DFS manner and pause when a solution is found."
DFSSolvingContinue::usage = "DFSSolvingContinue[] continues the DFS paused last time." 
DFSSolution::usage = "An object that contains the information for a DFS solution."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["Parallel`Queue`LIFO`"]


StackSerialize[stack_?qQ] := SerializedQueue @@ stack

StackDeserialize[stack_SerializedQueue] := Copy[Parallel`Queue`LIFO`Private`queue @@ stack] 

StackSave[stack_?qQ, path_] := Block[
    {
        serializedQueue
    },
    serializedQueue = StackSerialize[stack];
    DumpSave[path, serializedQueue];
]

StackLoad[path_] := Block[
    {
        serializedQueue
    },

    Get[path];
    StackDeserialize[serializedQueue]
]


(* ::Subsection:: *)
(*DFS Solving*)


Options[DFSSolvingStart] = {
    "DumpPath" -> "DFS_Solution_Stack.mx",
    "StopAtSolution" -> False
}

Options[DFSSolvingContinue] = {
    "DumpPath" -> "DFS_Solution_Stack.mx",
    "StopAtSolution" -> False
}

(* Solve all offsets *)
DFSSolvingStart[root_DRNode, o:OptionsPattern[]] := Module[
    {
        stack = LIFOQueue[],
        (* options *)
        dumpPath, stopAtSolution
    },

    {dumpPath} = OptionValue[DFSSolvingStart, {o}, {"DumpPath"}];

    SolveAllLeaves[stack, root];

    StackSave[stack, dumpPath];

    DFSSolvingContinue[root, o]

]

DFSSolvingContinue::nstart = "The solving for `1` has not start yet, please call DFSSolveStart[`1`]."
DFSSolvingContinue[root_DRNode, o:OptionsPattern[]] := Module[
    {
        serializedQueue, rootSolutionQ = False,
        (* options *)
        dumpPath, stopAtSolution
    },

    {dumpPath, stopAtSolution} = OptionValue[DFSSolvingStart, {o}, {"DumpPath", "StopAtSolution"}];

    If[FileExistsQ[dumpPath] && !DirectoryQ[dumpPath],
        stack = StackLoad[dumpPath],
        Message[DFSSolvingContinue::nstart, root];
        Abort[]
    ];

    While[Size[stack] > 0 && (!stopAtSolution || !rootSolutionQ),
        rootSolutionQ = SolveOneFlip[stack, DeQueue[stack]];
        NotebookDelete[$tempPrint];
        $tempPrint = PrintTemporary[
            "Current Queue Size: ", Size[stack], "\n",
            "Current Solution Size: ", Length[root["DFSSolutions"]]
        ];
        AbortProtect[
            StackSave[stack, dumpPath];
        ]
    ]

]


SolveAllLeaves[stack_?qQ, node_DRNode] := Module[
    {
        cayleyVertex, rootgraph
    },

    If[node["IsCayleyNode"],
        rootgraph = node["Root"]["Graph"];
        cayleyVertex = Max @@ (Part[
            EdgeList[rootgraph],
            node["TargetCayley"]
        ]);
        node["DFSSolutions"] = {
            DFSSolution[
                NodeSolution[
                    <|node["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))|>, (* identity function *)
                    <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
                    {1}, (* D-flip index *)
                    <||> (* overwriting C-flip*)
                ], (* One DFlip *)
                node (* corresponding node *)
            ],
            DFSSolution[
                NodeSolution[
                    <|node["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))|>, (* identity function *)
                    <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
                    {1}, (* D-flip index *)
                    <|cayleyVertex -> !PropertyValue[{rootgraph, cayleyVertex}, "Flip"]|> (* overwriting the C-flip*)
                ], (* One DFlip *)
                node (* corresponding node *)
            ]
        };

        (* tell its parents that new DFS solutions are born *)
        notifyParents[stack, node],

        (* Not a Cayley node *)
        If[!MissingQ[node["Solutions"]],
            (* Memoization *)
            node["DFSSolutions"] = Table[
                DFSSolution[nodeSolution, node],
                {nodeSolution, node["Solutions"]}
            ];
            notifyParents[stack, node];
            Return[]
        ];

        Table[
            subNode["Parents"] = Union[subNode["Parents"], {node}];
            SolveAllLeaves[stack, subNode],
            {subNode, node["SubNodes"]}
        ];
    ]

]


SolveOneFlip[
    stack_?qQ,
    DFSSolution[
        nodeSolution_NodeSolution,
        node_DRNode
    ]
] := Module[
    {
        oldDFSSolutions
    },


    If[node["Root"] === node,
        oldDFSSolutions = node["DFSSolutions"]
    ];

    node["DFSSolutions"] = (*Echo[#, "DFSSolutions"]& @*) Table[
        DFSSolution[solution, node],
        {solution, Flatten[DRPLAN`Solver`Private`SolveDFlip[node, nodeSolution]]}
    ];

    If[Length[node["DFSSolutions"]] > 0,
        notifyParents[stack, node];
    ];
    If[node["Root"] === node,
        (* Append new root solutions *)
        node["DFSSolutions"] = Join[
            oldDFSSolutions,
            Part[node["DFSSolutions"], All, 1]
        ],
        (* Clear EnQueued solutions for non-root nodes *)
        node["DFSSolutions"] = {}
    ];
    node["Root"] === node

]


notifyParents[stack_?qQ, node_DRNode] := (

    If[node["Root"] === node,
        (* root is solved *)
        (* Echo@Table[
            Replace[dfsSolution,
                DFSSolution[nodeSolution_NodeSolution, _DRNode] :> 
                    DRPLAN`Solver`Private`ToPlanSolution /@ nodeSolution
            ],
            {dfsSolution, node["DFSSolutions"]}
        ] // Flatten, *)
        (* Do nothing *)
        Return[],

        (* add to the priority queue*)
        Fold[EnQueue, stack, 
            Table[Outer[
                mergeDFSSolution[parent, ##]&,
                Sequence @@ Through[parent["SubNodes"]["DFSSolutions"]]
                ], {parent, node["Parents"]}
            ] // Flatten // Reverse (* Reverse the enqueue order in order to solve in order *)
        ]

    ]
)


mergeDFSSolution[node_DRNode, dfsSolutions__DFSSolution] := With[
    {
        dfsSolutionList = List @@@ {dfsSolutions}
    },

    DFSSolution[
        DRPLAN`Solver`Private`mergeNodeSolution @@ Part[dfsSolutionList, All, 1],
        node
    ]

]


End[]


EndPackage[]