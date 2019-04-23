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
(*DRPLAN Visualization*)


BeginPackage["DRPLAN`Visualizer`"]
ClearAll[Evaluate[Context[] <> "*"]]


PrintDRPlan::usage = "PrintDRPlan[node_DRNode] prints the freeCayley parameters at each node."
AnalyzeSolution::usage = "AnalyzeSolution[node_DRNode, cayleyLength_Association] gives the result graph and errors of each non-partial edge."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DRPLAN`Core`"]
Needs["DRPLAN`Solver`"]


(* ::Section:: *)
(*Visualization*)


DRNodeVRFunc[type_, Dynamic[selectedNode_]][rk_, vk_DRNode] := Block[
    {
    },

    Inset[Setter[Dynamic[selectedNode], vk, First[vk]], rk]
    (* Inset[Button[Column[{
        If[type === "Graph",
           HighlightNode[vk],
           Column @ (ToString/@{
               vk,
               {#, vk[#]}&["TargetCayley"],
               {#, vk[#]}&["FreeCayley"],
               {#, vk[#]}&["AllCayley"]
            })
        ],
        If[vk["IsCayleyNode"],
            Column @ (ToString/@{
                EdgeList[vk["Root"]["Graph"]][[vk["TargetCayley"]]],
                vk["Interval"]
            }),
            Nothing
        ]
    }],
    selectedNode = vk, Appearance->None], rk] *)
]


PrintDRPlan[node_DRNode]:= DynamicModule[
    {
    },

    Manipulate[
        With[{fn = Curry[DRNodeVRFunc][Dynamic[selectedNode]]},
        Row[{
            Column[{
                Grid[{{
                    (* openerView[node, Dynamic[selectedNode]], *)
                    LayeredGraphPlot[
                        node["EdgeRules"],
                        Top,
                        VertexRenderingFunction -> fn[nodeType],
                        EdgeRenderingFunction -> ({Dashed, Opacity[.5], Line[#1]}&),
                        ImageSize -> 100
                    ],
                    Column[{
                        HighlightNode[selectedNode],
                        Grid[{
                            {"Target Cayley", Part[EdgeList[selectedNode["Root"]["Graph"]], selectedNode["TargetCayley"]]},
                            {"Free Cayleys", Row[Part[EdgeList[selectedNode["Root"]["Graph"]], selectedNode["FreeCayley"]], ","]},
                            {"All Cayleys", Pane[Row[Part[EdgeList[selectedNode["Root"]["Graph"]], selectedNode["AllCayley"]], ","], 300]},
                            If[selectedNode["IsCayleyNode"],
                                {"Interval", MinMax[selectedNode["Interval"]]},
                                Nothing
                            ],
                            If[!MissingQ[selectedNode["Solutions"]],
                                {"Solutions", selectedNode["Solutions"]},
                                Nothing
                            ]
                        }, Alignment -> {{Right, Left}, Top}]
                    }],
                    (* TreePlot[
                        node["EdgeRules"],
                        Top,
                        node,
                        AspectRatio -> .5,
                        VertexRenderingFunction -> fn[nodeType],
                        EdgeRenderingFunction -> ({Dashed, Opacity[.5], Line[#1]}&),
                        ImageSize -> Large
                    ], *)

                    (* If[selectedNode =!= Null,
                        Column[{
                            If[trackQ, 
                                trackNode[selectedNode],
                                Nothing
                            ],
                            If[solveQ,
                                SolveNode[selectedNode],
                                Nothing
                            ]
                            (*Dynamic[OpenerView[{"SamplePoints", Reap[selectedNode["SolveNode"[]]]}, Method \[Rule] "Active"]]*)
                        }],
                        Nothing
                    ] *)
                    Nothing
                }}, Alignment -> Center]
            }]
        }]]
        ,
        (* {{nodeType, "Graph", "Node Type"}, {"Graph", "Text"}}, *)
        (* {{selectedNode, Null, "Selected Node"}, (Row[{#1, " ", Button["Reset", selectedNode = Null]}]&)}, *)
        {{selectedNode, node, "Selected Node"}},
        {{trackQ, False, "Track Node?"}, {True, False}},
        {{solveQ, False, "Solve Node?"}, {True, False}}
    ]
]


openerView[node_DRNode, Dynamic[selectedNode_]] := If[node["SubNodes"] == {},
    Row[{Setter[Dynamic[selectedNode], node], ""}],
    OpenerView[
        {
            Row[{Setter[Dynamic[selectedNode], node], ""}],
            Column[Curry[openerView][Dynamic[selectedNode]] /@ node["SubNodes"]]
        },
        True, ImageSize -> All
    ]
]


EdgeColorMap = <|
    "Add" -> Green,
    "Partial" -> Black,
    "Drop" -> Red
|>


HighlightNode[node_DRNode] := Module[
    {
        graph = node["Graph"], rootgraph = node["Root"]["Graph"],
        edgeStyle
    },

    edgeStyle = Style[#, EdgeColorMap[PropertyValue[{rootgraph, #}, "EdgeType"]]]& /@ EdgeList[graph];
    HighlightGraph[
        rootgraph,
        EdgeList[node["Graph"]],
        GraphHighlightStyle -> "DehighlightFade",
        ImageSize -> 400
	  ]
]


(* ::Subsection:: *)
(*trackNode*)


trackNode[node_DRNode] := Module[
    {
        nodeSolutions, testFunc
    },

    If[node =!= Null && !node["IsCayleyNode"],
       nodeSolutions = mergeNodeSolution @@ (SolveNode /@ node["SubNodes"]);
       Row[Panel[Column[{ToString[#], AnalyzeNode[node, #]}, Center]]& /@ nodeSolutions]
    ]
]


modifyGraph[graph_]:=Module[
	  {

    },

    (*VertexDelete[graph,5] // EdgeAdd[#,1\[UndirectedEdge]6]&*)
    graph
]


NodeManipulateRenderingFunction[node_DRNode, nodeSolution_NodeSolution, freeCayleys_Association] := Module[
    {
        solution, graph, cayleyLength, cayleyEdges, droppedEdge, visualCayleyLength
    },

    cayleyLength = (#[freeCayleys]&) /@ First[nodeSolution];
    graph = node["Realize"[PlanSolution[cayleyLength, <||>]]];
    If[FailureQ[graph], Return[$Failed]];

    cayleyEdges = Flatten @ {
        Part[EdgeList[node["Root"]["Graph"]], node["FreeCayley"]],
        Part[EdgeList[node["Root"]["Graph"]], node["TargetCayley"]]
    };
    droppedEdge = Part[EdgeList[node["Root"]["Graph"]], node["TargetDrop"]];

    graph = Fold[SetProperty[{#1, #2}, EdgeStyle -> Green]&, graph, cayleyEdges];
    graph = SetProperty[{graph, droppedEdge}, EdgeStyle -> Red];

    visualCayleyLength = KeyMap[Part[EdgeList[node["Root"]["Graph"]], #]&, cayleyLength];

    Column[{modifyGraph[graph], visualCayleyLength, dropDiff[node, graph]}]
]


AnalyzeNode[node_DRNode, nodeSolution_NodeSolution] := Module[
    {
        domain, cayleys, vars, labels, mins, maxs, refinedDomain
    },

    domain = Part[nodeSolution, 2];
    cayleys = Append[node["FreeCayley"], node["TargetCayley"]];
    labels = ("c" <> ToString[#]&) /@ cayleys;
    vars = Unique /@ labels;
    {mins, maxs} = Transpose @ (MinMax /@ (Lookup[cayleys]@ domain));
    (* refinedDomain = IntervalIntersection[
        Domain[Last[cayleys]],
        refineInterval[node, Most[cayleys],
            (* we require the target cayley parameter not appears in its prior vertices' solutions *)
            (#[<|Thread[Most[cayleys] -> Most[vars]]|>]&) /@ KeySelect[Solution, (# < Most[cayleys]&)]
        ]
    ];
    AppendTo[mins, Min[refinedDomain]];
    AppendTo[maxs, Max[refinedDomain]]; *)
    With[
        {
            constNodeSolution = nodeSolution,
            constCayleys = cayleys,
            constNode = node,
            constVars = vars,
            controls = Sequence @@ MapThread[{{#1, #3, #2}, #3, #4}&, {vars, labels, mins, maxs}]
        },

        (* Abort[]; *)
        (* Manipulate[NodeManipulateRenderingFunction[node, value], controls] *)
        Manipulate[
            NodeManipulateRenderingFunction[
                constNode,
                constNodeSolution,
                Association[Thread[constCayleys -> constVars]]
            ],
           controls
        ]
    ]
]


AnalyzeSolution[node_DRNode, cayleyLength_Association] :=
    AnalyzeSolution[node, {cayleyLength, <||>}]
AnalyzeSolution[node_DRNode, planSolution_PlanSolution] := Module[
    {
        originGraph, resultGraph
    },

    originGraph = node["Root"]["Graph"];
    resultGraph = node["Root"]["Realize"[planSolution]];
    Row @ {
        Graph[resultGraph, Options[originGraph, EdgeStyle], ImageSize -> 400],
        Grid[({
            #1 \[UndirectedEdge] #2,
            PropertyValue[{originGraph, #1 \[UndirectedEdge] #2}, EdgeStyle],
            Chop[EuclideanDistance[
                PropertyValue[{resultGraph, #1}, VertexCoordinates], 
                PropertyValue[{resultGraph, #2}, VertexCoordinates]
            ] - EuclideanDistance[
                PropertyValue[{originGraph, #1}, VertexCoordinates], 
                PropertyValue[{originGraph, #2}, VertexCoordinates]
            ]],
            Row @ {(Chop[EuclideanDistance[
                PropertyValue[{resultGraph, #1}, VertexCoordinates], 
                PropertyValue[{resultGraph, #2}, VertexCoordinates]
            ] / EuclideanDistance[
                PropertyValue[{originGraph, #1}, VertexCoordinates], 
                PropertyValue[{originGraph, #2}, VertexCoordinates]
            ]] - 1) * 100,
            "%"
            }
        } &) @@@ Select[EdgeList[originGraph], PropertyValue[{originGraph, #}, "EdgeType"] =!= "Partial"&]
        , Alignment -> {{"\[UndirectedEdge]", Center, ".", "."}, Baseline}]
    }
]


End[]


EndPackage[]