(* ::Package:: *)

(* ::Title:: *)
(*DRPLAN*)


BeginPackage["DRPLAN`"];
ClearAll[Evaluate[Context[] <> "*"]];


(* ::Item:: *)
(*Output Symbols*)


Displacement::usage = "Displacement[node_DRNode, node_DRNode] combines two realizations for the same linkage and draw displancement vectors."
RigidityMatrix::usage = "RigidityMatrix[graph_Graph] returns the rigidity matrix."
InfinitesimallyRigidQ::usage = "InfinitismallyRigidQ[graph_Graph] gives True if the input DRNode is infinitesimally rigid ."
DRNode::usage = "DRNode[$n][\"property\"] returns the specified property of given DRNode[$n]."
NewDRNode::usage = "NewDRNode[dotfile_String] returns the root node of a DR-Plan tree, according to the specified dotfile path."
GenerateDRPlan::usage = "GenerateDRPlan[node_DRNode] constructs the DR-Plan for given root node."
FlipAt::usage = "FlipAt[node_DRNode, vertices_List] flips given vertices in the list for given root node."
PrintDRPlan::usage = "PrintDRPlan[node_DRNode] prints the freeCayley parameters at each node."
SolveDRPlan::usage = "SolveDRPlan[node_DRNode] solves a DRPlan by passing in the root DR-node."
SolveNode::usage = "SolveNode[node_DRNode] solves the input node and its sub-nodes and returns all solutions.
SolveNode[node_DRNode, dFlip:(All | _List)] solves the node only for specified D-flip."
AnalyzeSolution::usage = "AnalyzeSolution[node_DRNode, cayleyLength_Association] gives the result graph and errors of each non-partial edge."


Begin["`Private`"];
ClearAll[Evaluate[Context[] <> "*"]];
Needs["GraphvizUtils`"];


(* ::Section:: *)
(*Utilities*)


(* ::Item:: *)
(*Displacement*)


Options[Displacement] = {"AddEdge" -> True};
Displacement::neq = "Two graphs have different number of vertices.";
Displacement[node1_DRNode, node2_DRNode, OptionsPattern[]] := Module[
    {
        graph1 = node1["Graph"], graph2 = node2["Graph"],
        combinedGraph, coordMap, colorMap, combinedVertexList, combinedEdgeList
    },

    If[Complement @@ (VertexList /@ {graph1, graph2}) =!= {},
       (* Two graphs are not same *)
       Message[Displacement::neq];
       Throw[$Failed]
    ];

    coordMap = (
        (PropertyValue[{graph1, #}, VertexCoordinates]& /@ VertexList[graph1])
        ~Join~
        (PropertyValue[{graph2, #}, VertexCoordinates]& /@ VertexList[graph2])
    );

    If[OptionValue["AddEdge"],
       colorMap = (PropertyValue[{graph1, #}, EdgeStyle]& /@ EdgeList[graph1])
                  ~ Join ~
                  (PropertyValue[{graph2, #}, EdgeStyle]& /@ EdgeList[graph2]),
       graph1 = EdgeDelete[graph1, Select[EdgeList[graph1], (PropertyValue[{graph1, #}, "EdgeType"] == "Add")&]];
       graph2 = EdgeDelete[graph2, Select[EdgeList[graph2], (PropertyValue[{graph2, #}, "EdgeType"] == "Add")&]];
       colorMap = Table[Black, EdgeCount[graph1]] ~ Join ~ Table[Blue, EdgeCount[graph2]]
    ];

    graph1 = VertexReplace[graph1, Table[i -> Subscript["a", i], {i, 0, VertexCount[graph1]}]];
    graph2 = VertexReplace[graph2, Table[i -> Subscript["b", i], {i, 0, VertexCount[graph2]}]];
    combinedVertexList = VertexList[graph1] ~ Join ~ VertexList[graph2];
    combinedEdgeList = Join[
        EdgeList[graph1] ~ Join ~ EdgeList[graph2],
        Thread[UndirectedEdge[VertexList[graph1], VertexList[graph2]]]
    ];
    colorMap = Thread[combinedEdgeList -> PadRight[colorMap, Length[combinedEdgeList], {{RGBColor[0.6, 0.6, 0.6], Dashed}}]];
    combinedGraph = Graph[combinedVertexList, combinedEdgeList, VertexCoordinates -> coordMap, EdgeStyle -> colorMap, VertexLabels -> "Name"]
];


(* ::Item:: *)
(*RigidityMatrix*)


RigidityMatrix[graph_Graph] := Module[
    {
        edgelist, coordMap
    },

    edgelist = EdgeList[graph];
    SparseArray[Join @@ (MapIndexed[RigidityElement[graph], edgelist])]
];


(* ::Item:: *)
(*RigidityElement*)


RigidityElement[graph_][UndirectedEdge[v1_Integer, v2_Integer], {row_Integer}] := Module[
    {
        displacement
    },

    displacement = Subtract @@ PropertyValue[{graph, #}, VertexCoordinates]& /@ {v1, v2};
    {
        {row, 2 * v1 + 1} -> First@displacement,
        {row, 2 * v1 + 2} -> Last@displacement,
        {row, 2 * v2 + 1} -> First@(-displacement),
        {row, 2 * v2 + 2} -> Last@(-displacement)
    }
];


(* ::Item:: *)
(*InfinitesimallyRigidQ*)


InfinitesimallyRigidQ[graph_Graph] := Module[
    {
        rigidityMatrix
    },

    rigidityMatrix = RigidityMatrix[graph];
    (Last@Dimensions[rigidityMatrix] - MatrixRank[rigidityMatrix]) == 3
];


(* ::Chapter:: *)
(*DRNode*)


(* ::Section:: *)
(*Constants*)


(* Some constants are declared here. *)
DRPLAN["EPSILON"] = 2 * 10^(-5);
DRPLAN["DRNodeVSFunc"][{xc_, yc_}, name_, {w_, h_}] := name["Graph"];
DRPLAN["CCW"[p_List, q_List, r__List]] := Det[Append[#, 1] & /@ {p, q, r}];
UnEcho = (#1&);
PrintEcho = ((Print[#1];#1)&);


(* ::Section:: *)
(*Constructor*)


$NodeID = 0 (* A global id for node *)
NodeIDGetNew[] := Symbol["$" <> ToString[($NodeID = $NodeID + 1)]]

(* Constructs a new DRNode *)
Options[NewDRNode] = {
    "ImportRealization" -> True
};
NewDRNode[dotfile_String, o:OptionsPattern[]] := NewDRNode[ImportGraphviz[dotfile, o]];

NewDRNode[graph:(_Graph | _Subgraph)] := Module[
    {
        newNode = DRNode[NodeIDGetNew[]]
    },

    newNode["FreeCayley"] = {};
    newNode["SubNodes"] = {};
    newNode["Graph"] = graph;
    newNode["Root"] = newNode;
    newNode["Solutions"] = Missing["NotSolved"];
    newNode
];


(* ::Section:: *)
(*AddSubNode*)


(* Add a new subnode, containing only the edges.
Thus the graph property should be accessed through root graph. *)
AddSubNode[node_DRNode, vertOrEdges:{(_Integer?NonNegative|_UndirectedEdge)..}] := Module[
    {
        newSubNode
    },

    newSubNode = NewDRNode[Subgraph[node["Graph"], vertOrEdges]];
    newSubNode["Root"] = node["Root"];
    newSubNode
];


(* ::Section:: *)
(*Flip*)


FlipAt[node_DRNode, vertices_List] := Module[
    {
        graph
    },

    graph = node["Graph"];
    (PropertyValue[{graph, #}, "Flip"] = True)& /@ vertices;
    node["Graph"] = graph;
    node
];


(* ::Section:: *)
(*GenerateDRPlan*)


GenerateDRPlan[node_DRNode] := Module[
    {
        soleEdge, freeCayley, subNodes
    },

    If[node["Root"] === node, (*RootQ*)
        calcInterval[node];
    ];

    If[node["IsCayleyNode"] = EdgeCount[node["Graph"]] == 1,
        (* is a cayley node *)
        soleEdge = First[EdgeList[node["Graph"]]];
        node["AllCayley"] = node["FreeCayley"] = {node["TargetCayley"]} = {EdgeIndex[node["Root"]["Graph"], soleEdge]};
        node["Interval"] = (Interval[{Min[#] + DRPLAN["EPSILON"], Max[#] - DRPLAN["EPSILON"]}]&) @ PropertyValue[{node["Root"]["Graph"], soleEdge}, "Interval"];
        node["EdgeRules"] = {},
        (* not a cayley node *)
        {freeCayley, subNodes} = GenerateDRNode[node];
        node["SubNodes"] = node["SubNodes"] ~Join~ subNodes;
        GenerateDRPlan /@ node["SubNodes"];
        node["FreeCayley"] = Union[Flatten[Through[node["SubNodes"]["FreeCayley"]]]];
        If[Length[freeCayley] == 0,
            (* no free cayleys *)
            node["TargetCayley"] = Last[node["FreeCayley"]];
            node["FreeCayley"] = Most[node["FreeCayley"]],
            (* else *)
            node["FreeCayley"] = Union[Join[node["FreeCayley"], Most[freeCayley]]];
            node["TargetCayley"] = Last[freeCayley]
        ];
        subNodes = AddSubNode[node, {EdgeList[node["Root"]["Graph"]][[#]]}]& /@ freeCayley;
        GenerateDRPlan /@ subNodes;
        node["SubNodes"] = node["SubNodes"] ~Join~ subNodes;
        node["AllCayley"] = Union[freeCayley, Union @@ Through[node["SubNodes"]["AllCayley"]]]
    ];
    node["EdgeRules"] = ((node -> #&) /@ node["SubNodes"]) ~Join~ (Join @@ Through[node["SubNodes"]["EdgeRules"]]);
];


(* ::Section:: *)
(*GenerateDRNode*)


(* Count down from the last edge to find a subnode. *)
GenerateDRNode[node_DRNode] := GenerateDRNode[{node, EdgeCount[node["Graph"]], 0}, {{}, {}}];
GenerateDRNode[{node_DRNode, edgeIndex_Integer?NonNegative, dropCounter_Integer?NonNegative}, {freeCayley:{_Integer?Positive...}, subNodes:{_DRNode...}}] := Module[
    {
        graph, rootgraph, curEdge, newSubNodes = subNodes, rootEdgeIndex
    },

    If[edgeIndex == 0 || dropCounter == 2,
       Return[{freeCayley, subNodes}];
    ];

    graph = node["Graph"];
    rootgraph = node["Root"]["Graph"];
    curEdge = EdgeList[graph][[edgeIndex]];
    rootEdgeIndex = EdgeIndex[rootgraph, curEdge];
    Replace[PropertyValue[{rootgraph, curEdge}, "EdgeType"], {
        "Drop" :> (
            Replace[dropCounter, {
                0 :> (
                    node["TargetDrop"] = rootEdgeIndex;
                    node["TargetLength"] = PropertyValue[{rootgraph, curEdge}, EdgeWeight]
                ),
                1 :> AppendTo[newSubNodes, AddSubNode[node, Range[0, Max[{First[curEdge], Last[curEdge]}]]]]
            }];
            GenerateDRNode[{node, edgeIndex - 1, dropCounter + 1}, {freeCayley, newSubNodes}]
        ),
        "Add" :> GenerateDRNode[{node, edgeIndex - 1, dropCounter}, {Prepend[freeCayley, rootEdgeIndex], subNodes}],
        "Partial" :> GenerateDRNode[{node, edgeIndex - 1, dropCounter}, {freeCayley, subNodes}]
    }]
]


(* ::Section:: *)
(*Linear Programming*)


(* ::Subsection:: *)
(*calcInterval*)


(* Calculate the interval from every added edge using linear programming. *)
calcInterval[node_DRNode] := Module[
    {
        graph = node["Graph"], addEdgeIndices, addNum, edgeToCol,
        e, m, b, lu, commonVertex, cons, mins, maxs
    },

    addEdgeIndices = Table[
        If[PropertyValue[{graph, e}, "EdgeType"] == "Add",
           EdgeIndex[graph, e],
           Nothing
        ],
        {e, EdgeList[graph]}
    ];

    addNum = Length[addEdgeIndices];
    edgeToCol = Association[Thread[(addEdgeIndices -> Range[addNum])]];

    {m, b, lu} = ((Flatten[#, 1]&) /@ (Transpose @ Table[
        e = EdgeList[graph][[ei]];
        commonVertex = Intersection[AdjacencyList[graph, First[e]], AdjacencyList[graph, Last[e]]];
        cons = getConstraints[graph, {e, UndirectedEdge[First[e], #], UndirectedEdge[Last[e], #]}, edgeToCol, addNum]& /@ commonVertex;
        (Flatten[#, 1]&) /@ Transpose[cons],
        {ei, Keys[edgeToCol]}
    ]));
    m = Normal /@ m;
    b = List @@@ b;
    lu = lu
         // Merge[(IntervalIntersection @@ #&)]
         // KeyValueMap[({{#1, 1} -> Min[#2], {#1, 2} -> Max[#2]}&)]
         // Flatten
         // (SparseArray[# ~Join~ {{_, 1} -> -Infinity, {_,2} -> Infinity}, {addNum, 2}]&);
    (*Echo@*MatrixForm/@{m, b, lu};*)
    mins = Association[
        (# -> LinearProgramming[SparseArray[{#} -> 1, addNum], m, List @@@ b, List @@@ lu].SparseArray[{#} -> 1, addNum])& /@ Range[addNum]
    ];
    maxs = Association[
        (# -> -LinearProgramming[SparseArray[{#} -> -1, addNum], m, List @@@ b, List @@@ lu].SparseArray[{#} -> -1, addNum])& /@ Range[addNum]
    ];
    (PropertyValue[{graph, EdgeList[graph][[#]]}, "Interval"] = Interval[{mins[edgeToCol[#]], maxs[edgeToCol[#]]}])& /@ Keys[edgeToCol];
	  node["Graph"] =  graph;
	  node
];


(* ::Subsection:: *)
(*getConstraints*)


(* Construct triangular inequalities from one triangle, containing at least one added edge. *)
getConstraints[graph_Graph, {e0_UndirectedEdge, e1_UndirectedEdge, e2_UndirectedEdge}, edgeToCol_Association, addNum_] :=
    Switch[Table[PropertyValue[{graph, e}, "EdgeType"], {e, {e1, e2}}],
           {"Drop", _} | {_, "Drop"},
           Nothing,
           {"Add", "Add"},
           {
               {
                   SparseArray[{
                       edgeToCol[EdgeIndex[graph, e0]] -> -1,
                       edgeToCol[EdgeIndex[graph, e1]] -> 1,
                       edgeToCol[EdgeIndex[graph, e2]] -> 1
                       }, addNum]
               },
               {
                   Boundary[0, 1]
               },
               {}
           },
           {"Add", "Partial"},
           {
               {
                   SparseArray[{
                       edgeToCol[EdgeIndex[graph, e0]] -> 1,
                       edgeToCol[EdgeIndex[graph, e1]] -> -1
                       }, addNum],
                   SparseArray[{
                       edgeToCol[EdgeIndex[graph, e0]] -> 1,
                       edgeToCol[EdgeIndex[graph, e1]] -> 1
                       }, addNum]
               },
               {
                   Boundary[PropertyValue[{graph, e2}, EdgeWeight], -1],
                   Boundary[PropertyValue[{graph, e2}, EdgeWeight], 1]
               },
               {}
           },
           {"Partial", "Add"},
           {
               {
                   SparseArray[{
                       edgeToCol[EdgeIndex[graph, e0]] -> 1,
                       edgeToCol[EdgeIndex[graph, e2]] -> -1
                       }, addNum],
                   SparseArray[{
                       edgeToCol[EdgeIndex[graph, e0]] -> 1,
                       edgeToCol[EdgeIndex[graph, e2]] -> 1
                       }, addNum]
               },
               {
                   Boundary[PropertyValue[{graph, e1}, EdgeWeight], -1],
					         Boundary[PropertyValue[{graph, e1}, EdgeWeight], 1]
				       },
				       {}
			     },
		       {"Partial", "Partial"},
			     {
				       {},
				       {},
				       <| edgeToCol[EdgeIndex[graph, e0]] -> Interval[{
                       Abs[PropertyValue[{graph, e1}, EdgeWeight] - PropertyValue[{graph, e2}, EdgeWeight]],
                       PropertyValue[{graph, e1}, EdgeWeight] + PropertyValue[{graph, e2}, EdgeWeight]
               }]|>
			     }
	  ];


(* ::Section:: *)
(*Print*)


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
];


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
];


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
|>;


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
];


Format[nodeSolution:NodeSolution[solution_Association, domain_Association, dFlip_List]] := Interpretation[
    NodeSolution[Panel[Column[{
        "Solution:",
        Panel[Column @ Normal @ solution],
        "Domain:",
        Panel[Column @ Normal @ domain],
        "DFlip:",
        Panel[dFlip]
    }]]],
    nodeSolution
];


ToString[NodeSolution[solution_Association, domain_Association, dFlip_List]] ^:= StringJoin[
    "NodeSolution:\n",
    "    Solution:\n",
    {"        ", ToString[#], "\n"}& /@ (Normal @ solution),
    "    Domain:\n",
    {"        ", ToString[#], "\n"}& /@ (Normal @ domain),
    "    DFlip:\n",
    "        ", ToString[dFlip]
]



(* ::Section:: *)
(*Realization*)


(* ::Subsection:: *)
(*Realize*)


DRNode[objID_]["Realize"[]] := Module[
    {
        obj = DRNode[objID], cayleyLength, coordsList, tmpSubGraph
    },

    cayleyLength = <| # -> PropertyValue[{obj["Root"]["Graph"], #}, EdgeWeight]& /@ obj["AllCayley"] |>;
    obj["Realize"[cayleyLength]]
];

DRNode[objID_]["Realize"[cayleyLength_Association]] := Module[
    {
        obj = DRNode[objID], coordsList, tmpSubGraph
    },

    Check[
        coordsList = calcCoords[obj, VertexList[obj["Graph"]], cayleyLength];
        Subgraph[obj["Graph"], Keys[coordsList], VertexCoordinates -> Normal@coordsList, VertexLabels->"Name"],
        $Failed
    ]
];


(* ::Subsection:: *)
(*refineInterval*)


refineInterval[node_DRNode, TargetCayley_, CayleyLength_Association] := Module[
    {
        graph = node["Graph"], rootgraph = node["Root"]["Graph"], targetedge,
        v1, v2, commonvertex, d1, d2
    },

    targetedge = EdgeList[rootgraph][[TargetCayley]];
    {v1, v2} = List @@ targetedge;

    commonvertex = Min[AdjacencyList[graph, v1] ~Intersection~ AdjacencyList[graph, v2]];
    {d1, d2} = Function[{edge},
        If[PropertyValue[{rootgraph, edge}, "EdgeType"] == "Add",
           CayleyLength[EdgeIndex[rootgraph, edge]] // Replace[_Missing :> (Echo@CayleyLength; Abort[])],
           PropertyValue[{rootgraph, edge}, EdgeWeight]
        ]
    ] /@ {UndirectedEdge[commonvertex, v1], UndirectedEdge[commonvertex, v2]};

    Interval[{Abs[d1 - d2], d1 + d2}]
];


(* ::Subsection:: *)
(*CalcCoords*)


calcCoords::negdel = "The determinant `1` is negative at vertex `2` with cayleylength `3`.";
calcCoords::nosol = "The graph is not realizable.";
calcCoords[node_DRNode, vertices_List, CayleyLength_Association] := Module[
    {
        v1, v2
    },

    If[Length[vertices] > 2,

        {v1, v2} = Take[vertices, 2];
        {
            Drop[vertices, 2],
            <|
                v1 -> {0, 0},
                v2 -> {PropertyValue[{node["Root"]["Graph"], v1<->v2}, EdgeWeight], 0}
            |>
        }
        // calcCoordsImpl[node, CayleyLength],
        <||>
    ]

]

calcCoordsImpl[node_DRNode, CayleyLength_Association][{{}, coordsList:Association[(_Integer -> {_?NumericQ, _?NumericQ})...]}] := coordsList
calcCoordsImpl[node_DRNode, CayleyLength_Association][{{curVertex_Integer, restVertices___}, coordsList_Association}] := Module[
    {
        preVertices, getEdgeLength, constrains, preCoords,
        preLengths, dx, dy, d0, mx, my, dd, md, delta, sign, solutions, solution
    },

    If[Length[coordsList] < 2, Return[{{}, <||>}]];

    preVertices = AdjacencyList[node["Graph"], curVertex]
    // Select[(# < curVertex && PropertyValue[{node["Root"]["Graph"], #<->curVertex}, "EdgeType"] != "Drop")&]
    // Sort;
    On[Assert];
    Assert[Length[preVertices] == 2];
    Off[Assert];

    getEdgeLength[edge_UndirectedEdge] := If[PropertyValue[{node["Root"]["Graph"], edge}, "EdgeType"] == "Add",
        CayleyLength[EdgeIndex[node["Root"]["Graph"], edge]],
        PropertyValue[{node["Root"]["Graph"], edge}, EdgeWeight]
    ];

    preCoords = coordsList /@ preVertices;
    If[!MatchQ[preCoords, {{_?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}],
        Echo[curVertex, "curVertex"];
        Echo[preVertices, "preVertices"];
        Echo[coordsList, "preCoords"];
        Print[CayleyLength];
        Return[{{}, <||>}]
    ];

    preLengths = getEdgeLength /@ Thread[UndirectedEdge[preVertices, curVertex]];
    {dx, dy} = Subtract @@ preCoords;
    d0 = getEdgeLength[UndirectedEdge @@ preVertices] (*EuclideanDistance @@ preCoords*);
    {mx, my} = Plus @@ preCoords / 2;
    dd = Subtract @@ preLengths;
    md = Plus @@ preLengths /2;
    delta = Chop[(d0 - dd) * (md - d0 / 2)] * (d0 + dd) * (md + d0 / 2);
    If[ListQ[delta], Print["delta: ", preLengths, CayleyLength]];
    solutions = If[delta < 0,
        Message[calcCoords::negdel, delta, curVertex, CayleyLength];
        (* Echo[CayleyLength, "CayleyLength"]; *)
        (* Echo[t`$rd, "Refined Domain"];
        Echo[d0, "d0"];
        Echo[dd, "dd"];
        Echo[md, "md"];
        Echo[preLengths, "preLengths"]; *)
        (* Abort[]; *)
        (* Return["Unrealizable"]; *)
        {},
        sign = If[PropertyValue[{node["Root"]["Graph"], curVertex}, "Flip"], 1, -1];
        {(- dd * md * {dx, dy} + {-1, 1} * sign * {dy, dx} * Sqrt[delta]) / d0^2 + {mx, my}}
    ];
    (*constrains = And @@ (
        (SquaredEuclideanDistance[coordsList[#], {x, y}] == getEdgeLength[# \[UndirectedEdge] curVertex]^2)&
            /@ preVertices);

    constrains = constrains && (If[PropertyValue[{node["RootGraph"], curVertex}, "Flip"], Less, Greater ]
        @@ {DRPLAN["CCW"[#1, #2, {x, y}]]& @@ preCoords, 0});
    solutions = Solve[constrains, {x, y}, Reals];*)
    If[Length[solutions] == 0,
        Message[calcCoords::nosol]; (*Print[constrains];*)
        {{}, <||>},
        {{restVertices}, Append[coordsList, curVertex -> (First[solutions])]}
    ]

] // Replace[Return[val_] :> val] // calcCoordsImpl[node, CayleyLength]


(* ::Subsection:: *)
(*Solving*)


SolveDRPlan[node_DRNode, dFlip_:All] := getNodeValue /@ SolveNode[node, dFlip];

getNodeValue[nodeSolution_NodeSolution] := Module[
    {
      solution
    },
    solution = Part[nodeSolution, 1];
    (#[{}]&) /@ solution
]

Options[SolveNode] = {
    (* False to use cached *)
    (* True to re-evaluate current node *)
    (* All to re-evaluate current and all sub- nodes *)
    "Reevaluate" -> False
}

SolveNode::invtdf = "Invalid D-flip specified: `1`."
SolveNode[node_DRNode, o:OptionsPattern[]] := SolveNode[node, All, o]
(*
    Caveat: If you specified the D-flip, be careful that the index running among all the D-flips.
    For example, let's say there are solutions in D-flip {1, {1}, {1}} and {1, {2}, {1}}.
    If you now specified D-flip to be {2, All, All}, it will return {1, {2}, {1}} because it is the second D-flip of the two above.
*)
SolveNode[node_DRNode, dFlip:(All | _List), o:OptionsPattern[]] := Module[
    {
        nodeSolutions, curDFlip, subDFlips,
        (* options *)
        reevaluate, subReevaluate
    },

    {reevaluate} = OptionValue[SolveNode, {o}, {"Reevaluate"}];

    (* Print["Solving " <> ToString[node]]; *)
    If[node["IsCayleyNode"],
        UnEcho[#, "nodeSolutions", (ToString/@#)&]& @
        {NodeSolution[
            <|node["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))|>, (* identity function *)
            <|node["TargetCayley"] -> node["Interval"]|>, (* domain *)
            {1} (* D-flip index *)
        ]},

        (* Memoization *)
        If[reevaluate === False && !MissingQ[node["Solutions"]],
            Return[node["Solutions"]]
        ];

        subReevaluate = If[reevaluate === All, All, False]; (* Unless All, do not re-evaluate the sub-nodes *)

        {curDFlip, subDFlips} = Replace[dFlip, {
            (* solve for all D-flips *)
            All :> {All, Table[All, Length[node["SubNodes"]]]},
            (* solve for the specified D-flips for current node, based on specifed sub-D-flips *)
            (* if not enough sub-D-flips are specified, the remainings will based on all sub-D-flips *)
            {cur_, subs:(All|_List)...} :> {{cur}, PadRight[{subs}, Length[node["SubNodes"]], All]},
            (* otherwise, invalid D-flips specifed *)
            _ :> (Message[SolveNode::invdf, targets]; Abort[])
        }];

        nodeSolutions = mergeNodeSolution @@ MapThread[SolveNode[#1, #2, "Reevaluate" -> subReevaluate]&, {node["SubNodes"], subDFlips}];
        (* Memoization *)
        node["Solutions"] = Part[Flatten @ (Curry[SolveDFlip, 2][node] /@ nodeSolutions), curDFlip];
        Echo[#, "nodeSolutions"]& @ node["Solutions"]
    ]
]


mergeNodeSolution[nodeSolutions:PatternSequence[{___NodeSolution}..]] := 
    Outer[mergeNodeSolution, nodeSolutions] // Flatten

mergeNodeSolution[nodeSolutions__NodeSolution] := With[
    {
        nodeSolutionList = List @@@ {nodeSolutions}
    },

    NodeSolution[
        Merge[First][Part[nodeSolutionList, All, 1]],
        Merge[Apply[IntervalIntersection]][Part[nodeSolutionList, All, 2]],
        Prepend[Part[nodeSolutionList, All, 3], Missing["DFlipNotSolved"]]
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
];


modifyGraph[graph_]:=Module[
	  {

    },

    (*VertexDelete[graph,5] // EdgeAdd[#,1\[UndirectedEdge]6]&*)
    graph
];


NodeManipulateRenderingFunction[node_DRNode, nodeSolution_NodeSolution, freeCayleys_Association] := Module[
    {
        solution, graph, cayleyLength, cayleyEdges, droppedEdge, visualCayleyLength
    },

    cayleyLength = (#[freeCayleys]&) /@ First[nodeSolution];
    graph = node["Realize"[cayleyLength]];
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
];

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
];


AnalyzeSolution[node_DRNode, cayleyLength_Association] := Module[
    {
        originGraph, resultGraph
    },

    originGraph = node["Root"]["Graph"];
    resultGraph = node["Root"]["Realize"[cayleyLength]];
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


(* ::Subsection:: *)
(*solve node*)


dropLength[node_DRNode] := With[
	{
		rootgraph = node["Root"]["Graph"]
	},
	
	PropertyValue[{rootgraph, Part[EdgeList[rootgraph], node["TargetDrop"]]}, EdgeWeight]
	
];

dropDiff[node_DRNode, graph_Graph] := Module[
    {
        rootgraph = node["Root"]["Graph"], dropEdge, CayleyLength
    },

    dropEdge = EdgeList[rootgraph][[node["TargetDrop"]]];
    EuclideanDistance[
        PropertyValue[{graph, First[dropEdge]}, VertexCoordinates],
        PropertyValue[{graph, Last[dropEdge]}, VertexCoordinates]
    ] - dropLength[node]
]

dropDiff[node_DRNode, Solution_Association, sample_Association] := Module[
    {
        rootgraph = node["Root"]["Graph"], coordsList, dropEdge, CayleyLength
	  },

    CayleyLength = ((#[sample]&) /@ Solution);
    Check[
        coordsList = calcCoords[node, VertexList[node["Graph"]], CayleyLength];
        (* coordsList = node["CalcCoords"[VertexList[node["Graph"]], CayleyLength]]; *)
        dropEdge = Part[EdgeList[rootgraph], node["TargetDrop"]];
        EuclideanDistance[coordsList[First[dropEdge]], coordsList[Last[dropEdge]]] - dropLength[node],
        $Failed
    ]
]


$SampleDivisor = 2^17 (* the minimal distance between samples should be 1/$SampleDivisor *)
$SampleNum = 36 (* estimated number of samples *)
$BoundaryRatio = 0.05 (* the width of the boundary outline on both sides of the interval that require dense sampling *)
$RefineSampling = True (* on for refinsampling, off for uniform sparse sampling *)
$DenseMultipler = 3 (* the ratio of dense sampling to sparse sampling *)
$UniformSampleDistance = Max[Round[$SampleDivisor / $SampleNum], 1] (* the distance between two uniform samples *)
$DenseSampleDistance = Max[Round[$UniformSampleDistance / $DenseMultipler], 1] (* the distance between two dense samples *)
$SparseSampleDistance = $UniformSampleDistance (* the distance between two sparse samples *)
$LeftBoundaryEnd = Max[Round[$SampleDivisor * $BoundaryRatio], 1]
$RightBoundaryStart = Max[$SampleDivisor - Round[$SampleDivisor * $BoundaryRatio], 1]
$UniformSampleIndices = DeleteDuplicates[Range[0, $SampleDivisor, Max[Round[$SampleDivisor / $SampleNum], 1]] ~Join~ {$SampleDivisor}]
$DenseSampleIndices =  DeleteDuplicates[Join[
    Range[0, $LeftBoundaryEnd, $DenseSampleDistance], {$LeftBoundaryEnd},
    Range[$RightBoundaryStart, $SampleDivisor, $DenseSampleDistance], {$SampleDivisor}
]]
$SparseSampleIndices = DeleteCases[
    Range[$LeftBoundaryEnd, $RightBoundaryStart, $SparseSampleDistance],
    $LeftBoundaryEnd | $RightBoundaryStart
]
$ResampleRatio = 0.15
$ZeroRatio = 0.01


(* return a list of sample indices from 0 to $SampleDivisor *)
getSamples[interval_Interval] := Module[
    {
        left, right,
        sampleIndices
    },

    {left, right} = MinMax[interval];

    sampleIndices = If[$RefineSampling,
        Join[$DenseSampleIndices, $SparseSampleIndices],
        $UniformSampleIndices
    ];

    SparseArray[(sampleIndices + 1) -> left + (right - left) * sampleIndices / $SampleDivisor, $SampleDivisor + 1, Missing["NotSampled"]]
];


SolveDFlip::dupz = "`1` zeros are found.";
SolveDFlip::noz = "no zeros are found.";
SolveDFlip::nosolplan = "no solution for the dr-plan.";
(* This function solves the given dropped flip. *)
SolveDFlip[node_DRNode, nodeSolution_NodeSolution] := Module[
    {
        domain, freeSamples, interpolant,
        incList, decList, time, firstSamples, firstResults,
        resampleTargets, resampleList, nearRatio = 0.30, nearZerosIntervals,
        refinedFreeSamples, refinedResults,
        secondSamples, secondResults, finalSamples, finalResults
	},

    domain = Part[nodeSolution, 2];
    freeSamples = If[Length[node["FreeCayley"]] != 0,
        SparseArray[Values[getSamples /@ KeyTake[domain, First[node["FreeCayley"]]]]],
        Echo["Last Cayley"];
        $on = True;
        {}
    ];
    $sampleLists = {};
    
    (*presamples // scanSamples // genResamples *)
    (* firstSamples = Echo @ If[Length[node["FreeCayley"]] == 0,
        {<||>},
        (* only handles flex-1 case*)
        (<|First[node["FreeCayley"]] -> #|>&) /@ Select[First[freeSamples], Not@*MissingQ]
    ];
    firstResults = scanSamples[node, nodeSolution] /@ firstSamples; *)
    $on = False;
    firstSamples = If[node["FreeCayley"] === {},
        {<||>},
        First[freeSamples]
    ];
    (* If[$on, Echo[firstSamples]]; *)
    (* Echo[node["FreeCayley"]]; *)
    firstResults = If[node["FreeCayley"] === {},
        {Tuple[scanSamples[node, nodeSolution][<||>]]},
        (Replace[freeSample:Except[_Missing] :> (
            Tuple[scanSamples[node, nodeSolution][<|First[node["FreeCayley"]] -> freeSample|>]]
        )] /@ firstSamples)
    ];
    If[$on, Echo[firstResults]];
    (* Echo[firstResults]; *)
    If[node["FreeCayley"] === {},
        finalSamples = firstSamples;
        finalResults = firstResults,
        nearZerosIntervals = findNearZerosIntervals[firstResults, domain[First[node["FreeCayley"]]], nearRatio];
        refinedFreeSamples = getDenseSamples[firstSamples, nearZerosIntervals];
        (* If[nearZerosIntervals =!= {}, Echo[refinedFreeSamples]];
        Abort[]; *)

        $on = True;
        refinedResults = (Replace[freeSample:Except[_Missing] :> (
            Tuple[scanSamples[node, nodeSolution][<|First[node["FreeCayley"]] -> freeSample|>]]
        )] /@ refinedFreeSamples);

        finalSamples = Echo@SparseArray[
            Most[ArrayRules[firstSamples]] ~Join~ Most[ArrayRules[refinedFreeSamples]],
            $SampleDivisor + 1, Missing["NotSampled"]
        ];
        finalResults = Echo@SparseArray[
            Most[ArrayRules[firstResults]] ~Join~ Most[ArrayRules[refinedResults]],
            $SampleDivisor + 1, Missing["NotSampled"]
        ];

    ];
    
    (*Print[sampleLists];*)
    (* interpZeros[firstSamples, #]& /@ Transpose[firstResults] *)
    (* Echo@threadZeros[Identity@@@Select[finalResults, Not@*MissingQ]]; *)
    (* Abort[]; *)
    MapIndexed[
        interpZeros[node, nodeSolution, Select[finalSamples, Not@*MissingQ], #1, First[#2]]&,
        threadZeros[Identity@@@Select[finalResults, Not@*MissingQ]]
    ]

]


scanSamples[node_DRNode, nodeSolution_NodeSolution][freeSample_Association] := Module[
    {
        solution, domain,
        refinedDomain, targetSamples, sampleList, approxIntervals, targetRefinedSamples, refinedSampleList,
        threshold, zeroThreshold, interp, interpd, tmpZeros
    },

    solution = Part[nodeSolution, 1];
    domain = Part[nodeSolution, 2];

    (* refine the domain use triangle inequalities *)
    refinedDomain = t`$rd = IntervalIntersection[
        domain[node["TargetCayley"]],
        refineInterval[node, node["TargetCayley"],
            (* we require the target cayley parameter not appears in its prior vertices' solutions *)
            (#[freeSample]&) /@ KeySelect[solution, (# < node["TargetCayley"]&)]
        ]
    ];
    If[Head[refinedDomain] =!= Interval, Echo[t`rd]];
    If[(Max[#] - Min[#]&)[refinedDomain] <= ($MachineEpsilon * $SampleDivisor), Echo[freeSample,"Empty Refined Interval"]; Return[{}]];

    targetSamples = getSamples[refinedDomain];
    sampleList = (Replace[targetSample:Except[_Missing] :> (
        dropDiff[node, solution, Append[freeSample, <|node["TargetCayley"] -> targetSample|>]]
        // Replace[{
            $Failed :> ((*Echo[targetSample]; AbortNow = True;*) Missing["NoSolution"]),
            diff_?NumericQ :> Pair[targetSample, diff],
            unknown_ :> (Echo[unknown, "Unknown Result"]; Abort[])
        }]
    )] /@ targetSamples);

    (* If[TrueQ[AbortNow], Echo[sampleList]; Abort[]]; *)
    
    threshold = $ResampleRatio * dropLength[node];
    approxIntervals = findApproxIntervals[sampleList, threshold];
    targetRefinedSamples = getDenseSamples[targetSamples, approxIntervals];
    
    refinedSampleList = (Replace[targetSample:Except[_Missing] :> (
        dropDiff[node, solution, Append[freeSample, <|node["TargetCayley"] -> targetSample|>]]
        // Replace[{
            $Failed :> Missing["NoSolution"],
            diff_?NumericQ :> Pair[targetSample, diff],
            unknown_ :> (Echo[unknown, "Unknown Result"]; Abort[])
        }]
    )] /@ targetRefinedSamples);

    sampleList = SparseArray[
        Most[ArrayRules[sampleList]] ~Join~ Most[ArrayRules[refinedSampleList]],
        $SampleDivisor + 1, Missing["NotSampled"]
    ];

    (* If[Length[ArrayRules[refinedSampleList]] > 1, Echo@sampleList];
    Return[] *)

    (* sampleLists = sampleList; *)

    (* To draw 3D points plot, do not delete the following line*)
    If[Length[node["FreeCayley"]] > 0,
        AppendTo[$sampleLists,
            sampleList // ArrayRules // Most
            // Cases[({pos_} -> Pair[x_, y_]) :> {freeSample[First@node["FreeCayley"]], x, y}]
        ]
    ];

    (* If[MatchQ[sampleList, {_Real, _Real}],
        Print[targets]
    ]; *)


    With[
        {
            interpSampleList = List@@@Select[sampleList, Not@*MissingQ] (* DeleteMissing does not work for SparseArray *)
        },

        If[Length[interpSampleList] > 0,
            interp = Interpolation[interpSampleList, InterpolationOrder -> 3, Method -> "Spline"];
            interpd = interp';
            tmpZeros = (findZeros[interp, interpSampleList])
            // Replace[_findZeros :> (
                (* wront type *)
                Print[sampleList];
                Abort[]
            )],
            tmpZeros = {}
        ]
    ]

    If[Length[node["FreeCayley"]] == 0,
        Echo[sampleList, "SampleList"];
        (* Echo[tmpZeros, "tmpZeros"]; *)
        If[Length[tmpZeros] == 0,
            Message[DRNode::nosolplan]
            (*, Message[DRNode::noz]*)
        ]
    ];

    zeroThreshold = $ZeroRatio * dropLength[node];
    zeroIntervals = findApproxIntervals[sampleList, zeroThreshold];
    (* zeroIntervals = {}; *)
    approxZeros = getApproxZeros[tmpZeros, sampleList, zeroIntervals];
    approxZeros = DeleteDuplicates[Join[approxZeros, getBoundaryApproxZeros[sampleList, zeroIntervals]]];
    (* If[Length[approxZeros] > 0,
        Echo[Length[approxZeros], "Num of approximated zeros"];
    ]; *)

    {#, interpd[#]}& /@ Sort[Join[tmpZeros, List@@@Part[sampleList, approxZeros, 1]]]

]


AlternativeInterpolation[list_List] := Module[
    {
        first, last, midOdd, midEven, interpOdd, InterpEven
    },

    first = First[list];
    last = Last[list];
    {midOdd, midEven} = Part[GatherBy[Partition[Riffle[Most[Rest[list]], {"Odd", "Even"}], 2], Last], All, All, 1];

    interpOdd = Interpolation[Join[{first}, midOdd, {last}], InterpolationOrder -> 3, Method -> "Hermite"];
    interpEven = Interpolation[Join[{first}, midEven, {last}], InterpolationOrder -> 3, Method -> "Hermite"];

    InterpolatingFunctionGroup[{interpOdd, interpEven}]

]


(* AveragingInterpolations[ifs:{__InterpolatingFunction}] := Interpretation[
    Row[{"InterpolatingFunctionGroup", "[", Panel[Row[{Nothing, Column[ifs]}]], "]"}],
    InterpolatingFunctionGroup[ifs]
] *)
InterpolatingFunctionGroup[ifs:{__InterpolatingFunction}][x:(_?NumericQ | {__?NumericQ})] := Mean[Through[ifs[x]]]
InterpolatingFunctionGroup[ifs:{__InterpolatingFunction}]["Domain"] := First[ifs]["Domain"]


(* Format[ifg:InterpolatingFunctionGroup[ifs:{__InterpolatingFunction}]] := Interpretation[
    InterpolatingFunctionGroup[Panel[Row[{Nothing, Column[ifs]}]]],
    ifg
] *)


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


findNearZerosIntervals[zeroTuples_SparseArray, domain_Interval, threshold_?NumericQ] := Module[
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
                Join[{Min[domain]}, Part[zeroTuple, All, 1], {Max[domain]}]
                // Differences
                // Select[LessEqualThan[threshold * RegionMeasure[domain]]]
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
getDenseSamples[targetSamples_SparseArray, intervals:{{_Integer, _Integer}...}] := Module[
    {
        sampleIndices, left, right
    },

    left = First[targetSamples] // Replace[_Missing :> (Message[getDenseSamples::misint]; Abort[])];
    right = Last[targetSamples] // Replace[_Missing :> (Message[getDenseSamples::misint]; Abort[])];

    sampleIndices = Complement[
        Join[getDenseSamplesImpl /@ intervals],
        targetSamples // ArrayRules // Most
        // Cases[({pos_} -> val_) :> pos - 1] (* the index is off-by-one*)
    ];
    SparseArray[
        Thread[(sampleIndices + 1) -> left + (right - left) * sampleIndices / $SampleDivisor],
        $SampleDivisor + 1, Missing["NotSampled"]
    ]
]

getDenseSamplesImpl[{start_Integer, end_Integer}] := (
    Range[
        Max[(start - 1) - $SparseSampleDistance, $LeftBoundaryEnd],
        Min[(end - 1) + $SparseSampleDistance, $RightBoundaryStart],
        $DenseSampleDistance
    ]
)


(* Find zeros in a interpolating function *)
findZeros[interp_InterpolatingFunction, samplelist:{{_?NumericQ, _?NumericQ}..}] := Module[
    {
        domain, bsp, knots, controlpoints, polyform, t, zeros, zerodomain
    },

    {domain} = interp["Domain"];
    bsp = First@Cases[interp, _BSplineFunction, Infinity];
    {knots} = bsp["Knots"];
    controlpoints = bsp["ControlPoints"];
    (*zerodomain= Transpose[{Most[knots[[3;;-3]]],Rest[knots[[3;;-3]]]}][[Flatten[Position[Most[controlpoints]*Rest[controlpoints],_?NonPositive]]]];*)
    zerodomain = Part[
        Transpose[{Most[samplelist[[All,1]]], Rest[samplelist[[All,1]]]}],
        Flatten[Position[Most[samplelist[[All,2]]] * Rest[samplelist[[All,2]]], _?NonPositive]]
    ];

    polyform = PiecewiseExpand[Sum[
        controlpoints[[i + 1]] * PiecewiseExpand[BSplineBasis[{3, knots}, i, t]],
        {i, 0, Length[controlpoints] - 1}
    ]];
    polyform = PiecewiseExpand[Piecewise[{{polyform,(Or@@((#[[1]] <= t <= #[[2]])&/@zerodomain))}},1]];
    zeros = Flatten[Solve[polyform == 0, Reals]];
    (t/.#&) /@ zeros
];



getApproxZeros[trueZeros_List, sampleList_SparseArray, zeroMinima_List] := Module[
    {umZeros, umMinima}, 

    {umZeros, umMinima} = getApproxZerosImpl[sampleList][{{trueZeros, zeroIntervals}, {{},{}}}];
    (* If[Length[umMinima] > 0,
        Echo[(Part[sampleList, -1, 1] - Part[sampleList, 1, 1]) / $SampleDivisor * $DenseSampleDistance, "tolerance"];
        Echo[{umZeros, Normal[Part[sampleList, umMinima, 1]]}]
    ]; *)
    umMinima

]

getApproxZerosImpl[sampleList_SparseArray][{{{}, {}}, unmatches:{{___?NumericQ}, {___Integer}}}] := unmatches
getApproxZerosImpl[sampleList_SparseArray][{{{}, intervals:{__}}, unmatches_List}] := (* continue as if there is a zero at infinity point *)
    getApproxZerosImpl[sampleList][{{{Infinity}, intervals}, unmatches}]
getApproxZerosImpl[sampleList_SparseArray][{{trueZeros:{__}, {}}, unmatches_List}] := (* continue as if there is an interval at infinity point *)
    getApproxZerosImpl[sampleList][{{trueZeros, {Infinity}}, unmatches}]
getApproxZerosImpl[sampleList_SparseArray][{{trueZeros:{__}, intervals:{__}}, {umZeros_List, umMinima_List}}] := Module[
    {
        tolerance, firstZero, firstInterval, firstMinima
    },

    tolerance = (Part[sampleList, -1, 1] - Part[sampleList, 1, 1]) / $SampleDivisor * $DenseSampleDistance;

    firstZero = First[trueZeros];
    firstInterval = Replace[intervals, {
        {i:{_Integer, _Integer}, ___} :> Interval[Normal[Part[sampleList, First[intervals], 1]]],
        {Infinity} :> Interval[{Infinity, Infinity}]
    }];
    Which[
        TrueQ[firstZero < firstInterval],
        {
            {Rest[trueZeros], intervals},
            If[Length[umMinima] > 0 && firstZero < Part[sampleList, Last[umMinima], 1] + tolerance,
                (* current Zero is >~ the minimum of the last interval *)
                {umZeros, Most[umMinima]},
                {Append[umZeros, firstZero], umMinima}
            ]
        },
        TrueQ[firstZero > firstInterval],
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
] // getApproxZerosImpl[sampleList]


getBoundaryApproxZeros::nep = "Cannot find the neighbor point of the boundary point."
getBoundaryApproxZeros[_, {}] := {}
getBoundaryApproxZeros[sampleList_SparseArray, zeroIntervals:{__}] := Module[
    {

    },

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
]


interpZeros[node_DRNode, nodeSolution_NodeSolution, samples_, sampleList:{(_?NumericQ|_Missing)..}, index_Integer] := Module[
    {
        solution, dflip, interpList, zeroFunc, targetRule,
        newSolution, newDomain, newDFlip
    },

    solution = Part[nodeSolution, 1];
    dflip =  Part[nodeSolution, 3];
    zeroFunc = If[Length[node["FreeCayley"]] > 0,
    (* Ci vs C1 *)
        interpList = DeleteMissing[Transpose[{samples, sampleList}], 1, 1];
            If[Length[interpList] <= 4,
            (* not enough samples *)
                Return[{}]
            ];
        AlternativeInterpolation[interpList],
        (* Interpolation[interpList, InterpolationOrder -> 3, Method -> "Hermite"], *)
        (* Interpolation[interpList, InterpolationOrder -> 3, Method -> "Spline"], *)
    (* the last Cayley C1 *)
        Function[{const}, (const &)] @@ sampleList
    ];
    (*Print[zeroFunc[1]];*)

    targetRule = (Construct[Lookup, node["TargetCayley"]] -> (
        zeroFunc
        @* (Sequence@@#&)
        @* (Curry[Through[#1[#2]]&, 2][Lookup /@ node["FreeCayley"]]))
    );

    newSolution = (solution /. targetRule);
    newDomain = AssociationThread[node["FreeCayley"], Interval /@ zeroFunc["Domain"]];
    newDFlip = ReplacePart[dflip, 1 -> index];
    NodeSolution[newSolution, newDomain, newDFlip]
]


(* ::Section:: *)
(*Thread*)


NewThread[beads_] := <|"beads" -> beads, "terminated" -> {}, "active" -> {}, "position" -> 0|>;

CreateLine[state_Association, index_Integer] := ReplacePart[state, "active" -> Append[state["active"], CreateLine[index, state["position"]]]];
CreateLine[index_Integer, pos_Integer] := <|"index" -> index, "beads" -> {}, "start" -> pos|>;

TerminateLine[state_Association, index_Integer] := Module[
	{
		activeLines, terminatingIdx, terminatedLines
	},
	
	activeLines = state["active"];
	terminatingIdx = FirstPosition[activeLines, _?(#index == index&), {}, {1}, Heads -> False];
	terminatedLines = Join[state["terminated"], Part[activeLines, terminatingIdx]];
	activeLines = Delete[activeLines, terminatingIdx];
	ReplacePart[state, {"active" -> activeLines, "terminated" -> terminatedLines}]
]; 

AllLines[state_Association] := Module[
	{
		activeLines = state["active"], terminatedLines = state["terminated"],
		line, numBeads
	},
	
	numBeads = Length[state["beads"]];
	(Table[Missing[], #start] ~Join~ #beads ~Join~ Table[Missing[], numBeads - #start - Length[#beads]]&) /@ (activeLines ~Join~ terminatedLines)
	
];


ThreadBeads[state_Association] := Module[
	{
		position = state["position"], beads = state["beads"], curBeads,
		activeLines
	},
	
	position += 1;
	curBeads = Part[beads, position];
	activeLines = state["active"];
	activeLines = MapThread[ReplacePart[#1, "beads" -> Append[#1["beads"], #2]]&, {
		activeLines,
		Part[curBeads, Through[activeLines["index"]], 1]
	}];
	ReplacePart[state, {"position" -> position, "active" -> activeLines}]
];


ChangeLineIdx[state_Association, offsets_List] := Module[
	{
		activeLines
	},
	
	activeLines = state["active"];
	activeLines = MapThread[ReplacePart[#1, "index" -> (#1["index"] + #2)]&, {
		activeLines,
		Part[offsets, Through[activeLines["index"]]]
	}];
	
	ReplacePart[state, "active" -> activeLines]
];


threadZeros[zeroTuples_List] := Module[
    {
        steps
    },
	If[Length[zeroTuples] == 1,
		Return[Transpose[{Part[zeroTuples, 1, All, 1]}]]
	];
    steps = Echo@MapThread[threadStep, {Most@zeroTuples, Rest@zeroTuples}];
    AllLines[Fold[threadLine, NewThread[zeroTuples], steps]]
];


(* TODO: merge threadLine and threadStep *)


threadLine[state_Association, step_] := Module[
    {
        numLines,
        activeIds, terminatedIds,
        newState = state
    },

    numLines = Length[step];
    activeIds = Through[state["active"]["index"]] ;

    (* terminates lines that out of boundary *)
    terminatedIds = Select[activeIds, (# <  1 || # > numLines)&]; 
    newState = Fold[TerminateLine, newState, terminatedIds];

    (* add new lines *)
    activeIds = Complement[activeIds, terminatedIds];
    newState = Fold[CreateLine, newState, Complement[Range[numLines], activeIds]];
    
    newState = ThreadBeads[newState];
    newState = ChangeLineIdx[newState, step]

];


threadStep[firstPair_, secondPair_] := Module[
    {
    },

    If[firstPair === {} || secondPair === {},
       Return[{}]
    ];

    (* Print["Comparing", firstPair, secondPair]; *)

    If[Length @ firstPair == Length @ secondPair,
       (* same length*)
       If[(Positive /@ Part[firstPair, All, 2]) === (Positive /@ Part[secondPair, All, 2]),
          (* derivaive match *)
          Return[Table[0, Length @ firstPair]],
          (* one-off *)
          If[Abs[Part[firstPair, 1, 1] - Part[secondPair, -1, 1]] > Abs[Part[firstPair, -1, 1] - Part[secondPair, 1, 1]],
             Return[Table[-1, Length @ firstPair]],
             Return[Table[1, Length @ firstPair]]
          ]
       ],

       (* not same length*)
       If[Abs[Length @ firstPair - Length @ secondPair] == 1,
          (* 3-2, 2-3, 2-1 or 1-2 *)
          If[Positive @ Part[firstPair, 1, 2] === Positive @ Part[secondPair, 1, 2],
              Return[Table[0, Length @ firstPair]],
              If[Length @ firstPair > Length @ secondPair,
                Return[Table[-1, Length @ firstPair]],
                Return[Table[1, Length @ firstPair]]
              ]
          ],
          (* 3-1 or 1-3*)
          If[Length @ firstPair > Length @ secondPair,
             (* 3-1 *)
             Return[Table[1 - First @ FirstPosition[Part[firstPair, All, 1], First @ Nearest[Part[firstPair, All, 1], Part[secondPair, 1, 1]]], 3]],
             (* 1-3 *)
             Return[FirstPosition[Part[secondPair, All, 1], First @ Nearest[Part[secondPair, All, 1], Part[firstPair, 1, 1]]] - 1]
          ]

       ]

    ]

];


End[];


EndPackage[];
