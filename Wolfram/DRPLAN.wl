(* ::Package:: *)

(* ::Title:: *)
(*DRPLAN*)


BeginPackage["DRPLAN`"];
ClearAll[Evaluate[Context[] <> "*"]];


(* ::Item:: *)
(*Output Symbols*)


DRNode::usage = "DRNode[$n][\"property\"] returns the specified property of given DRNode[$n]."
NewDRNode::usage = "NewDRNode[dotfile_String] returns the root node of a DR-Plan tree, according to the specified dotfile path."
GenerateDRPlan::usage = "GenerateDRPlan[node_DRNode] constructs the DR-Plan for given root node."
FlipAt::usage = "FlipAt[node_DRNode, vertices_List] flips given vertices in the list for given root node."


Begin["`Private`"];
ClearAll[Evaluate[Context[] <> "*"]];
Needs["GraphvizUtils`"];


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
    newNode["Parents"] = {};
    newNode["Graph"] = graph;
    newNode["Root"] = newNode;
    newNode["Solutions"] = Missing["NotSolved"]; (* For memoization, should be constant once assign a Non-Missing value. *)
    newNode["OffsetSolutions"] = {}; (* Value gets updated from time to time. *)
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
        <|edgeToCol[EdgeIndex[graph, e0]] -> Interval[{
            Abs[PropertyValue[{graph, e1}, EdgeWeight] - PropertyValue[{graph, e2}, EdgeWeight]],
            PropertyValue[{graph, e1}, EdgeWeight] + PropertyValue[{graph, e2}, EdgeWeight]
        }]|>
    }
];






End[];


EndPackage[];
