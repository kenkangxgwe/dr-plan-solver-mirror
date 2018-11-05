(* ::Package:: *)

(* ::Title:: *)
(*DRPLAN*)


BeginPackage["DRPLAN`", "Graphviz`"];

ClearAll["DRPLAN`*"];
ClearAll["DRPLAN`Private`*"];


Displacement::usage = "Displacement[node_DRNode, node_DRNode] combines two realizations for the same linkage and draw displancement vectors.";
RigidityMatrix::usage = "RigidityMatrix[graph_Graph] returns the rigidity matrix.";
InfinitesimallyRigidQ::usage = "InfinitismallyRigidQ[graph_Graph] gives True if the input DRNode is infinitesimally rigid .";
NewDRNode::usage = "NewDRNode[dotfile_String] returns the root node of a DR-Plan tree, according to the specified dotfile path.
	DRNode[objID][\"GenerateDRPlan\"[]] generates DR-plan for given node.";
PrintDRPlan::usage = "PrintDRPlan[node_DRNode] prints the freeCayley parameters at each node.";
SolveDRPlan::usage = "SolveDRPlan[node_DRNode] solves a DRPlan by passing in the root DR-node."


Begin["`Private`"];


(* ::Subsection:: *)
(*Utilities*)


Options[Displacement] = {"AddEdge" -> True};
Displacement::neq = "Two graphs have different number of vertices.";
Displacement[node1_DRNode, node2_DRNode, OptionsPattern[]] := Module[{graph1 = node1["Graph"], graph2 = node2["Graph"],
    combinedGraph, coordMap, colorMap, combinedVertexList, combinedEdgeList},
    If[Complement @@ (VertexList /@ {graph1, graph2}) =!= {},
        Message[Displacement::neq];
        Throw[$Failed];
    ];
    coordMap = ((PropertyValue[{graph1, #}, VertexCoordinates]& /@ VertexList[graph1])
        ~ Join ~ (PropertyValue[{graph2, #}, VertexCoordinates]& /@ VertexList[graph2]));
    If[OptionValue["AddEdge"],
        colorMap = (PropertyValue[{graph1, #}, EdgeStyle]& /@ EdgeList[graph1])
            ~ Join ~ (PropertyValue[{graph2, #}, EdgeStyle]& /@ EdgeList[graph2]);,
        graph1 = EdgeDelete[graph1, Select[EdgeList[graph1], (PropertyValue[{graph1, #}, "EdgeType"] == "Add")&]];
        graph2 = EdgeDelete[graph2, Select[EdgeList[graph2], (PropertyValue[{graph2, #}, "EdgeType"] == "Add")&]];
        colorMap = Table[Black, EdgeCount[graph1]] ~ Join ~ Table[Blue, EdgeCount[graph2]];
    ];
    graph1 = VertexReplace[graph1, Table[i -> Subscript["a", i], {i, 0, VertexCount[graph1]}]];
    graph2 = VertexReplace[graph2, Table[i -> Subscript["b", i], {i, 0, VertexCount[graph2]}]];
    combinedVertexList = VertexList[graph1] ~ Join ~ VertexList[graph2];
    combinedEdgeList = EdgeList[graph1] ~ Join ~ EdgeList[graph2]
        ~ Join ~ Thread[VertexList[graph1]\[UndirectedEdge]VertexList[graph2]];
    colorMap = Thread[combinedEdgeList -> PadRight[colorMap, Length[combinedEdgeList], {{RGBColor[0.6, 0.6, 0.6], Dashed}}]];
    combinedGraph = Graph[combinedVertexList, combinedEdgeList, VertexCoordinates -> coordMap, EdgeStyle -> colorMap, VertexLabels -> "Name"]
];


RigidityMatrix[graph_Graph] := Module[{edgelist, coordMap},
    edgelist = EdgeList[graph];
    SparseArray[Join @@ (MapIndexed[RigidityElement[graph], edgelist])]
];

RigidityElement[graph_][UndirectedEdge[v1_Integer, v2_Integer], {row_Integer}] := Module[{displacement},
    displacement = Subtract @@ PropertyValue[{graph, #}, VertexCoordinates]& /@ {v1, v2};
    {{row, 2 * v1 + 1} -> First@displacement, {row, 2 * v1 + 2} -> Last@displacement, {row, 2 * v2 + 1} -> First@(-displacement), {row, 2 * v2 + 2} -> Last@(-displacement)}
];

InfinitesimallyRigidQ[graph_Graph] := Module[{rigidityMatrix},
    rigidityMatrix = RigidityMatrix[graph];
    (Last@Dimensions[rigidityMatrix] - MatrixRank[rigidityMatrix]) == 3
];


(* ::Section:: *)
(*DRPLAN*)


(* ::Subsection:: *)
(*Constants*)


(* Some constants are declared here. *)
DRPLAN["EPSILON"] = 2 * 10^(-5);
DRPLAN["DRNodeVSFunc"][{xc_, yc_}, name_, {w_, h_}] := name["Graph"];
DRPLAN["CCW"[p_List, q_List, r__List]] := Det[Append[#, 1] & /@ {p, q, r}];
DRPLAN["SampleNum"] = 40;
UnEcho = (#1&);


(* ::Subsection:: *)
(*Constructor*)


(* Constructs a new DRNode *)
Options[NewDRNode] = {
    "ImportRealization" -> True
};
NewDRNode[dotfile_String, o:OptionsPattern[]] := NewDRNode[Graphviz`ImportGraphviz[dotfile, o]];
    
NewDRNode[graph:(_Graph | _Subgraph)] := Module[{objID = Unique[]},
    DRNode[objID]["FreeCayley"] = {};
    DRNode[objID]["SubNodes"] = {};
    DRNode[objID]["Graph"] = graph;
    DRNode[objID]["Root"] = DRNode[objID];
    DRNode[objID]
];


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

DRNode[objID_]["FlipAt"[vertices_List]] := Module[{obj = DRNode[objID], graph},
    graph = obj["Graph"];
    (PropertyValue[{graph, #}, "Flip"] = True)& /@ vertices;
    obj["Graph"] = graph;
    obj
];

DRNode[objID_]["GenerateDRPlan"[]] := Module[
	{
		obj = DRNode[objID],
		soleEdge, freeCayley, subNodes
	},

	If[obj["Root"] === obj,
		calcInterval[obj];
	];
	
    If[obj["IsCayleyNode"] = EdgeCount[obj["Graph"]] == 1,
		soleEdge = First[EdgeList[obj["Graph"]]];
        obj["AllCayley"] = obj["FreeCayley"] = {obj["TargetCayley"]} = {EdgeIndex[obj["Root"]["Graph"], soleEdge]};
        obj["Interval"] = Interval[{Min[#] + DRPLAN["EPSILON"], Max[#] - DRPLAN["EPSILON"]}]& @ PropertyValue[{obj["Root"]["Graph"], soleEdge}, "Interval"];
        
        obj["EdgeRules"] = {};
    ,
        {freeCayley, subNodes} = GenerateDRNode[obj];
        obj["SubNodes"] = obj["SubNodes"] ~Join~ subNodes;
        Through[obj["SubNodes"]["GenerateDRPlan"[]]];
        obj["FreeCayley"] = Union[Flatten[Through[obj["SubNodes"]["FreeCayley"]]]];
        If[Length[freeCayley] == 0,
            obj["TargetCayley"] = Last[obj["FreeCayley"]];
            obj["FreeCayley"] = Most[obj["FreeCayley"]]
        ,
            obj["FreeCayley"] = Union[Join[obj["FreeCayley"], Most[freeCayley]]];
            obj["TargetCayley"] = Last[freeCayley]
        ];
        subNodes = AddSubNode[obj, {EdgeList[obj["Root"]["Graph"]][[#]]}]& /@ freeCayley;
        Through[subNodes["GenerateDRPlan"[]]];
        obj["SubNodes"] = obj["SubNodes"] ~Join~ subNodes;
        obj["AllCayley"] = Union[freeCayley, Union @@ Through[obj["SubNodes"]["AllCayley"]]];
        obj
    ];
    obj["EdgeRules"] = ((obj -> #&) /@ obj["SubNodes"]) ~Join~ (Join @@ Through[obj["SubNodes"]["EdgeRules"]]);
];

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
	Switch[PropertyValue[{rootgraph, curEdge}, "EdgeType"],
        "Drop",
            Switch[dropCounter,
                0, 
                    node["TargetDrop"] = rootEdgeIndex;
                    node["TargetLength"] = PropertyValue[{rootgraph, curEdge}, EdgeWeight];,
                1, 
                    AppendTo[newSubNodes, AddSubNode[node, Range[0, Max[{First[curEdge], Last[curEdge]}]]]];
            ];
            GenerateDRNode[{node, edgeIndex - 1, dropCounter + 1}, {freeCayley, newSubNodes}],
        "Add",
            GenerateDRNode[{node, edgeIndex - 1, dropCounter}, {Append[freeCayley, rootEdgeIndex], subNodes}],
        "Partial",
            GenerateDRNode[{node, edgeIndex - 1, dropCounter}, {freeCayley, subNodes}]
    ]
];


(* ::Subsubsection:: *)
(*Linear Programming*)


(* Calculate the interval from every added edge using linear programming. *)
calcInterval[node_DRNode] := Module[
	{
		graph = node["Graph"], addEdgeIndices, addNum, edgeToCol,
		e, m, b, lu, commonVertex, cons, mins, maxs
	},

	addEdgeIndices = Table[
		If[PropertyValue[{graph, e}, "EdgeType"]== "Add",
			EdgeIndex[graph, e],
			Nothing
		],
		{e, EdgeList[graph]}
	];
	addNum = Length[addEdgeIndices];
	edgeToCol = Association[Thread[(addEdgeIndices -> Range[addNum])]];
	
	{m, b, lu} = ( Flatten /@ (Transpose @ Table[
		e = EdgeList[graph][[ei]];
		commonVertex = Intersection[AdjacencyList[graph, First[e]], AdjacencyList[graph, Last[e]]];
		cons = getConstraints[graph, {e, UndirectedEdge[First[e], #], UndirectedEdge[Last[e], #]}, edgeToCol, addNum]& /@ commonVertex;
		Flatten /@ Transpose[cons],
		{ei, Keys[edgeToCol]}
	]));
	b = List @@@ b;
	lu = lu
	// Merge[(IntervalIntersection @@ #&)]
	/* KeyValueMap[({{#1, 1} -> Min[#2], {#1, 2} -> Max[#2]}&)]
	/* Flatten
	/* (SparseArray[#, {addNum, 2}]&);
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


(* ::Subsection:: *)
(*Print*)


DRNodeVRFunc[rk_, vk_] := 
	Inset[Button[Column[{
		HighlightNode[vk],
		If[vk["IsCayleyNode"],
			ToString[vk["Interval"]],
			Nothing
		]}],
	vk["ListSolution"[]], Appearance->None], rk];

DRNode[objID_]["PlotDRPlan"[]] := TreePlot[DRNode[objID]["EdgeRules"],
    Top, DRNode[objID], VertexRenderingFunction -> DRNodeVRFunc, EdgeRenderingFunction -> ({Dashed, Opacity[.5], Line[#1]}&)];

PrintDRPlan[node_DRNode]:= Module[
	{
		graph = node["Graph"]
	},
	
	Print[node["FreeCayley"]];
    Print[node["SubNodes"]];
    PrintDRPlan /@ node["SubNodes"];
];

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
		GraphHighlightStyle -> "DehighlightFade"
	]
];

Print[nodeSolution_NodeSolution] ^:= Print[ToString[nodeSolution]];

ToString[nodeSolution_NodeSolution] ^:= "NodeSolution:\n" <> 
"\tSolution:\n" <>
StringJoin[("\t\t" <> # <> "\n"&) @* ToString /@ (Normal @ First @ nodeSolution)] <>
"\tDomain:\n" <>
StringJoin[("\t\t" <> # <> "\n"&) @* ToString /@ (Normal @ Last @ nodeSolution)];


(* ::Subsection:: *)
(*Realization*)


DRNode[objID_]["Realize"[]] := Module[{obj = DRNode[objID], cayleyLength, coordsList, tmpSubGraph},
    cayleyLength = <| # -> PropertyValue[{obj["RootGraph"], #}, EdgeWeight]& /@ obj["AllCayley"] |>;
    obj["Realize"[cayleyLength]]
];
DRNode[objID_]["Realize"[cayleyLength_Association]] := Module[{obj = DRNode[objID], coordsList, tmpSubGraph},
    coordsList = obj["CalcCoords"[VertexList[obj["Graph"]], cayleyLength]];
    Subgraph[obj["Graph"], Keys[coordsList], VertexCoordinates -> Normal@coordsList, Options[obj["Graph"]]]
];

DRNode::nosol = "The graph is not realizable.";
DRNode[objID_]["CalcCoords"[{v1_, v2_}, CayleyLength_Association]] := <|v1 -> {0, 0}, v2 -> {PropertyValue[{DRNode[objID]["Root"]["Graph"], v1<->v2}, EdgeWeight], 0}|>;
DRNode[objID_]["CalcCoords"[vertices_List, CayleyLength_Association]] := Module[{obj = DRNode[objID], curVertex = Last[vertices],
    coordsList, preVertices, getEdgeLength, constrains, preCoords, preLengths, dx, dy, d0, mx, my, dd, md, delta, sign, solutions, solution},
(*Print[CayleyLength];*)
    coordsList = obj["CalcCoords"[Most[vertices], CayleyLength]];
    (*Print[coordsList];*)
    If[Length[coordsList] == 0,
        {},
        preVertices = Sort[Select[AdjacencyList[obj["Graph"], curVertex], (# < curVertex && PropertyValue[{obj["Root"]["Graph"], #<->curVertex}, "EdgeType"] != "Drop")&]];
        On[Assert];
        Assert[Length[preVertices] == 2];
        Off[Assert];
        
        getEdgeLength[edge_UndirectedEdge] := If[PropertyValue[{obj["Root"]["Graph"], edge}, "EdgeType"] == "Add",
            CayleyLength[EdgeIndex[obj["Root"]["Graph"], edge]],
            PropertyValue[{obj["Root"]["Graph"], edge}, EdgeWeight]
        ];
        preCoords = coordsList /@ preVertices;
        If[!MatchQ[preCoords, {{_?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}],
	        Print["preCoords", coordsList];
	        Print[CayleyLength];
	        Return["Missing Keys"];
        ];
        preLengths = getEdgeLength[# \[UndirectedEdge] curVertex]& /@ preVertices;
        {dx, dy} = Subtract @@ preCoords;
        d0 = EuclideanDistance @@ preCoords;
        {mx, my} = Plus @@ preCoords / 2;
        dd = Subtract @@ preLengths;
        md = Plus @@ preLengths /2;
        delta = Chop[(d0^2 - dd^2) * (md^2 - d0^2 / 4)];
        If[ListQ[delta], Print["delta: ", preLengths, CayleyLength]];
        solutions = If[delta < 0,
            UnEcho[delta, "delta"];
            Return["Unrealizable"];
            {},
            sign = If[PropertyValue[{obj["Root"]["Graph"], curVertex}, "Flip"], 1, -1];
            {(- dd * md * {dx, dy} + {-1, 1} * sign * {dy, dx} * Sqrt[delta]) / d0^2 + {mx, my}}
        ];
        (*constrains = And @@ (
            (SquaredEuclideanDistance[coordsList[#], {x, y}] == getEdgeLength[# \[UndirectedEdge] curVertex]^2)&
                /@ preVertices);
        
        constrains = constrains && (If[PropertyValue[{obj["RootGraph"], curVertex}, "Flip"], Less, Greater ]
            @@ {DRPLAN["CCW"[#1, #2, {x, y}]]& @@ preCoords, 0});
        solutions = Solve[constrains, {x, y}, Reals];*)
        If[Length[solutions] == 0,
            Message[DRNode::nosol]; (*Print[constrains];*) {},
            coordsList ~ Join ~ <|curVertex -> (First[solutions])|>
        ]
    ]
];



(* ::Subsection:: *)
(*Solving*)


SolveDRPlan[node_DRNode] := getNodeValue /@ node["SolveNode"[]];

getNodeValue[nodeSolution_NodeSolution] := Module[
	{
		solution, domain
	},
	{solution, domain} = List @@ nodeSolution;
	(#[{}]&) /@ solution
]

DRNode[objID_]["SolveNode"[]] := Module[
	{
		obj = DRNode[objID], nodeSolutions
	},
	
    If[obj["IsCayleyNode"],
        UnEcho[#, "nodeSolutions", (ToString/@#&)]& @ {NodeSolution[<|obj["TargetCayley"] -> (First @* (Curry[Through[#1[#2]]&, 2][Lookup /@ obj["FreeCayley"]]))|>, <|obj["TargetCayley"] -> obj["Interval"]|>]}
    ,
        nodeSolutions = mergeSolution[Through[obj["SubNodes"]["SolveNode"[]]]];
        UnEcho[#, "nodeSolutions", (ToString/@#&)]& @ Flatten @ (obj["Expand"[#]]& /@ nodeSolutions)
    ]
];

mergeSolution[nodeSolutionLists:{{_NodeSolution...}...}] := Module[
	{
		nodeSolutions
    },
    
	Outer[
		(NodeSolution @@ (Merge[First] /@ (UnEcho@Transpose @ (List @@@ {##}))))&,
		Sequence @@ nodeSolutionLists
	] // Flatten // UnEcho
];

DRNode[objID_]["ListSolution"[]] := Module[{obj = DRNode[objID], nodeSolutions, testFunc},
    If[!obj["IsCayleyNode"],
        nodeSolutions = mergeSolution[Through[obj["SubNodes"]["SolveNode"[]]]];
        Print[Panel[Column[{ToString[#], Construct[AnalyzeNode, obj, #]}, Center]]& /@ nodeSolutions];
    ];
];

AnalyzeNode[node_DRNode, nodSolution_NodeSolution] := Module[
	{
		cayleys, vars, labels, mins, maxs
	},
	
	cayleys = Append[node["FreeCayley"], node["TargetCayley"]];
	labels = ("c" <> ToString[#]&) /@ cayleys;
	vars = Unique /@ labels;
	{mins, maxs} = Transpose @ (MinMax /@ (Lookup[cayleys]@ Last @ nodSolution));
	Manipulate[node["Realize"[#1]], ##2]&[
		((#[Association[Thread[cayleys -> vars]]]&) /@ (First @ nodSolution)), 
		Sequence @@ MapThread[{{#1, #3, #2}, #3, #4}&, {vars, labels, mins, maxs}]
	]
];

dropDiff[node_DRNode, Solution_Association, sample_Association] := Module[
	{
		rootgraph = node["Root"]["Graph"], coordsList, dropEdge, CayleyLength
	},
	CayleyLength = ((#[sample]&) /@ Solution);
	coordsList = node["CalcCoords"[VertexList[node["Graph"]], CayleyLength]];
	dropEdge = EdgeList[rootgraph][[node["TargetDrop"]]];
    EuclideanDistance[coordsList[First[dropEdge]], coordsList[Last[dropEdge]]] -
        PropertyValue[{rootgraph, dropEdge}, EdgeWeight]
];

DRNode::dupz = "`1` zeros are found.";
DRNode::noz = "no zeros are found.";
DRNode::nosolplan = "no solution for the dr-plan.";
DRNode[objID_]["Expand"[NodeSolution[Solution_Association, Domain_Association]]] := Module[
	{
		obj = DRNode[objID], subDivide, samples, interpolant, findZero,
		interpZeros, incList, decList, time,
		sampleLists
	},
	
    subDivide[key_, interval_Interval] := (<|key -> #|>& /@ Subdivide[Min[interval], Max[interval], DRPLAN["SampleNum"]]);
    samples = If[Length[obj["FreeCayley"]] != 0,
        Flatten @ Outer[Join, KeyValueMap[subDivide, KeyTake[Domain, obj["FreeCayley"]]]],
        {<|Nothing|>}
    ];
    sampleLists = {};
    findZero[Sample_Association] := Module[
		{
			targets, sampleList, tmpSample, tmpZeros, findDom, res, cntZeros
		},
        

        targets = subDivide[obj["TargetCayley"], Domain[obj["TargetCayley"]]];
        sampleList = ({
            #[obj["TargetCayley"]], 
            dropDiff[obj, Solution, Append[Sample, #]]
        }& /@ targets);

        (*AppendTo[sampleLists, Prepend[Sample[First@obj["FreeCayley"]]] /@ sampleList];*)

        tmpZeros = findZeros[sampleList];
          
        If[Length[tmpZeros] == 0, 
            If[Length[obj["FreeCayley"]] == 0,
                UnEcho[sampleList, "SampleList"];
                Message[DRNode::nosolplan]
                (*, Message[DRNode::noz]*)
            ];
        ];
        
        If[Length[tmpZeros] > 2, Message[DRNode::dupz, Length[tmpZeros]]];
        
        If[Length[obj["FreeCayley"]] == 0,
            tmpZeros[[All,1]],
            {
	            FirstCase[tmpZeros, {r_, _?NonNegative} :> r],
	            FirstCase[tmpZeros, {r_, _?Negative} :> r]
            }
        ]
    ];
    
    interpZeros[sampleList:{(_Real|_Missing)..}] := Module[
        {
            interpList, zeroFunc, targetRule, newSolution, newDomain
        },
        
        zeroFunc = If[Length[obj["FreeCayley"]] > 0,
        	interpList = DeleteMissing[Transpose[{Values[samples], sampleList}], 1, 1];
            If[Length[interpList] <= 4, Return[{}]];
	        Interpolation[interpList],
	        Function[{const}, (const &)] @@ sampleList
	    ];
        (*Print[zeroFunc[1]];*)
        targetRule = (Construct[Lookup, obj["TargetCayley"]] -> (zeroFunc @* (Sequence@@#&) @* (Curry[Through[#1[#2]]&, 2][Lookup /@ obj["FreeCayley"]])));
    
        newSolution = (Solution /. targetRule);
        newDomain = AssociationThread[obj["FreeCayley"], Interval/@zeroFunc["Domain"]];
        NodeSolution[newSolution, newDomain]
    ];
    
    interpZeros /@ Transpose[findZero /@ samples]
];

findZeros[samplelist:{{_Real, _Real}..}] := Module[
    {interp, domain, bsp, knots, controlpoints, polyform, t, zeros, zerodomain},

    interp = Interpolation[samplelist, InterpolationOrder -> 3, Method -> "Spline"];
    {domain} = interp["Domain"];
    bsp = First@Cases[interp, _BSplineFunction, Infinity];
    {knots} = bsp["Knots"];
    controlpoints = bsp["ControlPoints"];
	(*zerodomain= Transpose[{Most[knots[[3;;-3]]],Rest[knots[[3;;-3]]]}][[Flatten[Position[Most[controlpoints]*Rest[controlpoints],_?NonPositive]]]];*)
	zerodomain = Transpose[{Most[samplelist[[All,1]]],Rest[samplelist[[All,1]]]}][[Flatten[Position[Most[samplelist[[All,2]]]*Rest[samplelist[[All,2]]],_?NonPositive]]]];
    polyform = PiecewiseExpand[Sum[
            controlpoints[[i + 1]] * PiecewiseExpand[BSplineBasis[{3, knots}, i, t]],
            {i, 0, Length[controlpoints] - 1}
        ]];
	polyform = PiecewiseExpand[Piecewise[{{polyform,(Or@@((#[[1]] <= t <= #[[2]])&/@zerodomain))}},1]];
    zeros = Flatten[Solve[polyform == 0, Reals]];
    ({#, interp'@#}&) @* (t/.#&) /@ zeros
];



End[];


EndPackage[];
