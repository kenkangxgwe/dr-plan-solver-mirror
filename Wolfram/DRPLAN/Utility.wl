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
(*DRPLAN Utility*)


BeginPackage["DRPLAN`Utility`"]
ClearAll[Evaluate[Context[] <> "*"]]


Displacement::usage = "Displacement[node_DRNode, node_DRNode] combines two realizations for the same linkage and draw displancement vectors."
RigidityMatrix::usage = "RigidityMatrix[graph_Graph] returns the rigidity matrix."
InfinitesimallyRigidQ::usage = "InfinitismallyRigidQ[graph_Graph] gives True if the input DRNode is infinitesimally rigid ."
FlipVectorIndices::usage = "FlipVectorIndices[graph_Graph] gives a list of indices that should be used when computing the flip vector."
ComputeFlipVector::usage = "ComputeFlipVector[graph_Graph] gives a flip vector of the input graph."
PlanToOrignal::usage = "PlanToOriginal[graph_Graph, o:OptionsPatterns[]] gives the graph with all the Cayley parameters removed and dropped edges added back."
PlanToTwotree::usage = "PlanToTwotree[graph_Graph, o:OptionsPatterns[]] gives the graph with all the Cayley parameters removed and without dropped edges."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DRPLAN`Core`"]


(* ::Item:: *)
(*Displacement*)


Options[Displacement] = {"AddEdge" -> True}
Displacement::neq = "Two graphs have different number of vertices."
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
]


(* ::Item:: *)
(*RigidityMatrix*)


RigidityMatrix[graph_Graph] := With[
    {
        edgelist = EdgeList[graph]
    },

    SparseArray[Join @@ (MapIndexed[RigidityElement[graph], edgelist])]
]


(* ::Item:: *)
(*RigidityElement*)


RigidityElement[graph_][UndirectedEdge[v1_Integer, v2_Integer], {row_Integer}] := With[
    {
        displacement = Subtract @@ PropertyValue[{graph, #}, VertexCoordinates]& /@ {v1, v2}
    },

    {
        {row, 2 * v1 + 1} -> First@displacement,
        {row, 2 * v1 + 2} -> Last@displacement,
        {row, 2 * v2 + 1} -> First@(-displacement),
        {row, 2 * v2 + 2} -> Last@(-displacement)
    }
]


(* ::Item:: *)
(*InfinitesimallyRigidQ*)


InfinitesimallyRigidQ[graph_Graph] := With[
    {
        rigidityMatrix = RigidityMatrix[graph]
    },

    (2 * VertexCount[graph] - MatrixRank[rigidityMatrix]) == 3
]


FlipVectorIndices[graph_Graph] := (
    graph
    // VertexList
    // Drop[#, 2]&
    // Map[(v0 \[Function] (
        AdjacencyList[graph, v0]
        // Select[(# < v0 && PropertyValue[{graph, # <-> v0}, "EdgeType"] != "Drop")&]
        // Append[v0]
    ))]
)


ComputeFlipVector[graph_Graph] := (
    graph
    // FlipVectorIndices
    // Select[
        Map[PropertyValue[{graph, #}, VertexCoordinates]&]
        /* Apply[ccwQ]
    ]
    // Part[#, All, -1]&
)


ccwQ[p_List, q_List, r__List] := Det[Append[#, 1] & /@ {p, q, r}] // Negative


Options[PlanToOrignal] = {
    "IncludeBoundaries" -> True,
    "KeepColors" -> False
}

PlanToOrignal[graph_Graph, o:OptionsPattern[]] := Block[
    {
        newGraph = graph
    },

    If[!OptionValue["KeepColors"],
        newGraph
        // EdgeList
        // Select[(PropertyValue[{newGraph, #}, "EdgeType"] == "Drop")&]
        // Map[(PropertyValue[{newGraph, #}, EdgeStyle] =
            If[PropertyValue[{newGraph, #}, "BoundaryQ"],
                Gray,
                Black
            ]
        )&]
    ];

    newGraph
    // EdgeList
    // Select[PropertyValue[{newGraph, #}, "EdgeType"] == "Add" ||
        (!OptionValue["IncludeBoundaries"] &&
        PropertyValue[{newGraph, #}, "BoundaryQ"])&
    ]
    // EdgeDelete[newGraph, #]&
]


Options[PlanToTwotree] = {
    "IncludeBoundaries" -> True,
    "KeepColors" -> True
}

PlanToTwotree[graph_Graph, o:OptionsPattern[]] := Block[
    {
        newGraph = graph
    },

    If[!OptionValue["KeepColors"],
        newGraph
        // EdgeList
        // Select[PropertyValue[{newGraph, #}, "EdgeType"] == "Add"&]
        // Map[PropertyValue[{newGraph, #}, EdgeStyle -> Black]&]
    ];

    newGraph
    // EdgeList
    // Select[PropertyValue[{newGraph, #}, "EdgeType"] == "Drop"&]
    // EdgeDelete[newGraph, #]&
]


End[]


EndPackage[]