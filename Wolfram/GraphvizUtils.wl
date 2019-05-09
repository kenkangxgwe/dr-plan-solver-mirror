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


BeginPackage["GraphvizUtils`"]
ClearAll[Evaluate[Context[] <> "*"]]


ImportGraphviz::usage = "ImportGraphviz[dotfile_String] imports a graph from a dotfile."
ExportGraphviz::usage = "ExportGraphviz[graph_Graph] exports a dotfile string from a realization."
ConvertDotToCrdsAndCons::usage = "Convert a dotfile to Crds and Cons file."
HexColor::usage = "HexColor[\"#rrggbb\"] represent the discrete hexdecimal color space."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


(* ::Subsection:: *)
(*HexColor*)


HexColor /: ColorConvert[color_?ColorQ, HexColor] := 
	HexColor[StringJoin["#",
		IntegerString[Round[255 * #], 16, 2]&
		/@ (List @@ ColorConvert[color, RGBColor])
	]]

ColorQ[HexColor[hex_String]] ^:= ColorQ[RGBColor[hex]]

HexColor /: ColorConvert[HexColor[hex_String]?ColorQ, colorSpace_] := ColorConvert[RGBColor[hex], colorSpace]

X11Color["Black"] = RGBColor["#000000"]
X11Color["Gray"] = RGBColor["#C0C0C0"]
X11Color["Red"] = RGBColor["#FF0000"]
X11Color["Green"] = RGBColor["#00FF00"]
X11Color["Pink"] = RGBColor["#FFC0CB"]


(* ::Subsection:: *)
(*ImportGraphviz*)


Options[ImportGraphviz]= {
    "ImportRealization" -> True,
    "ImportColor" -> True
}

ImportGraphviz[dotfile_String, o:OptionsPattern[]] := Module[
	{
		graphVertList = ToExpression[Import[dotfile, "VertexList"]],
		graphEdgeList = (ToExpression[First[#]] <-> ToExpression[Last[#]])& /@ EdgeList[Import[dotfile, "Graph"]],
		coordMap, colorMap, edgeTypeMap, boundaryMap,
		edgeCustomProps, edgeWeight, edgeStyle, vertexCustomProps, fullGraph,
		(* options *)
		importRealization, importColor
	},

	{importRealization, importColor} = OptionValue[ImportGraphviz, {o}, {"ImportRealization", "ImportColor"}];

	(* vertex coordinates *)
	coordMap = Thread[graphVertList -> (("Coordinates" /. #)& /@ Import[dotfile, "VertexAttributes"])];

	(* edge lengths (edgeWeight) *)
    edgeWeight = (N[Norm[(First[#] /. coordMap) - (Last[#] /. coordMap)]])& /@ graphEdgeList;

	(* edge color *)
    colorMap = If[TrueQ[importColor],
		(* Convert to HexColor space to discretize the color value *)
		(Fold[ColorConvert, Replace["Color", #], {HexColor, RGBColor}])&
		/@ Import[dotfile, "EdgeAttributes"],
		Table[X11Color["Black"], EdgeCount[graphEdgeList]]
	] // Replace[{
		res:{__RGBColor} :> res,
		err_ :> Throw[err]
	}];
	edgeStyle = MapThread[#1 -> #2&, {graphEdgeList, colorMap}];
	(*
		edge type:
		- "Partial": fixed edge of the two tree,
		- "Add": Cayley parameter,
		- "Drop": dropped edge excluded from the two tree.
	*)
	edgeTypeMap = colorMap /. {
		X11Color["Black"] | X11Color["Gray"] -> "Partial",
		X11Color["Green"] -> "Add",
		X11Color["Red"] | X11Color["Pink"] -> "Drop"
	} // Replace[{
		res:{("Partial" | "Add" | "Drop")..} :> res,
		err_ :> Throw[err]
	}];
	(* boundary edge *)
	boundaryMap = colorMap /. {
		X11Color["Black"] | X11Color["Green"] | X11Color["Red"] -> False,
		X11Color["Gray"] | X11Color["Pink"] -> True
	} // Replace[{
		res:{__?BooleanQ} :> res,
		err_ :> Throw[err]
	}];
	(*
		custom edge properties:
		- "EdgeType"
		- "BoundaryQ"
	*)
	edgeCustomProps = MapThread[( #1 -> {"EdgeType" -> #2, "BoundaryQ" -> #3})&, {graphEdgeList, edgeTypeMap, boundaryMap}];
	(*
		custom vertex properties:
		- "Flip"
	*)
    vertexCustomProps = (# -> {"Flip" -> False})& /@ graphVertList;
	(*
		native edge properties:
		- EdgeWeight: edge length
		- EdgeStyle

		native vertex properties:
		- VertexLabels: vertex id (from 0)
		- VertexCoordinates
	*)
    Graph[graphVertList, graphEdgeList, Properties -> vertexCustomProps ~ Join ~ edgeCustomProps,
        EdgeWeight -> edgeWeight, EdgeStyle -> edgeStyle, VertexLabels -> "Name", VertexCoordinates->
        If[importRealization,
            (graphVertList /.coordMap),
            Automatic
        ]
    ]
    (*Print[vertexPropRules // Column];*)
    (*Print[edgePropRules // Column];*)
    (*Print[VertexList[fullGraph]/.coordMap];*)
]


(* ::Subsection:: *)
(*ExportGraphviz*)


Options[ExportGraphviz] := {
	"ScaleRatio" -> 1.0
}

ExportGraphviz[graph_Graph, o:OptionsPattern[]] := Module[
	{
		vertexlist, edgelist, vertexcoords, vertexdot, edgedot, width, scaleRatio
	},
	
	{scaleRatio} = OptionValue[ExportGraphviz, {o}, {"ScaleRatio"}];

	vertexlist = VertexList[graph];
	vertexcoords = (VertexCoordinates /. Options[graph]);
	(* padding 0 bits *)
	width = Floor[Log10[Max[vertexlist]] + 1];
	vertexdot = MapThread[StringJoin["  ",
		StringPadLeft[ToString[#1], width, "0"],
		" [label=\"",
		StringPadLeft[ToString[#1], width, "0"],
		"\", width=0, height=0; pos=\"",
		ToString[First @ #2],
		",",
		ToString[Last @ #2],
		"!\"];\n"
	]&, {vertexlist, ScalingTransform[{scaleRatio, scaleRatio}] @ vertexcoords}];

	edgelist = EdgeList[graph];
	edgedot = StringJoin["  ",
		StringPadLeft[ToString[First @ #1], width, "0"],
		"--",
		StringPadLeft[ToString[Last @ #1], width, "0"],
		" [color=\"black\", penwidth=1];\n"
	]& /@ edgelist;
	StringJoin["graph G {\n", vertexdot, "\n", edgedot, "}"]
]


(* ::Subsection:: *)
(*ConvertDotToCrdsAndCons*)


ConvertDotToCrdsAndCons[dotfile_String] := Module[
	{
		graph
	},

	graph = ImportGraphviz[dotfile, "ImportRealization"->True];
	{
		Column[(ToString[#[[1]]]<>" "<>ToString[#[[2]]])&/@(VertexCoordinates/.Options[graph])],
		Column[(ToString[#[[1]]]<>" "<>ToString[#[[2]]])&/@EdgeList[graph]]
	}
]


End[]


EndPackage[]
