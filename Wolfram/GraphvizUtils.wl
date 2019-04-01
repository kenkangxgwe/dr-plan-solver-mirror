(* ::Package:: *)

BeginPackage["GraphvizUtils`"];
ClearAll[Evaluate[Context[] <> "*"]];


ImportGraphviz::usage = "ImportGraphviz[dotfile_String] imports a graph from a dotfile.";
ExportGraphviz::usage = "ExportGraphviz[graph_Graph] exports a dotfile string from a realization.";
ConvertDotToCrdsAndCons::usage = "Convert a dotfile to Crds and Cons file.";
HexColor::usage = "HexColor represent the discrete hexdecimal color space."


Begin["`Private`"];
ClearAll[Evaluate[Context[] <> "*"]];


HexColor /: ColorConvert[color_?ColorQ, HexColor] := 
	HexColor[StringJoin["#",
		IntegerString[Round[255 * #], 16, 2]&
		/@ (List @@ ColorConvert[color, RGBColor])
	]]

ColorQ[HexColor[hex_String]] ^:= ColorQ[RGBColor[hex]]

HexColor /: ColorConvert[HexColor[hex_String]?ColorQ, colorSpace_] := ColorConvert[RGBColor[hex], colorSpace]


Options[ImportGraphviz]= {
    "ImportRealization" -> True,
    "ImportColor" -> True
};

ImportGraphviz[dotfile_String, OptionsPattern[]] := Module[
	{
		graphVertList = ToExpression[Import[dotfile, "VertexList"]],
		graphEdgeList = (ToExpression[First[#]] <-> ToExpression[Last[#]])& /@ EdgeList[Import[dotfile, "Graph"]],
		coordMap, colorMap, edgeTypeMap, boundaryMap,
		edgeCustomProps, edgeWeight, edgeStyle, vertexCustomProps, fullGraph
	},

	coordMap = Thread[graphVertList -> (("Coordinates" /. #)& /@ Import[dotfile, "VertexAttributes"])];
    edgeWeight = (N[Norm[(First[#] /. coordMap) - (Last[#] /. coordMap)]])& /@ graphEdgeList;
    colorMap = If[TrueQ[OptionValue["ImportColor"]],
		(Fold[ColorConvert, Replace["Color", #], {HexColor, RGBColor}])&
		/@ Import[dotfile, "EdgeAttributes"],
		Table[Black, EdgeCount[graphEdgeList]]
	];
	edgeStyle = MapThread[#1 -> #2&, {graphEdgeList, colorMap}];
	edgeTypeMap = colorMap /. {
		RGBColor["#000000"] | RGBColor["#C0C0C0"] -> "Partial",
		RGBColor["#00FF00"] -> "Add",
		RGBColor["#FF0000"] | RGBColor["#FFC0CB"] -> "Drop"
	};
	boundaryMap = colorMap /. {
		RGBColor["#000000"] | RGBColor["#00FF00"] | RGBColor["#FF0000"] -> False,
		RGBColor["#C0C0C0"] | RGBColor["#FFC0CB"] -> True
	};
	edgeCustomProps = MapThread[( #1 -> {"EdgeType" -> #2, "BoundaryQ" -> #3})&, {graphEdgeList, edgeTypeMap, boundaryMap}];
    vertexCustomProps = (# -> {"Flip" -> False})& /@ graphVertList;
    Graph[graphVertList, graphEdgeList, Properties -> vertexCustomProps ~ Join ~ edgeCustomProps,
        EdgeWeight -> edgeWeight, EdgeStyle -> edgeStyle, VertexLabels -> "Name", VertexCoordinates->
        If[OptionValue["ImportRealization"],
            (graphVertList /.coordMap),
            Automatic
        ]
    ]
    (*Print[vertexPropRules // Column];*)
    (*Print[edgePropRules // Column];*)
    (*Print[VertexList[fullGraph]/.coordMap];*)
];


(* ::Subsection:: *)
(*ExportGraphviz*)


Options[ExportGraphviz] := {
	"ScaleRatio" -> 1.0
};

ExportGraphviz[graph_Graph, OptionsPattern[]] := Module[
	{
		vertexlist, edgelist, vertexcoords, vertexdot, edgedot, width, sr
	},
	
	vertexlist = VertexList[graph];
	vertexcoords = (VertexCoordinates/.Options[graph]);
	width = Floor[Log10[Max[vertexlist]]+1];
	sr = OptionValue["ScaleRatio"];
	vertexdot = MapThread[("  "
		<> StringPadLeft[ToString[#1], width, "0"] <> " [label=\"" <> StringPadLeft[ToString[#1], width, "0"]
		<> "\", width=0, height=0; pos=\""
		<> ToString[First@#2] <> "," <> ToString[Last@#2]
		<> "!\"];\n"
	)&, {vertexlist, ScalingTransform[{sr,sr}]@vertexcoords}];
	edgelist = EdgeList[graph];
	edgedot = ("  "
		<> StringPadLeft[ToString[First@#1], width, "0"] <> "--" <> StringPadLeft[ToString[Last@#1], width, "0"]
		<> " [color=\"black\", penwidth=1];\n")&/@edgelist;
	StringJoin["graph G {\n", vertexdot, "\n", edgedot, "}"]
];


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
];


End[];


EndPackage[];
