#!/usr/bin/env wolframscript
(* ::Package:: *)

$Path = Join[{
	DirectoryName[$InputFileName],
	FileNameJoin[{DirectoryName[$InputFileName], "extras", "Matypetica", "src"}]
}, $Path]

ImportContexts = {
	"DRPLAN`Core`",
	"DRPLAN`Solver`",
	"DRPLAN`Visualizer`",
	"DRPLAN`OffsetSolver`",
	"DRPLAN`DFSSolver`"
}

WriteString[$Output, "Initializing DRPLAN... "]

Get /@ ImportContexts

WriteString[$Output, "Done"]
