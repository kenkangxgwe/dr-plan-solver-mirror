#!/usr/bin/env wolframscript
(* ::Package:: *)

PrependTo[$Path, DirectoryName[$InputFileName]]

ImportContexts = {
	"DRPLAN`Core`",
	"DRPLAN`Solver`",
	"DRPLAN`Visualizer`"
}

WriteString[$Output, "Initializing DRPLAN... "]

Get /@ ImportContexts

WriteString[$Output, "Done"]
