#!/usr/bin/env wolframscript
(* ::Package:: *)

AppendTo[$Path, DirectoryName[$InputFileName]];

SourceFiles = {
	"GraphvizUtils.wl",
	"DRPLAN.wl"
};

WriteString[$Output, "Initializing DRPLAN... "];

Get /@ SourceFiles;
WriteString[$Output, "Done"];
