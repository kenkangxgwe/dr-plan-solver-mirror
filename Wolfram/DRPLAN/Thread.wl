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
(*Beads Threading*)


BeginPackage["DRPLAN`Thread`"]
ClearAll[Evaluate[Context[] <> "*"]]


threadZeros::usage = "threadZeros[zeroTuples_List] connects the zero points using a number of threads, so that each thread is a D-flip."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


(*
    We treat the sample results as beads.
    They are distributed on the c_i vs c_0 plane along several curves which are D-flips.
    We want to categorize them into different flips using the distance and derivative information.
*)


NewThread[beads_] := <|"beads" -> beads, "terminated" -> {}, "active" -> {}, "position" -> 0|>

CreateLine[state_Association, index_Integer] := ReplacePart[state, "active" -> Append[state["active"], CreateLine[index, state["position"]]]]
CreateLine[index_Integer, pos_Integer] := <|"index" -> index, "beads" -> {}, "start" -> pos|>

TerminateLine[state_Association, index_Integer] := Module[
	{
		activeLines, terminatingIdx, terminatedLines
	},
	
	activeLines = state["active"];
	terminatingIdx = FirstPosition[activeLines, _?(#index == index&), {}, {1}, Heads -> False];
	terminatedLines = Join[state["terminated"], Part[activeLines, terminatingIdx]];
	activeLines = Delete[activeLines, terminatingIdx];
	ReplacePart[state, {"active" -> activeLines, "terminated" -> terminatedLines}]
] 

AllLines[state_Association] := With[
	{
		activeLines = state["active"], terminatedLines = state["terminated"],
		numBeads = Length[state["beads"]]
	},
	
	(Table[Missing[], #start] ~Join~ #beads ~Join~ Table[Missing[], numBeads - #start - Length[#beads]]&) /@ (activeLines ~Join~ terminatedLines)
	
]


ThreadBeads[state_Association] := With[
    {
        position = state["position"] + 1
    },
	
	ReplacePart[state, {
        "position" -> position,
        "active" -> Table[
            ReplacePart[activeLine, "beads" -> Append[
                activeLine["beads"],
                Part[state["beads"], position, activeLine["index"], 1]
            ]],
            {activeLine, state["active"]}
        ]
    }]
]


(*
    Dynamically updates the index of thread according to current offset
*)
ChangeLineIdx[state_Association, offsets_List] := (

    ReplacePart[state, "active" -> Table[
        ReplacePart[activeLine, "index" -> (
            activeLine["index"] + Part[offsets, activeLine["index"]]
        )],
        {activeLine, state["active"]}
    ]]
)


(*
    TODO: merge threadLine and threadStep,
    or maybe not?
*)


(* threadStep *)
threadLine[state_Association, step_] := Module[
    {
        numLines,
        activeIds, terminatedIds,
        newState = state
    },

    numLines = Length[step];
    activeIds = Through[state["active"]["index"]];

    (* terminates lines that out of boundary *)
    terminatedIds = Select[activeIds, (# <  1 || # > numLines)&]; 
    newState = Fold[TerminateLine, newState, terminatedIds];

    (* add new lines *)
    activeIds = Complement[activeIds, terminatedIds];
    newState = Fold[CreateLine, newState, Complement[Range[numLines], activeIds]];
    
    ChangeLineIdx[ThreadBeads[newState], step]

]


(*
    Leverages the derivatives to decide the connection between last and current steps.
    Returns the offset between last and current points.
 *)
threadStep[firstPair_, secondPair_] := (
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

)


(*
    Rare cases where there is not enough tuples in the list,
    not insteresting
*)
threadZeros[{}] := {{}, {}}
threadZeros[{zeroTuple_}] := Transpose[{Part[zeroTuple, All, 1]}]
(* Use Thread to divide tuples into different branches *)
threadZeros[zeroTuples:{_, __}] := Module[
    {
        steps = MapThread[threadStep, {Most @ zeroTuples, Rest @ zeroTuples}]
    },

    AllLines[Fold[threadLine, NewThread[zeroTuples], steps]]
]


End[]


EndPackage[]