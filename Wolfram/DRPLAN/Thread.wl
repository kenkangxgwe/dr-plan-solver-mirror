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


ThreadZeros::usage = "ThreadZeros[zeroTuples_List] connects the zero points using a number of threads, so that each thread is a D-flip."
InterpolationPiece::usage = "InterpolationPiece[point] is used to split the interpolating function piece-wisely."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


InterpolationPiece[InterpolationPiece[p_]] := InterpolationPiece[p]


(*
    We treat the sample results as beads.
    They are distributed on the c_i vs c_0 plane along several curves which are D-flips.
    We want to categorize them into different flips using the distance and derivative information.
*)


(*
    TODO: merge threadLine and threadStep,
    or maybe not?
*)


(* threadStep *)
threadLine[state_Association, step_] := With[
    {
        forkLeftIds = Tally[step] // Cases[{idx_, _?(GreaterThan[1])} :> idx]
    },

    state
    // ReplacePart["position" -> (state["position"] + 1)]
    // ReplacePart[Key["terminated"] -> Join[
        state["terminated"],
        Part[state["active"], Complement[Range[Length[state["active"]]], Flatten[step]]]
    ]]
    // ReplacePart[Key["active"] -> Table[
        With[
        {
            nextBead = Part[state["beads"], state["position"] + 1, rightIdx, 1]
        },
            Replace[
                Part[step, rightIdx],
                {
                    0 :> (
                        {<|"beads" -> {nextBead}, "start" -> state["position"]|>}
                    ),
                    leftIdx_Integer?(MemberQ[forkLeftIds, #]&) :> (
                        Part[state["active"], leftIdx]
                        // Flatten
                        // Map[ReplacePart[#, "beads" -> Append[MapAt[InterpolationPiece, #beads, -1], nextBead]]&]
                    ),
                    leftIdx_Integer :> (
                        Part[state["active"], leftIdx]
                        // Flatten
                        // Map[ReplacePart[#, "beads" -> Append[#beads, nextBead]]&]
                    ),
                    leftIdx:{_Integer, _Integer} :> (
                        Part[state["active"], leftIdx]
                        // Flatten
                        // Map[ReplacePart[#, "beads" -> Append[#beads, InterpolationPiece[nextBead]]]&]
                    )
                }
            ]
        ],
    {rightIdx, Length[step]}]]
]


(*
    Leverages the derivatives to decide the connection between last and current steps.
    Returns the last thread indices to use for the next threads.
 *)
threadStep::strcase = "Strange `1` case happens at index `2`: `3`."
threadStep[firstPair_, secondPair_, index_Integer, {min_, max_}] := With[
    {
        firstSigns = Part[firstPair, All, 2] // Sign,
        secondSigns = Part[secondPair, All, 2] // Sign
    },
    {firstPair, secondPair}
    // Map[Length]
    // Replace[{
        {len1_, 0} :> (
            (* all ends *)
            If[len1 > 2,
                Message[threadStep::strcase, ToString[len1] <> "-0", index, ToString[len1] <> " is larger than 2"]
            ];
            {}
        ),
        {0, len2_} :> (
            (* all starts *)
            If[len2 > 2 && index > 0,
                Message[threadStep::strcase, "0-" <> ToString[len2], index, ToString[len2] <> " is larger than 2"]
            ];
            Table[0, len2]
        ),
        {3, 3} :> (
            If[{firstSigns, secondSigns} // MapThread[Equal] // Apply[And] // Not,
                Message[threadStep::strcase, "3-3", index, "derivatives mismatch for " <> ToString[{firstSigns, secondSigns} // Thread]]
            ];
            (* Continue *)
            Range[3]
        ),
        {3, 2} :> (
            If[EuclideanDistance[Part[firstPair, 2], Part[secondPair, 1]] < EuclideanDistance[Part[firstPair, 2], Part[secondPair, -1]],
                (* 2-1, 3-2 *)
                If[Part[{firstSigns, secondSigns}, All, -1] // Apply[Equal] // Not,
                    Message[threadStep::strcase, "3-2", index, "derivatives mismatch at last tuple " <> ToString[Part[{firstSigns, secondSigns}, All, -1]] <> ", if 2-1 & 3-2"]
                ];
                If[((Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) / (Part[firstPair, 2, 1] - Part[firstPair, 1, 1])) < 0.6,
                    (* 1-1 *)
                    If[Take[firstSigns, 2] // Apply[Equal],
                        Message[threadStep::strcase, "3-2", index, "derivatives of " <> ToString[firstPair] <> " are the same sign at first two points, if 1 & 2 join 1"]
                    ];
                    {{1, 2}, 3},
                    (* 1 ends *)
                    {2, 3}
                ],
                (* 1-1, 2-2 *)
                If[Part[{firstSigns, secondSigns}, All, 1] // Apply[Equal] // Not,
                    Message[threadStep::strcase, "3-2", index, "derivative signs mismatch at first tuple " <> ToString[Part[{firstPair, secondPair}, All, 1]] <> ", if 1-1, 2-2."]
                ];
                If[((Part[secondPair, -1, 1] - Part[firstPair, 2, 1]) / (Part[firstPair, -1, 1] - Part[firstPair, 2, 1])) > 0.4,
                    (* 3-2 *)
                    If[Take[firstSigns, -2] // Apply[Equal],
                        Message[threadStep::strcase, "3-2", index, "derivatives are same at last two points " <> ToString[Take[firstPair, -2]] <> ", if 2 & 3 join 2."]
                    ];
                    {1, {2, 3}},
                    (* 3 ends *)
                    {1, 2}
                ]
            ]
        ),
        {2, 3} :> (
            If[EuclideanDistance[Part[firstPair, 1], Part[secondPair, 2]] < EuclideanDistance[Part[firstPair, -1], Part[secondPair, 2]],
                (* 1-2, 2-3 *)
                If[Part[{firstSigns, secondSigns}, All, -1] // Apply[Equal] // Not,
                    Message[threadStep::strcase, "2-3", index, "derivative signs mismatch at last tuple " <> ToString[Part[{firstPair, secondPair}, All, -1]] <> ", if 1-2, 2-3."]
                ];
                If[(Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) / (Part[secondPair, 2, 1] - Part[secondPair, 1, 1]) < 0.6,
                    (* 1-1 *)
                    If[Take[secondSigns, 2] // Apply[Equal],
                        Message[threadStep::strcase, "2-3", index, "derivative signs are same at first two points " <> ToString[Take[secondPair, 2]] <> ", if 1 forks 1 & 2."]
                    ];
                    {1, 1, 2},
                    (* 1 starts *)
                    {0, 1, 2}
                ],
                (* 1-1, 2-2 *)
                If[Part[{firstSigns, secondSigns}, All, 1] // Apply[Equal] // Not,
                    Message[threadStep::strcase, "2-3", index, "derivative signs mismatch at first tuple " <> ToString[Part[{firstPair, secondPair}, All, 1]] <> ", if 1-1, 2-2."]
                ];
                If[((Part[firstPair, -1, 1] - Part[secondPair, 2, 1]) / (Part[secondPair, -1, 1] - Part[secondPair, 2, 1])) > 0.4,
                    (* 2-3 *)
                    If[Take[firstSigns, -2] // Apply[Equal],
                        Message[threadStep::strcase, "2-3", index, "derivative signs are same at last two points " <> ToString[Take[firstPair, -2]] <> ", if 2 forks 2 & 3."]
                    ];
                    {1, 2, 2},
                    (* 3 starts *)
                    {1, 2, 0}
                ]
            ]

        ),
        {2, 2} :> (
            If[Part[firstPair, 1, 1] < Part[secondPair, 1, 1],
                (* - *)
                If[Part[firstPair, -1, 1] < Part[secondPair, 1, 1],
                    (* --__ *)
                    If[(Part[firstPair, -1, 1] - Part[firstPair, 1, 1]) / (Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) > 0.6 &&
                        (Part[secondPair, 1, 1] - Part[firstPair, -1, 1]) / (Part[secondPair, -1, 1] - Part[firstPair, -1, 1]) < 0.4 &&
                        Part[firstSigns, -1] == Part[lastSigns, 1],
                        {2, 0},
                        {0, 0}
                    ],
                    (* -_ *)
                    If[Part[firstPair, -1, 1] > Part[secondPair, -1, 1],
                        (* -__- *)
                        If[{firstSigns, secondSigns} // MapThread[Equal] // Apply[And] // Not,
                            Message[threadStep::strcase, "2-2", index, "derivatives mismatch for " <> ToString[{firstSigns, secondSigns} // Thread]]
                        ];
                        (* 1-1, 2-2 *)
                        If[(Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) / (Part[firstPair, -1, 1] - Part[firstPair, 1, 1]) > 0.4,
                            Message[threadStep::strcase, "2-2", index, "first tuple doesn't close enough if 1-1: " <> ToString[{firstPair, secondPair}]]
                        ];
                        If[(Part[secondPair, -1, 1] - Part[secondPair, 1, 1]) / (Part[firstPair, -1, 1] - Part[secondPair, 1, 1]) < 0.6,
                            Message[threadStep::strcase, "2-2", index, "first tuple doesn't close enough if 2-2: " <> ToString[{firstPair, secondPair}]]
                        ];
                        {1, 2},
                        (* -_-_ *)
                        {
                            (Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) / (Part[firstPair, -1, 1] - Part[firstPair, 1, 1]),
                            (Part[firstPair, -1, 1] - Part[secondPair, 1, 1]) / (Part[secondPair, -1, 1] - Part[secondPair, 1, 1])
                        }
                        // Replace[{
                            {_?(Between[{0.4, 0.6}]), _?(Between[{0.4, 0.6}])} :> ({{1,2}, 2}),
                            {_?(Between[{0.4, 0.6}]), _?(LessThan[0.4])} :> ({{1,2}, 0}),
                            {_?(GreaterThan[0.6]), _?(Between[{0.4, 0.6}])} :> ({2, 2}),
                            {_?(Between[{0.4, 0.6}]), _?(GreaterThan[0.6])} :> (
                                Message[threadStep::strcase, "2-2", index, "first tuple doesn't close enough if 1-1: " <> ToString[{firstPair, secondPair}]];
                                {0, 2}
                            ),
                            {_?(LessThan[0.4]), _?(Between[{0.4, 0.6}])} :> (
                                Message[threadStep::strcase, "2-2", index, "last tuple doesn't close enough if 2-2: " <> ToString[{firstPair, secondPair}]];
                                {1, 0}
                            ),
                            {_?(LessThan[0.4]), _?(GreaterThan[0.6])} :> (
                                {1, 2}
                            ),
                            {_?(GreaterThan[0.6]), _?(LessThan[0.4])} :> (
                                Message[threadStep::strcase, "2-2", index, "first point isn't continued" <> ToString[{firstPair, secondPair}]];
                                {2, 0}
                            ),
                            {_?(GreaterThan[0.6]), _?(GreaterThan[0.6])} :> (
                                Message[threadStep::strcase, "2-2", index, "first point isn't continued" <> ToString[{firstPair, secondPair}]];
                                {0, 2}
                            ),
                            {_?(LessThan[0.4]), _?(LessThan[0.4])} :> (
                                Message[threadStep::strcase, "2-2", index, "second point isn't continued" <> ToString[{firstPair, secondPair}]];
                                {1, 0}
                            ),
                            err_ :> (
                                Message[threadStep::strcase, "2-2", index, "impossible result when computing ratio: " <> ToString[err]];
                            )
                        }]
                    ]
                ],
                (*
                    _
                *)
                If[Part[firstPair, 1, 1] > Part[secondPair, -1, 1],
                    (* __-- *)
                    If[(Part[secondPair, -1, 1] - Part[secondPair, 1, 1]) / (Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) > 0.6 &&
                        (Part[firstPair, 1, 1] - Part[secondPair, -1, 1]) / (Part[firstPair, -1, 1] - Part[secondPair, -1, 1]) < 0.4 &&
                        Part[firstSigns, 1] == Part[lastSigns, -1],
                        {0, 1},
                        {0, 0}
                    ],
                    (* _- *)
                    If[Part[firstPair, -1, 1] < Part[secondPair, -1, 1],
                        (* _--_ *)
                        (* 1-1, 2-2 *)
                        If[{firstSigns, secondSigns} // MapThread[Equal] // Apply[And] // Not,
                            Message[threadStep::strcase, "2-2", index, "derivatives mismatch for " <> ToString[{firstSigns, secondSigns} // Thread]]
                        ];
                        If[(Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) / (Part[firstPair, -1, 1] - Part[secondPair, 1, 1]) > 0.4,
                            Message[threadStep::strcase, "2-2", index, "first tuple doesn't close enough if 1-1: " <> ToString[{firstPair, secondPair}]]
                        ];
                        If[(Part[firstPair, -1, 1] - Part[firstPair, 1, 1]) / (Part[secondPair, -1, 1] - Part[firstPair, 1, 1]) < 0.6,
                            Message[threadStep::strcase, "2-2", index, "first tuple doesn't close enough if 2-2: " <> ToString[{firstPair, secondPair}]]
                        ];
                        {1, 2},
                        (* _-_- *)
                        {
                            (Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) / (Part[secondPair, -1, 1] - Part[secondPair, 1, 1]),
                            (Part[secondPair, -1, 1] - Part[firstPair, 1, 1]) / (Part[firstPair, -1, 1] - Part[firstPair, 1, 1])
                        } // Replace[{
                            {_?(Between[{0.4, 0.6}]), _?(Between[{0.4, 0.6}])} :> ({1, {1,2}}),
                            {_?(GreaterThan[0.6]), _?(Between[{0.4, 0.6}])} :> ({0, {1, 2}}),
                            {_?(Between[{0.4, 0.6}]), _?(LessThan[0.4])} :> ({1, 1}),
                            {_?(Between[{0.4, 0.6}]), _?(GreaterThan[0.6])} :> (
                                Message[threadStep::strcase, "2-2", index, "first tuple doesn't close enough if 1-1: " <> ToString[{firstPair, secondPair}]];
                                {0, 2}
                            ),
                            {_?(LessThan[0.4]), _?(Between[{0.4, 0.6}])} :> (
                                Message[threadStep::strcase, "2-2", index, "last tuple doesn't close enough if 2-2: " <> ToString[{firstPair, secondPair}]];
                                {1, 0}
                            ),
                            {_?(LessThan[0.4]), _?(GreaterThan[0.6])} :> (
                                {1, 2}
                            ),
                            {_?(GreaterThan[0.6]), _?(LessThan[0.4])} :> (
                                Message[threadStep::strcase, "2-2", index, "first point isn't continued: " <> ToString[{firstPair, secondPair}]];
                                {2, 0}
                            ),
                            {_?(GreaterThan[0.6]), _?(GreaterThan[0.6])} :> (
                                Message[threadStep::strcase, "2-2", index, "first point isn't continued: " <> ToString[{firstPair, secondPair}]];
                                {0, 2}
                            ),
                            {_?(LessThan[0.4]), _?(LessThan[0.4])} :> (
                                Message[threadStep::strcase, "2-2", index, "second point isn't continued: " <> ToString[{firstPair, secondPair}]];
                                {1, 0}
                            ),
                            err_ :> (
                                Message[threadStep::strcase, "2-2", index, "impossible result when computing ratio: " <> ToString[err]];
                                {0, 0}
                            )
                        }]
                    ]
                ]
            ]
        ),
        {3, 1} :> (
            Part[secondPair, 1, 1]
            // Replace[{
                _?(LessThan[Part[firstPair, 1, 1]]) :> ({1}),
                _?(LessThan[Part[firstPair, 2, 1]]) :> (
                    (Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) / (Part[firstPair, 2, 1] - Part[firstPair, 1, 1])
                    // Replace[{
                        _?(LessThan[0.4]) :> {1},
                        _?(Between[{0.4, 0.6}]) :> {{1,2}},
                        _?(GreaterThan[0.6]) :> {2}
                    }]
                ),
                _?(LessThan[Part[firstPair, 3, 1]]) :> (
                    (Part[secondPair, 1, 1] - Part[firstPair, 2, 1]) / (Part[firstPair, 3, 1] - Part[firstPair, 2, 1])
                    // Replace[{
                        _?(LessThan[0.4]) :> {2},
                        _?(Between[{0.4, 0.6}]) :> {{2,3}},
                        _?(GreaterThan[0.6]) :> {3}
                    }]
                ),
                _?(GreaterEqualThan[Part[firstPair, 3, 1]]) :> ({3}),
                err_ :> (
                    Message[threadStep::strcase, "3-1", index, "impossible result when computing ratio: " <> ToString[err]];
                    {0}
                )
            }]
        ),
        {1, 3} :> (
            Part[firstPair, 1, 1]
            // Replace[{
                _?(LessThan[Part[secondPair, 1, 1]]) :> ({1, 0, 0}),
                _?(LessThan[Part[secondPair, 2, 1]]) :> (
                    (Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) / (Part[secondPair, 2, 1] - Part[secondPair, 1, 1])
                    // Replace[{
                        _?(LessThan[0.4]) :> {1, 0, 0},
                        _?(Between[{0.4, 0.6}]) :> {1, 1, 0},
                        _?(GreaterThan[0.6]) :> {0, 1, 0}
                    }]
                ),
                _?(LessThan[Part[secondPair, 3, 1]]) :> (
                    (Part[firstPair, 1, 1] - Part[secondPair, 2, 1]) / (Part[secondPair, 3, 1] - Part[secondPair, 2, 1])
                    // Replace[{
                        _?(LessThan[0.4]) :> {0, 1, 0},
                        _?(Between[{0.4, 0.6}]) :> {0, 1, 1},
                        _?(GreaterThan[0.6]) :> {0, 0, 1}
                    }]
                ),
                _?(GreaterEqualThan[Part[secondPair, 3, 1]]) :> ({0, 0, 1}),
                err_ :> (
                    Message[threadStep::strcase, "1-3", index, "impossible result when computing ratio: " <> ToString[err]];
                    {0}
                )
            }]
        ),
        {2, 1} :> (
            If[EuclideanDistance[Part[firstPair, 1], Part[secondPair, 1]] < EuclideanDistance[Part[firstPair, -1], Part[secondPair, 1]],
                If[Part[firstPair, 1, 1] > Part[secondPair, 1, 1] &&
                    (Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) / (Part[firstPair, -1, 1] - Part[secondPair, 1, 1]) > 0.6,
                    Message[threadStep::strcase, "2-1", index, "The first tuple" <> ToString[Part[{firstPair, secondPair}, All, 1, 1]] <> " is further than the second point " <> Part[firstPair, -1, 1] <> ", if 1-1"];
                    {0},
                    (* 1-1 *)
                    If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, 1, 1]] > EuclideanDistance[Part[firstPair, 1, 1], min],
                        Message[threadStep::strcase, "2-1", index, "The first tuple" <> ToString[Part[{firstPair, secondPair}, All, 1]] <> " is further than one of them to the boudary " <> ToString[domain] <> ", if 1-1"]
                    ];
                    If[EuclideanDistance[Part[firstPair, -1, 1], Part[secondPair, 1, 1]] < EuclideanDistance[Part[firstPair, -1, 1], max] &&
                        Part[firstPair, 1, 1] < Part[secondPair, 1, 1] < Part[firstPair, -1, 1],
                        (* 2-1 *)
                        {{1, 2}},
                        (* 2 ends *)
                        {1}
                    ]
                ],
                If[Part[firstPair, -1, 1] < Part[secondPair, 1, 1] &&
                    (Part[firstPair, -1, 1] - Part[firstPair, 1, 1]) / (Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) < 0.4,
                    Message[threadStep::strcase, "2-1", index, "The last tuple" <> ToString[Part[{firstPair, secondPair}, All, -1, 1]] <> " is further than the first point " <> Part[firstPair, 1, 1] <> ", if 2-1"];
                    {0},
                    (* 2-1 *)
                    If[EuclideanDistance[Part[firstPair, -1, 1], Part[secondPair, 1, 1]] > EuclideanDistance[Part[firstPair, -1, 1], max],
                        Message[threadStep::strcase, "2-1", index, "The last tuple" <> ToString[Part[{firstPair, secondPair}, All, -1]] <> " is further than one of them to the boudary " <> ToString[domain] <> ", if 2-1"]
                    ];
                    If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, 1, 1]] < EuclideanDistance[Part[firstPair, 1, 1], min] &&
                        Part[firstPair, 1, 1] < Part[secondPair, 1, 1] < Part[firstPair, -1, 1],
                        (* 1-1 *)
                        {{1, 2}},
                        (* 1 ends *)
                        {2}
                    ]
                ]
            ]
        ),
        {1, 2} :> (
            If[EuclideanDistance[Part[firstPair, 1], Part[secondPair, 1]] < EuclideanDistance[Part[firstPair, 1], Part[secondPair, -1]],
                If[Part[firstPair, 1, 1] < Part[secondPair, 1, 1] &&
                    (Part[secondPair, 1, 1] - Part[firstPair, 1, 1]) / (Part[secondPair, -1, 1] - Part[firstPair, 1, 1]) > 0.6,
                    Message[threadStep::strcase, "1-2", index, "The first tuple" <> ToString[Part[{firstPair, secondPair}, All, 1, 1]] <> " is further than the second point " <> Part[secondPair, -1, 1] <> ", if 1-2"];
                    {0},
                    (* 1-1 *)
                    If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, 1, 1]] > EuclideanDistance[Part[secondPair, 1, 1], min],
                        Message[threadStep::strcase, "1-2", index, "The first tuple" <> ToString[Part[{firstPair, secondPair}, All, 1]] <> " is further than each other than one of them to the boudary " <> ToString[{min, max}] <> ", if 1-1"]
                    ];
                    If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, -1, 1]] < EuclideanDistance[Part[secondPair, -1, 1], max],
                        (* 1-2 *)
                        {1, 1},
                        (* 2 starts *)
                        {1, 0}
                    ]
                ],
                If[Part[firstPair, 1, 1] > Part[secondPair, -1, 1] &&
                    (Part[secondPair, -1, 1] - Part[secondPair, 1, 1]) / (Part[firstPair, 1, 1] - Part[secondPair, 1, 1]) < 0.4,
                    Message[threadStep::strcase, "1-2", index, "The first tuple" <> ToString[Part[{firstPair, secondPair}, All, 1, 1]] <> " is further than the second point " <> Part[secondPair, -1, 1] <> ", if 1-2"];
                    {0},
                    (* 1-2 *)
                    If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, -1, 1]] > EuclideanDistance[Part[secondPair, -1, 1], max],
                        Message[threadStep::strcase, "1-2", index, "The last tuple" <> ToString[Part[{firstPair, secondPair}, All, -1]] <> " is further than each other than one of them to the boudary " <> ToString[{min, max}] <> ", if 1-2"]
                    ];
                    If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, 1, 1]] < EuclideanDistance[Part[secondPair, 1, 1], min],
                        (* 1-1 *)
                        {1, 1},
                        (* 1 starts *)
                        {0, 1}
                    ]
                ]
            ]

        ),
        {1, 1} :> (
            If[EuclideanDistance[Part[firstPair, 1, 1], Part[secondPair, 1, 1]] < (max - min) / 3,
                (* 1-1 *)
                {1},
                (* 1 ends, 1 starts*)
                {0}
            ]
        ),
        {len1_, len2_} :> (Message[threadStep::strcase, ToString[len1] <> "-" <> ToString[len2], index, "wrong lengths or case not handled"])
    }]
]


(*
    Rare cases where there is not enough tuples in the list,
    not insteresting
*)
ThreadZeros[{}, (*domain*)_] := {{}, {}}
ThreadZeros[{zeroTuple_}, (*domain*)_] := Transpose[{Part[zeroTuple, All, 1]}]
(* Use Thread to divide tuples into different branches *)
ThreadZeros[zeroTuples:{_, __}, domain_] := With[
    {
        steps = MapThread[threadStep[#1, #2, #3, domain]&, {
            Prepend[zeroTuples // Most, {}],
            zeroTuples,
            Range[0, Length[zeroTuples] - 1]
        }]
    },

    Fold[threadLine, <|"beads" -> zeroTuples, "terminated" -> {}, "active" -> {}, "position" -> 0|>, steps]
    // Flatten[#active ~Join~ #terminated]&
	// Map[Table[Missing[], #start] ~Join~ #beads ~Join~ Table[Missing[], Length[zeroTuples] - #start - Length[#beads]]&]
]


End[]


EndPackage[]