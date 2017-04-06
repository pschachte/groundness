# groundness &mdash; an efficient groundness analyser for Prolog

Copyright 1998-2017 Peter Schachte

Licensed under the
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)
(the "License"); you may not use this code except in compliance with
the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

This is the groundness analyser described in the paper:

Tania Armstrong, Kim Marriott, Peter Schachte, and Harald Søndergaard. [Two classes of Boolean functions for dependency analysis.](http://people.eng.unimelb.edu.au/schachte/papers/scp98.pdf) Science of Computer Programming, 31(1):3–45, May 1998.

It is based on the abstract domain Pos documented in the paper:

K. Marriott, H. Sondergaard, [Precise and efficient groundness analysis for logic programs](http://people.eng.unimelb.edu.au/harald/papers/loplas93.pdf), ACM Letters on Programming Languages and Systems 2 (14) (1993) 181-196.

This is a two-phase [abstract
interpreter](https://en.wikipedia.org/wiki/Abstract_interpretation),
meaning it first analyses the
[groundness](https://en.wikipedia.org/wiki/Ground_expression) relationships
among the arguments of each predicate bottom-up over the program call graph
(analysing one [strongly connected
component](https://en.wikipedia.org/wiki/Strongly_connected_component) at a
time), and then in the second, top-down, phase, uses the results computed
in the first phase to determine the groundness of arguments to each
predicate.  This approach is sound, but loses precision unless the analysis
domain is _condensing_, which the Pos domain is.

## Building

You must have [SWI Prolog](http://www.swi-prolog.org/) version 7 installed.

This code uses my [Reduced Ordered Binary Decision
Diagram](https://en.wikipedia.org/wiki/Binary_decision_diagram)
implementation written in C. This must be compiled before using the
analyzer. To compile, simply run `make`. This will build the shared library
needed to access the ROBDD code. Note that this assumes that SWI Prolog is
installed in your search path with the name `swipl`. This Makefile has only
been tested on Mac OS X version 10.12. I welcome pull requests with
adaptations to other OSes, and ports to other Prologs.

## Running

To run the analyzer, load `analyzer.pl` and execute the query

`anz([`_filename_`]).`

This will analyze the file _filename_ and all the files it loads and
print out the goal-independent (bottom-up relational) analysis of
each predicate, called `(g.i.)`, then the least upper bound of all
calls of the predicate, called `(call)`, and the answer groundness,
called `(answ)`.  The latter is the worst case groundness on completion of
all calls to the program, which is the lattice meet of the first two
analyses.

Analysis results are shown in disjunctive normal form, as either
`TRUE` or `FALSE` or as one or more parenthesized conjunctions of argument
numbers, separated by spaces, and the lists are disjoined.  So for example,
the goal independent analysis result for append is shown as

```
(1 2 3) (1 ~2 ~3) (~1 ~3)
```

This means that when `append/2` succeeds, either all three arguments are
ground, or the first argument is ground and the others are not
(necessarily) ground, or the first and third are not (necessarily) ground,
whether the second is ground or not.  This is equivalent to the more
succinct statement (1 &and; 2) &harr; 3, which you can verify with
a truth table.

## Known bugs

The top-down analysis does not give the strongest possible analysis.
