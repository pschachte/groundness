# groundness
An efficient groundness analyser for Prolog

Copyright 1998-2017 Peter Schachte

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
You may not use this code except in compliance with the License.

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
