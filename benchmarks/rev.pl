% CVS: $Id: rev.pl,v 1.3 1998/10/20 03:23:48 pets Exp $
goal :-  go.
% *************************************
% CLP(R) Version 1.1 - Example Programs
% *************************************
%
% Naive reverse of 150 elements

app([], L, L).
app([H | L1], L2, [H | L]) :- app(L1, L2, L).	
rev([], []).
rev([H | L1], L) :- rev(L1, L2), app(L2, [H], L).

do_rev :-
    rev(_, _).

/*
do_rev :-
    rev([a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14], _).
*/

/*
do_rev :- rev([
a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, 
a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, 
a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, 
a43, a44, a45, a46, a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, 
a57, a58, a59, a60, a61, a62, a63, a64, a65, a66, a67, a68, a69, a70, 
a71, a72, a73, a74, a75, a76, a77, a78, a79, a80, a81, a82, a83, a84, 
a85, a86, a87, a88, a89, a90, a91, a92, a93, a94, a95, a96, a97, a98, 
a99, a100, a101, a102, a103, a104, a105, a106, a107, a108, a109, a101,
a111, a112, a113, a114, a115, a116, a117, a118, a119, a120, a121, a122,
a123, a124, a125, a126, a127, a128, a129, a130, a131, a132, a133, a134,
a135, a136, a137, a138, a139, a140, a141, a142, 
a143, a144, a145, a146, a147, a148, a149, a150], _).
*/

go :-
	%ztime,
        do_rev.
        %ctime(TIME), 
	%LIPS * TIME = 11475,
	%printf("Time = % (% lips)\n", [TIME, LIPS]).

%?- printf("\n>>> Sample goal: go/0\n", []).

