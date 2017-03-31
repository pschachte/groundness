% CVS: $Id: ime_v2-2-1.pl,v 1.4 1998/10/21 04:26:00 pets Exp $
goal :- ground(M), linear_multi_equation(M, R).

/*
This code resolves lenear multipe equations in Prolog. Using Gauss method,
It finds the maximum (abs) element and change the corresponding row and
column to first row and column in the working matrix.
Any comment welcome.
To run it, use goal linear_multi_equation(Matrix,Result).
An example:

go:-
     Matrix= [[    5, 1, 2,-3, 1     ],   % 5*X1 +   X2 + 2*X3 - 3*X4 = 1
              [    0, 0,-9, 7, 1     ],   %             - 9*X3 + 7*X4 = 1
              [    4, 9, 6,-8, 8     ],   % 4*X1 + 9*X2 + 6*X3 - 8*X4 = 8
              [    5, 8, 1,-4, 8     ]],  % 5*X1 + 8*X2 +   X3 - 4*X4 = 8
     write(Matrix),nl,
     linear_multi_equation(Matrix,Result),
     write('result = '),write(Result),nl.

The output is
[[5, 1, 2, -3, 1], [0, 0, -9, 7, 1], [4, 9, 6, -8, 8], [5, 8, 1, -4, 8]]
result = [1, 2, 3, 4]
*/
/*-------------------------------------------------------------------------*/
linear_multi_equation(Matrix,Result):-              /*     IME v2.2.1      */
   getorder(Matrix,Index0),                         /*      made by        */
   solut(Matrix,[],TriMatrix,Result1,Index0,Index), /*zhuhail@vax.sbu.ac.uk*/
   backsubstitute(TriMatrix,[Result1],Result0),     /* any comment welcome */
   make_in_order(Result0,Index,Result).

solut([[H|[T|[]]]],TriMatrix,TriMatrix,Result1,Index,Index):- !,
        Result1 is T/H.
solut(Matrix,Tem,TriMatrix,Result1,Index0,Index):-
        changematrix(Matrix,ChangedMatrix,
                     Tem,ChangedTem,Index0,Index1),
        ChangedMatrix =[MainRow|SubMatrix],
        solut1(MainRow,SubMatrix,SubMatrix1),
        drop(SubMatrix1,ReduceMatrix),
        solut(ReduceMatrix,[MainRow|ChangedTem],TriMatrix,Result1,Index1,Index).

solut1(MainRow,[],[]):- !.
solut1(MainRow,[Row|RT],[NewRow|NT]):-
        MainRow=[MainElement|_],
        Row=[LeftElement|_],
        Ratio is LeftElement/MainElement,
        solut2(MainElement,Ratio,MainRow,Row,NewRow),
        solut1(MainRow,RT,NT).

solut2(MainElement,Ratio,[],[],[]):- !.
solut2(MainElement,Ratio,[TopElement|MT],[Element|T],[NewElement|NT]):-
        NewElement is Element-Ratio*TopElement,
        solut2(MainElement,Ratio,MT,T,NT).

changematrix(Matrix,ChangedMatrix,Tem,ChangedTem,Index0,Index):-
        maximum(Matrix,1,0,1,1,MaxRowNum,MaxColNum),
        change_row(Matrix,MaxRowNum,MaxColNum,ChangedRowMatrix),
        change_col(ChangedRowMatrix,MaxColNum,ChangedMatrix),
        change_tri(Tem,MaxColNum,ChangedTem,Index0,Index).

maximum([],_,_,MaxRowNum,MaxColNum,MaxRowNum,MaxColNum):- !.
maximum([Row|MatrixT],RowNum,Max,TemRowNum,TemColNum,MaxRowNum,MaxColNum):-
   maximum1(Row,RowNum,1,Max,NewM,TemRowNum,TemColNum,MaxRowNum1,MaxColNum1),
   NextRowNum is RowNum+1,
   maximum(MatrixT,NextRowNum,NewM,MaxRowNum1,MaxColNum1,MaxRowNum,MaxColNum).

abs(A,Abs):- A < 0, !, Abs is -A.
abs(A,A).

maximum1([_],_,_,Max,Max,MaxRowNum,MaxColNum,MaxRowNum,MaxColNum):- !.
maximum1([H|T],RowNum,ColNum,Max,NewM,TemRowNum,TemColNum,MaxRowNum,MaxColNum):-
   abs(H,AbsH), Max < AbsH, !,
        Max1 is AbsH, TemRowNum1 is RowNum,
        TemColNum1 is ColNum, NextC is ColNum+1,
   maximum1(T,RowNum,NextC,Max1,NewM,TemRowNum1,TemColNum1,MaxRowNum,MaxColNum).
maximum1([H|T],RowNum,ColNum,Max,NewM,TemRowNum,TemColNum,MaxRowNum,MaxColNum):-
        NextC is ColNum+1,
   maximum1(T,RowNum,NextC,Max,NewM,TemRowNum,TemColNum,MaxRowNum,MaxColNum).

change_row(Matrix,MaxRowNum,MaxColNum,ChangedMatrix):-
        MaxRowNum = 1, !, ChangedMatrix = Matrix.
change_row(Matrix,MaxRowNum,MaxColNum,ChangedMatrix):-
        change_row1(Matrix,1,MaxRowNum,NewMatrix,FirstRow,TemR,MainRow),
        NewMatrix=[H|T],
        ChangedMatrix = [MainRow|T].

change_col(Matrix,MaxColNum,ChangedMatrix):-
        MaxColNum = 1, !, ChangedMatrix = Matrix.
change_col(Matrix,MaxColNum,ChangedMatrix):-
        change_col1(Matrix,MaxColNum,ChangedMatrix).

change_row1([],_,_,[],_,MainRow,MainRow):- !.
change_row1([Row|MatrixT],RowNum,MaxRowNum,[NewRow|NT],FirstRow,TemR,MainRow):-
   RowNum = 1, !, NextRowNum is RowNum+1, FirstRow = Row,
        change_row1(MatrixT,NextRowNum,MaxRowNum,NT,FirstRow,TemR,MainRow).
change_row1([Row|MatrixT],RowNum,MaxRowNum,[NewRow|NT],FirstRow,TemR,MainRow):-
   RowNum = MaxRowNum, !, NewRow = FirstRow, TemR = Row, NextRowNum is RowNum+1,
        change_row1(MatrixT,NextRowNum,MaxRowNum,NT,FirstRow,TemR,MainRow).
change_row1([Row|MatrixT],RowNum,MaxRowNum,[NewRow|NT],FirstRow,TemR,MainRow):-
        NewRow = Row, NextRowNum is RowNum+1,
        change_row1(MatrixT,NextRowNum,MaxRowNum,NT,FirstRow,TemR,MainRow).

change_col1([],_,[]):- !.
change_col1([Row|MatrixT],MaxColNum,[NewRow|NT]):-
        change_col2(Row,1,MaxColNum,Row1,First,TemMain,Main),
        Row1 = [H|T],
        NewRow = [Main|T],
        change_col1(MatrixT,MaxColNum,NT).

change_col2([Last],_,_,[Last],_,Main,Main):- !.
change_col2([H|T],ColNum,MaxColNum,[NH|NT],First,TemMain,Main):-
    ColNum = 1, !, NextColNum is ColNum+1, First = H,
        change_col2(T,NextColNum,MaxColNum,NT,First,TemMain,Main).
change_col2([H|T],ColNum,MaxColNum,[NH|NT],First,TemMain,Main):-
    ColNum = MaxColNum, !, NH = First, TemMain = H, NextColNum is ColNum+1,
        change_col2(T,NextColNum,MaxColNum,NT,First,TemMain,Main).
change_col2([H|T],ColNum,MaxColNum,[NH|NT],First,TemMain,Main):-
        NH = H, NextColNum is ColNum+1,
        change_col2(T,NextColNum,MaxColNum,NT,First,TemMain,Main).

change_tri(Tem,MaxColNum,ChangedTem,Index0,Index):-
    MaxColNum = 1, !, ChangedTem = Tem, Index = Index0.
change_tri(Tem,MaxColNum,ChangedTem,Index0,Index):-
        ChangeNum is MaxColNum+1,
        change_tri1(Tem,2,ChangeNum,ChangedTem,MainNum1,ChangeNum1),
        MainNum2 is MainNum1-1,
        ChangeNum2 is ChangeNum1-1,   /*for change vector*/
        change_tri2(Index0,1,MainNum2,ChangeNum2,_,_,Index).

change_tri1([],MainNum1,ChangeNum1,[],MainNum1,ChangeNum1):- !.
change_tri1([Row|MatrixT],MainNum,ChangeNum,[NewRow|NT],MainNum1,ChangeNum1):-
        change_tri2(Row,1,MainNum,ChangeNum,_,_,NewRow),
        NextMainNum is MainNum+1,
        NextChangeNum is ChangeNum+1,
        change_tri1(MatrixT,NextMainNum,NextChangeNum,NT,MainNum1,ChangeNum1).

change_tri2([],_,_,_,_,_,[]):- !.
change_tri2([H|T],Num,MainNum,ChangeNum,MainE,ChangeE,[NH|NT]):-
     Num = MainNum, !, NH = ChangeE, NextNum is Num+1, MainE = H,
        change_tri2(T,NextNum,MainNum,ChangeNum,MainE,ChangeE,NT).
change_tri2([H|T],Num,MainNum,ChangeNum,MainE,ChangeE,[NH|NT]):-
     Num = ChangeNum, !, NH = MainE, NextNum is Num+1, ChangeE = H,
        change_tri2(T,NextNum,MainNum,ChangeNum,MainE,ChangeE,NT).
change_tri2([H|T],Num,MainNum,ChangeNum,MainE,ChangeE,[NH|NT]):-
        NH = H, NextNum is Num+1,
        change_tri2(T,NextNum,MainNum,ChangeNum,MainE,ChangeE,NT).

drop([],[]):- !.
drop([[H|ReduceRow]|T],[ReduceRow|RT]):-
        drop(T,RT).

backsubstitute([],Result0,Result0):- !.
backsubstitute([[H|RowT]|MatrixT],Tem,Result0):-
        multiply(RowT,Tem,0,Sum,RightElement),
        Result is (RightElement-Sum)/H,
        backsubstitute(MatrixT,[Result|Tem],Result0).

multiply([H|[]],_,Tem,Tem,H):- !.
multiply([H|T],[RH|RT],Tem,Sum,RightElement):-
        NewSum = Tem+H*RH,
        multiply(T,RT,NewSum,Sum,RightElement).

make_in_order(Result0,Index,Result):-
        make_in_order1(Result0,Index,VectorWithNum),
        sort(VectorWithNum,SortVector),
        make_in_order2(SortVector,Result).

make_in_order1([],_,[]):- !.
make_in_order1([H|T],[IH|IT],[IH-H|RT]):-
        make_in_order1(T,IT,RT).

make_in_order2([],[]):- !.
make_in_order2([H-S|T],[S|RT]):-
        make_in_order2(T,RT).

getorder([Row|MatrixT],Index0):-
        get1(Row,Index0,1).

get1([],[],_):- !.
get1([H|T],[Num|OT],Num):-
        NextNum is Num+1,
        get1(T,OT,NextNum).
