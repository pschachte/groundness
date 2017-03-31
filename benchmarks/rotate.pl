% CVS: $Id: rotate.pl,v 1.3 1998/10/21 04:26:12 pets Exp $

goal :- ground(Ground), rotate(Ground, _).

rotate(Xs,Ys) :-                
 append(As,Bs,Xs),          
 append(Bs,As,Ys).          
                                
append(Xs,Ys,Zs) :-             
  Xs = [],                  
  Ys = Zs.                   
append(Xs,Ys,Zs) :-             
  Xs = [X|Xs1],              
  Zs = [X|Zs1],              
  append(Xs1,Ys,Zs1).
