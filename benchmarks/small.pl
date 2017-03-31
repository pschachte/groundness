% CVS: $Id: small.pl,v 1.2 1998/10/19 04:22:30 pets Exp $
update_circuit([function(K,Tk,Fk,CIk,IPk,ISk,Pk,Sk)|GsIn],
		Gi, Gj, V, Gs,
		[function(K,Tko,Fko,CIko,IPko,ISko,Pko,Sko)|GsOut]) :-
	Gi = function(I,_,Fi,_,IPi,ISi,Pi,_),
	Gj = function(J,_,Fj,_,_,_,_,Sj),
	set_union([I], Pi, PiI),
	set_union([J], Sj, SjJ),
	(K = J ->
		set_union(Tk, Fi, Tk2);
		Tk2 = Tk),
	(K = I ->
		set_union(Tk2, Fj, Tk3);
		Tk3 = Tk2),
	((set_member(K, IPi); set_member(K, ISi)) ->
		set_union(Tk3, [V], Tko);
		Tko = Tk3),
	(K = I ->
		set_union(Fk, [V], Fko);
		Fko = Fk),
	((set_member(K, Pi); K = I) ->
		set_difference(CIk, SjJ, CIk2);
		CIk2 = CIk),
	((set_member(I, CIk), set_member(V, Fk)) ->
		set_difference(CIk2, [I], CIk3);
		CIk3 = CIk2),
	(K = I ->
		exclude_if_vector_in_false_set(CIk3, Gs, V, CIk4);
		CIk4 = CIk3),
	(K = J ->
		set_difference(CIk4, [I], CIko);
		CIko = CIk4),
	(K = J ->
		set_union(IPk, [I], IPko);
		IPko = IPk),
	(K = I ->
		set_union(ISk, [J], ISko);
		ISko = ISk),
	(set_member(K, SjJ) ->
		set_union(Pk, PiI, Pko);
		Pko = Pk),
	(set_member(K, PiI) ->
		set_union(Sk, SjJ, Sko);
		Sko = Sk),
	update_circuit(GsIn, Gi, Gj, V, Gs, GsOut).
