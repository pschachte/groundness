%  file    : builtins
%  Authors : Peter Schachte
%  Purpose : Groundness analysis of Prolog buiiltins
%
%				   Abstract
%
%  This file lists Prolog's builtin predicates.

:- module(builtins, [
	builtin/2
   ]).


%  builtin(-Name, -Arity)
%  The predicate Name/Arity is a Prolog builtin.  Note that if new
%  predicaes are added here, they should also be added to
%  analysis:builtin_analysis/2.

builtin(!, 0).
builtin(true, 0).
builtin(trace, 0).
builtin(debug, 0).
builtin(notrace, 0).
builtin(nodebug, 0).
builtin(debuggging, 0).
builtin(spy, 1).
builtin(nospy, 1).
builtin(nospyall, 0).
builtin(otherwise, 0).
builtin(fail, 0).
builtin(false, 0).
builtin(abort, 0).
builtin(raise_exception, 1).
builtin(halt, 0).
builtin(halt, 1).
builtin(call, 1).
builtin(bagof, 3).
builtin(setof, 3).
builtin(findall, 3).
builtin(unix, 1).
builtin(repeat, 0).
builtin(compare, 3).
builtin((@<), 2).
builtin((@=<), 2).
builtin((==), 2).
builtin((@>=), 2).
builtin((@>), 2).
builtin(\=, 2).					% not really a builtin
builtin(~=, 2).					% not really a builtin
builtin((\==), 2).
builtin((<), 2).
builtin((=<), 2).
builtin((=:=), 2).
builtin((>=), 2).
builtin((>), 2).
builtin((=\=), 2).
builtin((is), 2).
builtin((=..), 2).
builtin(atom, 1).
builtin(atomic, 1).
builtin(callable, 1).
builtin(compound, 1).
builtin(db_reference, 1).
builtin(float, 1).
builtin(ground, 1).
builtin(integer, 1).
builtin(nonvar, 1).
builtin(number, 1).
builtin(simple, 1).
builtin(var, 1).
builtin(functor, 3).
builtin(arg, 3).
builtin(name, 2).
builtin(atom_chars, 2).
builtin(number_chars, 2).
builtin(numbervars, 3).
builtin(hash_term, 2).
builtin(subsumes_chk, 2).
builtin(copy_term, 2).
builtin(length, 2).
builtin(sort, 2).
builtin(keysort, 2).
builtin('C', 3).
builtin(statistics, 0).
builtin(statistics, 2).
builtin(see, 1).
builtin(seeing, 1).
builtin(seen, 0).
builtin(tell, 1).
builtin(telling, 1).
builtin(told, 0).
builtin(open, 3).
builtin(open, 4).
builtin(close, 1).
builtin(current_input, 1).
builtin(current_output, 1).
builtin(set_input, 1).
builtin(set_output, 1).
builtin(current_stream, 3).
builtin(stream_code, 2).
builtin(absolute_file_name, 2).
builtin(absolute_file_name, 3).
builtin(current_op, 3).
builtin(op, 3).
builtin(prompt, 3).
builtin(prompt, 2).
builtin(read, 1).
builtin(read, 2).
builtin(write, 1).
builtin(write, 2).
builtin(write_canonical, 1).
builtin(write_canonical, 2).
builtin(print, 1).
builtin(print, 2).
builtin(writeq, 1).
builtin(writeq, 2).
builtin(display,1).
builtin(display,2).
builtin(format, 2).
builtin(format, 3).
builtin(get, 1).
builtin(get, 2).
builtin(get0, 1).
builtin(get0, 2).
builtin(nl, 0).
builtin(nl, 1).
builtin(peek_char, 1).
builtin(peek_char, 2).
builtin(put, 1).
builtin(put, 2).
builtin(skip, 1).
builtin(skip, 2).
builtin(skip_line, 0).
builtin(skip_line, 1).
builtin(tab, 1).
builtin(tab, 2).
builtin(ttyget, 1).
builtin(ttyget0, 1).
builtin(ttynl, 0).
builtin(ttyput, 1).
builtin(ttyskip, 1).
builtin(ttytab, 1).
builtin(ttyflush, 0).
builtin(abolish, 1).
builtin(abolish, 2).
builtin(assert, 1).
builtin(assert, 2).
builtin(asserta, 1).
builtin(asserta, 2).
builtin(assertz, 1).
builtin(assertz, 2).
builtin(clause, 2).
builtin(clause, 3).
builtin(current_key, 2).
builtin(erase, 1).
builtin(instance, 2).
builtin(recorda, 3).
builtin(recorded, 3).
builtin(recordz, 3).
builtin(retract, 1).
builtin(retractall, 1).
builtin(expand_term, 2).

% Nonstandard (or non-Quintus) builtins added for specific tests
builtin(inc,2).
builtin(dec,2).
builtin(real, 1).
builtin(not,1).
