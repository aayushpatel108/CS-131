tower(N, T, C) :- 
	C = counts(Top, Bottom, Left, Right),
	len_row(T, N),
	len_col(T, N),
	within_domain(T, N),
	maplist(fd_all_different, T),
	transpose(T, X),
	maplist(fd_all_different, X),
	reverse_2d(T, RT),
	reverse_2d(X, RX),
	valid_rows(Left, T),
	valid_rows(Right, RT),
	valid_rows(Top, X),
	valid_rows(Bottom, RX),
	maplist(fd_labeling, T).

% FROM TA HINT CODE
len_row(X, N) :-
    length(X, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

% FROM TA HINT CODE
within_domain([], _).
within_domain([HD | TL], N) :-
    % http://www.gprolog.org/manual/html_node/gprolog057.html fd_domain(Vars, Lower, Upper)
    fd_domain(HD, 1, N),
    within_domain(TL, N).

count_check(0, [], _).
count_check(Count, [H|T], Height) :-
	H #< Height, 
	count_check(Count, T, Height);
	H #> Height, 
	X #= Count-1,
	count_check(X, T, H).

valid_rows([], []).
valid_rows([FirstCount|RemainingCounts], [FirstRow|RemainingRows]) :-
	count_check(FirstCount, FirstRow, 0),
	valid_rows(RemainingCounts, RemainingRows).

% FROM TA HINT CODE
reverse_2d(X, RX) :-
    maplist(reverse, X, RX).

% This is SWI-prolog's old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
	lists_firsts_rests(Rest, Fs, Oss).

plain_tower(N, T, C) :- 
	C = counts(Top, Bottom, Left, Right),
	len_row(T, N),
	len_col(T, N),
	fill_2d(T, Left, N),
	reverse_2d(T, RT),
	fill_2d(RT, Right, N),
	transpose(T, X),
	fill_2d(X, Top, N),
	reverse_2d(X, RX),
	fill_2d(RX, Bottom, N),
	length(Left, N),
	length(Right, N),
	length(Top, N),
	length(Bottom, N).

% FROM TA HINT CODE
% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], _, _).
fill_2d([Head | Tail], [Count1Head|Count1Tail], N) :-
    plain_domain(N, Domain),
	permutation(Domain, Head),
	member(Count1Head, Domain),
	plain_count_check(Count1Head, Head, 0),
    fill_2d(Tail, Count1Tail, N).

plain_count_check(0, [], _).
plain_count_check(Count, [H|T], Height) :-
	H < Height, 
	plain_count_check(Count, T, Height);
	H > Height, 
	X is Count-1,
	plain_count_check(X, T, H).

% FROM TA HINT CODE
all_unique([]).
all_unique([H|T]) :- exists(H, T), !, fail.
all_unique([_|T]) :- all_unique(T).

% FROM TA HINT CODE
exists(X, [X|_]).
exists(X, [_|T]) :-
	exists(X, T).

% FROM TA HINT CODE
plain_domain(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

ambiguous(N, C, T1, T2) :- 
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.

speedup(Result) :-
	statistics(cpu_time, [PStart|_]),
	plain_tower(5, _,counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	statistics(cpu_time, [PStop|_]),
	statistics(cpu_time, [TStart|_]),
	tower(5, _,counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	tower(5, _,counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	tower(5, _,counts([2,3,2,1,4],[3,1,3,3,2],[4,1,2,5,2],[2,4,2,1,2])),
	statistics(cpu_time, [TStop|_]),
	Result is (PStart - PStop)/((TStart - TStop)/3).

test(T) :-
	plain_tower(4,T, counts([1,2,2,3],[3,2,2,1],[1,2,2,4],[4,2,2,1])).


