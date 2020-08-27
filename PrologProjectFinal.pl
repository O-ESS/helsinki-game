grid_build(N,M):-
    length(M,N),
    grid_build1(M,N).

grid_build1([],_).
grid_build1([H|T],N):-
	length(H,N),
	grid_build1(T,N).

num_gen(X,X,[X]).
num_gen(X,Y,[X|L1]):-
	X < Y,
	X1 is X +1,
	num_gen(X1,Y,L1).

check_num_grid(G):-
    length(G,W),
	flatten(G,L),
	max_list(L,X),
	W >= X,
	N1 is X-1,
    check_member(N1,L). 
	
check_member(0,_).
check_member(X,L1):- 
    X>0,
    mem(X,L1),
    N1 is X-1,
    check_member(N1,L1).

mem(X,[X|T]).
mem(X,[H|T]):-
	X\==H,
	mem(X,T).

trans(M,M1):-
	transpose(M,M1).
transpose([[]|_],[]).
transpose(Matrix, [Row|Rows]):-
	transpose_1st_col(Matrix,Row,RestMatrix),
	transpose(RestMatrix,Rows).
	
transpose_1st_col([],[],[]).
transpose_1st_col([[H|T] | Rows], [H|Hs], [T|Ts]):-
	transpose_1st_col(Rows,Hs,Ts).

acceptable_distribution(G):-
	trans(G,Gt),
	not_same(G,Gt).

not_same([],[]).	
not_same([H|T],[H1|T1]):-
	H \= H1,
	not_same(T,T1).

distinct_rows([_|[]]).
distinct_rows([H|T]):-
	\+member(H,T),
	distinct_rows(T).
	
distinct_columns([]).
distinct_columns(M):-
	trans(M,M1),
	distinct_rows(M1).

same([],[]).
same([H|T],[X|T1]):-
	H==X,
	same(T,T1).
	
acceptable_permutation(L,L1):-
       permutation(L, L1),
	   check(L,L1).
	   
check([],[]).
check([H|T],[H1|T1]):-
        H\==H1,
        check(T,T1).

row_col_match(G):-
        trans(G,G1),
        acceptable_permutation(G,G1).
           

helsinki(N,G):-
     grid_gen(N,G).
 

grid_gen(N,M):-
        grid_build(N,M), 
		row_col_match(M),
		num_gen(1,N,L),
		fill_grid(L,M,N),
		distinct_rows(M),
		check_num_grid(M).

fill_grid(_,[],N).	
fill_grid(L,[H|T],N):- 
		helper(L,H,N),
		fill_grid(L,T,N).
		
helper(_,[],N).		
helper(L,[H|T],N):-
	between(1,N,H),
	helper(L,T,N).
