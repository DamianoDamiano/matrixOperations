:-lib(fd).

%print matrix
printMatrix([]):-nl.
printMatrix([HRows|TRows]):-
    write(HRows),nl,
    printMatrix(TRows).

%transpose(+Matrix,-Transposed)
transpose([],[]).
transpose([H|T],MT):-
    transpose(H,[H|T],MT).
transpose([],_,[]).
transpose([_|T],M,[HMT|TMT]):-
        transposeIn(M,HMT,M1),
        transpose(T,M1,TMT).
transposeIn([],[],[]).
transposeIn([[V1|V2]|T],[V1|TV],[V2|TM]):-
        transposeIn(T,TV,TM).

%matrixDiff(+Matrix1,+Matrix2,-MatrixResult)
%MatrixResult=Matrix1+Matrix2
matrixSum([],[],[]).
matrixSum([H1|T1],[H2|T2],[HO|TO]):-
    sumIn(H1,H2,HO),
    matrixSum(T1,T2,TO).
sumIn([],[],[]).
sumIn([H1|T1],[H2|T2],[HO|TO]):-
    HO is H1+H2,
    sumIn(T1,T2,TO).

%matrixDiff(+Matrix1,+Matrix2,-MatrixResult)
%MatrixResult=Matrix1-Matrix2
matrixDiff([],[],[]).
matrixDiff([H1|T1],[H2|T2],[HO|TO]):-
    diffIn(H1,H2,HO),
    matrixDiff(T1,T2,TO).
diffIn([],[],[]).
diffIn([H1|T1],[H2|T2],[HO|TO]):-
    HO is H1-H2,
    diffIn(T1,T2,TO).

%matrixProd(+Matrix1,+Matrix2,-MatrixResult)
%MatrixResult=Matrix1*Matrix2
matrixProd([],[],[]).
matrixProd(M1,M2,MO):-
    transpose(M2,MT),
    prodIn(M1,MT,MO).
prodIn([],_,[]).
prodIn([H1|T1],M2,[HO|TO]):-
    prodInIn(H1,M2,HO),
    prodIn(T1,M2,TO).
prodInIn(_,[],[]).
prodInIn(H1,[H2|T2],[HO|TO]):-
    prodInInIn(H1,H2,HO),
    prodInIn(H1,T2,TO).
prodInInIn([],[],0).
prodInInIn([H1|T1],[H2|T2],V):-
    V #= H1*H2 + V1,
    prodInInIn(T1,T2,V1).

%matrixPower(+Matrix1,+Exp,-MatrixResult)
%MatrixResult=Matrix1^Exp
matrixPower(A,N,R):-
    matrixPower(A,A,N,R).
matrixPower(AT,_,1,AT).
matrixPower(AT,A,N,R):-
    matrixProd(AT,A,A1),
    N1 is N-1,
    matrixPower(A1,A,N1,R).
    
%isSquare(+Matrix)
isSquare([H|T]):-
    length(H,V),
    length([H|T],V).

%trace(+Matrix,-Trace)
trace(M,V):-
    isSquare(M),
    traceA(M,V,1).

traceA([],0,_).
traceA([H|T],V,I):-
    traceInner(H,V1,I,1),
    V #= V1+V2,
    I1 is I+1,
    traceA(T,V2,I1).

traceInner([H|_],H,I,I):-!.
traceInner([_|T],V,I1,I2):-
    I is I2+1,
    traceInner(T,V,I1,I).

%trace(+Matrix,-Determinant)
determinant([[A,B],[C,D]],V):-
    V is A*D-B*C.

%diagonal(+Matrix,-Diagonal)
diagonal(M,D):-
    diagonalA(M,D,1).

diagonalA([],[],_).
diagonalA([H|T],[HD|TD],I):-
    diagonalInner(H,HD,I,1),
    I1 is I+1,
    diagonalA(T,TD,I1).

diagonalInner([H|_],H,I,I):-!.
diagonalInner([_|T],H,I,I1):-
    I2 is I1+1,
    diagonalInner(T,H,I,I2).

%antidiagonal(+Matrix,-Antidiagonal)
antidiagonal([H|T],A):-
    length(H,N),
    antidiagonalA([H|T],A,N).

antidiagonalA([],[],0).
antidiagonalA([H|T],[HA|TA],N):-
    antidiagonalInner(H,HA,N,1),
    N1 is N-1,
    antidiagonalA(T,TA,N1).

antidiagonalInner([H|_],H,I,I):-!.
antidiagonalInner([_|T],H,I,E):-
    E1 is E+1,
    antidiagonalInner(T,H,I,E1).
