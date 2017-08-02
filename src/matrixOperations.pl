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
