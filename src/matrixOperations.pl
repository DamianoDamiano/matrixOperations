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
    
%determinant(+Matrix,-Determinant)
determinant([[A,B],[C,D]],V):-!,
    V is A*D-B*C.

determinant([[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]],Det):-
    determinant([[A22,A23],[A32,A33]],D1),
    determinant([[A21,A23],[A31,A33]],D2),
    determinant([[A21,A22],[A31,A32]],D3),
    Det is D1*A11 - D2*A12 + D3*A13.

%determinantFd(+Matrix,-Determinant)
determinantFd([[A,B],[C,D]],V):-!,
    V #= A*D-B*C.

determinantFd([[A11,A12,A13],[A21,A22,A23],[A31,A32,A33]],Det):-
    determinantFd([[A22,A23],[A32,A33]],D1),
    determinantFd([[A21,A23],[A31,A33]],D2),
    determinantFd([[A21,A22],[A31,A32]],D3),
    Det #= D1*A11 - D2*A12 + D3*A13.

%size(+Matrix,-Rows,-Column)
size([H|T],Rows,Col):-
    length(H,Col),
    length([H|T],Rows).

%identity(-Matrix,+Size)
identity(M,S):-
    identityA(M,S,1).

identityA([],Size,Index):- Index>Size,!.
identityA([H|T],Size,Index):-
    identityB(H,Size,Index,1),
    Index1 is Index+1,
    identityA(T,Size,Index1).

identityB([],Size,_,Current):-Current>Size,!.
identityB([H|T],Size,Index,Index):- !,
    H is 1,
    Current1 is Index+1,
    identityB(T,Size,Index,Current1).
identityB([H|T],Size,Index,Current):-
    H is 0,
    Current1 is Current+1,
    identityB(T,Size,Index,Current1).

%nthRow(+Matrix,+Index,-Row).
nthRow(Matrix,Index,Row):-
    nthRow1(Matrix,Index,Row,1).

nthRow1([Row|_],Index,Row,Index):-!.
nthRow1([_|T],Index,Row,Current):-
    Current1 is Current+1,
    nthRow1(T,Index,Row,Current1).

%nthCol(+Matrix,+Index,-Row).
nthColumn(Matrix,Index,Col):-
    transpose(Matrix,Transposed),
    nthRow(Transposed,Index,Col).

%matrixDiffFd(+Matrix1,+Matrix2,-MatrixResult)
matrixDiffFd([],[],[]).
    matrixDiffFd([H1|T1],[H2|T2],[HO|TO]):-
    diffFdIn(H1,H2,HO),
matrixDiffFd(T1,T2,TO).
diffFdIn([],[],[]).
diffFdIn([H1|T1],[H2|T2],[HO|TO]):-
    HO #= H1-H2,
    diffFdIn(T1,T2,TO).

%scalarMultiplication(+Matrix,+Scalar,-MatrixResult).
scalarMultiplication([],_,[]).
scalarMultiplication([H|T],V,[HR|TR]):-
    scalarMultiplication1(H,V,HR),
    scalarMultiplication(T,V,TR).

scalarMultiplication1([],_,[]).
scalarMultiplication1([H|T],V,[HR|TR]):-
    HR is H*V,
    scalarMultiplication1(T,V,TR).

%scalarMultiplicationFd(+Matrix,+Scalar,-MatrixResult).
scalarMultiplicationFd([],_,[]).
scalarMultiplicationFd([H|T],V,[HR|TR]):-
    scalarMultiplicationFd1(H,V,HR),
    scalarMultiplicationFd(T,V,TR).

scalarMultiplicationFd1([],_,[]).
scalarMultiplicationFd1([H|T],V,[HR|TR]):-
    HR #= H*V,
    scalarMultiplicationFd1(T,V,TR).

%switchRow(+Matrix,+IndexRow1,+IndexRow2,-MatrixResult)
switchRow(Matrix,IndexRow1,IndexRow2,Result):-
    nthRow(Matrix,IndexRow1,Row1),
    nthRow(Matrix,IndexRow2,Row2),
    switchRow1(Matrix,Row1,Row2,IndexRow1,IndexRow2,1,Result).

switchRow1([],_,_,_,_,_,[]).
switchRow1([_|T],Row1,Row2,IndexRow1,IndexRow2,IndexRow1,[HR|TR]):-!,
    HR = Row2,
    Index is IndexRow1+1,
    switchRow1(T,Row1,Row2,IndexRow1,IndexRow2,Index,TR).
switchRow1([_|T],Row1,Row2,IndexRow1,IndexRow2,IndexRow2,[HR|TR]):-!,
    HR = Row1,
    Index is IndexRow2+1,
    switchRow1(T,Row1,Row2,IndexRow1,IndexRow2,Index,TR).
switchRow1([H|T],Row1,Row2,IndexRow1,IndexRow2,Index,[HR|TR]):-!,
    HR = H,
    Index1 is Index+1,
    switchRow1(T,Row1,Row2,IndexRow1,IndexRow2,Index1,TR).

%switchCol(+Matrix,+IndexCol1,+IndexCol2,-MatrixResult)
switchCol(Matrix,IndexCol1,IndexCol2,Result):-
    transpose(Matrix,Transposed),
    switchRow(Transposed,IndexCol1,IndexCol2,Result1),
    transpose(Result1,Result).

%eigenvalues(+Matrix,-Eigenvalue).
%to show all the eigenvalues click on more
eigenvalues(Matrix,Eigenvalue):-
    Eigenvalue::0..10,
    size(Matrix,Size,Size),
    identity(Identity,Size),
    scalarMultiplicationFd(Identity,Eigenvalue,Lambda),
    matrixDiffFd(Matrix,Lambda,MatrixDiff),
    determinantFd(MatrixDiff,Det),
    Det #= 0,
    flatten(Eigenvalue,E),
    labeling(E).
    %if you want to display all the eigenvalues
    %without clicking on "more solutions" replace . with , next to
    %labeling(E) and add the following lines:
    %write(E),nl,
    %fail.
    %eigenvalues(_,_):-nl.
