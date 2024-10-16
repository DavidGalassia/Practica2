male(miguel).
male(norberto).
male(david).
male(johan).

female(juana_d_arc).
female(maria).
female(juliana).
female(laura).
female(sara).

father(david, juliana).
father(miguel, sara).
father(david, norberto).
father(norberto, laura).
father(norberto, johan).

mother(maria, juliana).
mother(juliana, sara).
mother(maria, norberto).
mother(juana_d_arc, laura).
mother(juana_d_arc, johan).

% Relaciones familiares
parent(X, Y) :- father(X, Y); mother(X, Y).
grandfather(X, Y) :- father(X, Z), parent(Z, Y), male(X).
grandmother(X, Y) :- mother(X, Z), parent(Z, Y), female(X).
grandparent(X, Y) :- grandfather(X, Y); grandmother(X, Y).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
uncle(X, Y) :- sibling(X, Z), parent(Z, Y), male(X).
aunt(X, Y) :- sibling(X, Z), parent(Z, Y), female(X).
cousin(X, Y) :- parent(Z, X), parent(W, Y), sibling(Z, W).

% Niveles de consanguinidad
levelConsanguinity(X, Y, 1) :- parent(X, Y).
levelConsanguinity(X, Y, 2) :- sibling(X, Y).
levelConsanguinity(X, Y, 2) :- grandparent(X, Y).
levelConsanguinity(X, Y, 3) :- uncle(X, Y).
levelConsanguinity(X, Y, 3) :- aunt(X, Y).
levelConsanguinity(X, Y, 3) :- cousin(X, Y).

% Distribución de la herencia
distributeInheritance(Total, Distribution) :-
    % Obtener las listas de personas por nivel de consanguinidad
    findall(Person, levelConsanguinity(Person, _, 1), LevelOneList),
    findall(Person, levelConsanguinity(Person, _, 2), LevelTwoList),
    findall(Person, levelConsanguinity(Person, _, 3), LevelThreeList),

    % Calcular el porcentaje total de herencia directamente
    LevelsTotal is (length(LevelOneList) * 30) + (length(LevelTwoList) * 20) + (length(LevelThreeList) * 10),

    % Ajustar los porcentajes si superan el 100%
    (LevelsTotal =< 100
    ->  distributeByLevel(Total, LevelOneList, LevelTwoList, LevelThreeList, Distribution)
    ;   AdjustmentFactor is 100 / LevelsTotal,
        AdjustedTotal is Total * AdjustmentFactor,
        distributeByLevel(AdjustedTotal, LevelOneList, LevelTwoList, LevelThreeList, Distribution)
    ),

    % Imprimir la distribución final
    printDistribution(Distribution).

% Distribuir montos basados en niveles de consanguinidad
distributeByLevel(Total, LevelOneList, LevelTwoList, LevelThreeList, Distribution) :-
    % Combinar las distribuciones y sumar montos si hay duplicados
    findall(Person-Amount, 
        (member(Person, LevelOneList), amountByLevel(Total, 1, Amount); 
         member(Person, LevelTwoList), amountByLevel(Total, 2, Amount); 
         member(Person, LevelThreeList), amountByLevel(Total, 3, Amount)),
        RawDistribution),
    sumAmounts(RawDistribution, Distribution).

% Calcular el monto que corresponde según el nivel de consanguinidad
amountByLevel(Total, 1, Amount) :- Amount is Total * 0.30. % 30% para el primer nivel
amountByLevel(Total, 2, Amount) :- Amount is Total * 0.20. % 20% para el segundo nivel
amountByLevel(Total, 3, Amount) :- Amount is Total * 0.10. % 10% para el tercer nivel

% Sumar montos si hay duplicados
sumAmounts(List, Distribution) :-
    findall(Name-TotalAmount, 
            (setof(Amount, member(Name-Amount, List), Amounts), 
            sum_list(Amounts, TotalAmount)), 
            Distribution).

% Imprimir la distribución final
printDistribution([]). % Caso base: no hay más personas para imprimir
printDistribution([Person-Amount | Rest]) :- % Para cada persona y su monto
    format('~w recibe ~2f~n', [Person, Amount]), % Imprimir el nombre y el monto con formato
    printDistribution(Rest). % Continuar con el resto de la lista
