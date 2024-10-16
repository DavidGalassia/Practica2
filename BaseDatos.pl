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

    % Calcular el porcentaje total de herencia
    length(LevelOneList, LevelOneAmount),
    length(LevelTwoList, LevelTwoAmount),
    length(LevelThreeList, LevelThreeAmount),
    LevelsTotal is LevelOneAmount * 30 + LevelTwoAmount * 20 + LevelThreeAmount * 10,

    % Ajustar los porcentajes si superan el 100%
    (LevelsTotal =< 100
    ->  write('El monto total no excede 100%'), nl,
        distributeByLevel(Total, LevelOneList, LevelTwoList, LevelThreeList, Distribution)

    ;   write('Los montos exceden el 100%'), nl,
        AdjustmentFactor is 100 / LevelsTotal,
        distributeWithAdjustment(Total, AdjustmentFactor, LevelOneList, LevelTwoList, LevelThreeList, Distribution)

    ),

    % Imprimir la distribución final
    printDistribution(Distribution).

% Distribuir montos basados en niveles de consanguinidad sin ajuste
distributeByLevel(Total, LevelOneList, LevelTwoList, LevelThreeList, Distribution) :-
    % Distribuir montos fijos para el primer nivel de consanguinidad
    distributeByFixedAmounts(Total, LevelOneList, 1, FirstDistribution),
    
    % Distribuir montos fijos para el segundo nivel de consanguinidad
    distributeByFixedAmounts(Total, LevelTwoList, 2, SecondDistribution),
    
    % Distribuir montos fijos para el tercer nivel de consanguinidad
    distributeByFixedAmounts(Total, LevelThreeList, 3, ThirdDistribution),

    % Combinar las distribuciones y sumar montos si hay duplicados
    append(FirstDistribution, SecondDistribution, CombinedDistribution), % Combina la primera y segunda distribución en CombinedDistribution
    append(CombinedDistribution, ThirdDistribution, RawDistribution),  % Termina de combinar todas las listas en una sola distribución cruda
    sumAmounts(RawDistribution, Distribution). % Suma los montos para manejar duplicados y generar la distribución final

% Distribuir montos ajustados si la suma de los porcentajes excede el 100%
distributeWithAdjustment(Total, AdjustmentFactor, LevelOneList, LevelTwoList, LevelThreeList, Distribution) :-
    AdjustedTotal is Total * AdjustmentFactor, % Ajustar el total según el factor de ajuste
    distributeByLevel(AdjustedTotal, LevelOneList, LevelTwoList, LevelThreeList, Distribution). % Llamar a distributeByLevel con el total ajustado

% Asignar montos fijos basados en el nivel de consanguinidad
distributeByFixedAmounts(Total, List, Level, Distribution) :-
    % Para cada persona en la lista, calcular el monto correspondiente según su nivel
    findall(Person-Amount, (member(Person, List), amountByLevel(Total, Level, Amount)), Distribution).

% Calcular el monto que corresponde según el nivel de consanguinidad
amountByLevel(Total, 1, Amount) :- Amount is Total * 0.30. % 30% para el primer nivel
amountByLevel(Total, 2, Amount) :- Amount is Total * 0.20. % 20% para el segundo nivel
amountByLevel(Total, 3, Amount) :- Amount is Total * 0.10. % 10% para el tercer nivel

% Sumar montos si hay duplicados
sumAmounts(List, Distribution) :-
    % Agrupar los montos por nombre y sumar los montos duplicados
    findall(Name-TotalAmount, (setof(Amount, member(Name-Amount, List), Amounts), sum_list(Amounts, TotalAmount)), Distribution).

% Imprimir la distribución final
printDistribution([]). % Caso base: no hay más personas para imprimir
printDistribution([Person-Amount | Rest]) :- % Para cada persona y su monto
    format('~w recibe ~2f~n', [Person, Amount]), % Imprimir el nombre y el monto con formato
    printDistribution(Rest). % Continuar con el resto de la lista
