% Database of individuals
male(miguel).
male(norberto).
male(david).
male(johan).

female(juana_d_arc).
female(maria).
female(juliana).
female(laura).
female(sara).

% Family relationships
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

% Defining familial connections
parent(X, Y) :- father(X, Y); mother(X, Y).
grandfather(X, Y) :- father(X, Z), parent(Z, Y), male(X).
grandmother(X, Y) :- mother(X, Z), parent(Z, Y), female(X).
grandparent(X, Y) :- grandfather(X, Y); grandmother(X, Y).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
uncle(X, Y) :- sibling(X, Z), parent(Z, Y), male(X).
aunt(X, Y) :- sibling(X, Z), parent(Z, Y), female(X).
cousin(X, Y) :- parent(Z, X), parent(W, Y), sibling(Z, W).

% Levels of consanguinity
levelConsanguinity(X, Y, 1) :- parent(X, Y); parent(Y, X).
levelConsanguinity(X, Y, 2) :- sibling(X, Y); sibling(Y, X); grandparent(X, Y); grandparent(Y, X).
levelConsanguinity(X, Y, 3) :- uncle(X, Y); aunt(X, Y); cousin(X, Y); cousin(Y, X).

% Inheritance distribution logic
distributeInheritance(X, Total, Distribution) :-
    % Collect individuals at each consanguinity level
    findall(Y, levelConsanguinity(X, Y, 1), LevelOneList),
    findall(Y, levelConsanguinity(X, Y, 2), LevelTwoList),
    findall(Y, levelConsanguinity(X, Y, 3), LevelThreeList),

    length(LevelOneList, LevelOneCount),
    length(LevelTwoList, LevelTwoCount),
    length(LevelThreeList, LevelThreeCount),

    FirstLevelTotal is LevelOneCount * 30,
    SecondLevelTotal is LevelTwoCount * 20,
    ThirdLevelTotal is LevelThreeCount * 10,

    TotalSum is FirstLevelTotal + SecondLevelTotal + ThirdLevelTotal,

    (TotalSum > 100 ->
        FirstLevelPercent is (FirstLevelTotal * 100) / TotalSum,     %Si la suma de los porcentajes da mayor a 100, entonces se utiliza la formula de normalizacion para
        SecondLevelPercent is (SecondLevelTotal * 100) / TotalSum,   % poder distribuir los porcentajes equitativamente.
        ThirdLevelPercent is (ThirdLevelTotal * 100) / TotalSum; 

	%Sino, entonces simplemente se mantienen los porcentajes de antes.
        FirstLevelPercent is FirstLevelTotal,
        SecondLevelPercent is SecondLevelTotal,
        ThirdLevelPercent is ThirdLevelTotal
    ),

    % Calcular la cantidad que se debe repartir por nivel
    InheritanceFirstLevelTotal is (FirstLevelPercent / 100) * Total,    %Se divide el porcentaje que teniamos antes entre 100 para tener un 
    InheritanceSecondLevelTotal is (SecondLevelPercent / 100) * Total,   %numero con el cual reducir nuestro total de herencia
    InheritanceThirdLevelTotal is (ThirdLevelPercent / 100) * Total,

    % Calcular e imprimir la herencia para cada nivel
    (LevelOneCount > 0 ->
        InheritanceFirstLevelAmount is InheritanceFirstLevelTotal / LevelOneCount,
        print_list(LevelOneList, InheritanceFirstLevelAmount, InheritanceFirstLevelTotal);
        InheritanceFirstLevelAmount = 0),

    (LevelTwoCount > 0 ->
        InheritanceSecondLevelAmount is InheritanceSecondLevelTotal / LevelTwoCount,
        print_list(LevelTwoList, InheritanceSecondLevelAmount, InheritanceSecondLevelTotal);
        InheritanceSecondLevelAmount = 0),

    (LevelThreeCount > 0 ->
        InheritanceThirdLevelAmount is InheritanceThirdLevelTotal / LevelThreeCount,
        print_list(LevelThreeList, InheritanceThirdLevelAmount, InheritanceThirdLevelTotal);
        InheritanceThirdLevelAmount = 0),

    % Form the distribution list
    Distribution = [
        first_level(InheritanceFirstLevelAmount, InheritanceFirstLevelTotal),
        second_level(InheritanceSecondLevelAmount, InheritanceSecondLevelTotal),
        third_level(InheritanceThirdLevelAmount, InheritanceThirdLevelTotal)
    ].

% Funcion para imprimir toda la ifnromacion
print_list([], _, _).
print_list([Individual | Remaining], Amount, Total) :-

    % Calcular el porcentaje basado en el total del nivel
    Percentage is (Amount / Total) * 100,
    format('~w, ~2f, ~2f~n', [Individual, Amount, Percentage]),
    print_list(Remaining, Amount, Total).


