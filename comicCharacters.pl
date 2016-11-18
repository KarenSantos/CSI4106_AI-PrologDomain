% Comics Characters

% This is a knowledge base of Characters of the comic books.

% Questions that can be asked ————————————————————————————————————————————————————

% Who is a human? Is this character a human?

human(Character):-
	born(Character, earth).

% Who is a metahuman? Is this character a metahuman?

metahuman(Character):-
	human(Character).
%	bornWithPower(Character).

% Who are the characters in the comics?

character(Character):-
	superhero(Character);
	antihero(Character);
	villain(Character);
	person(Character).
	
% Who are all the distinct characters?

allCharacters(List):-
	setof(Char, character(Char), List).

% Is a character from Marvel comics? Who is a character from Marvel comics?

marvel(Character):-
	creator(marvel, Character).

% Is a character from DC comics? Who is a character from DC comics?

dc(Character):-
	creator(dc, Character).

% Is this character a Superhero? Who is a Superhero?

superhero(Character):-
	arrests(Character, villain(_)),
	hurts(Character, villain(_)).

% Is this character a superhero from Marvel comics? Who is a Superhero from Marvel comics?

superhero(Character, marvel):-
	superhero(Character),
	marvel(Character).

% Is this character a superhero from DC comics? Who is a superhero from DC comics?

superhero(Character, dc):-
	superhero(Character),
	dc(Character).

% Is this character an Antihero? Who is an Antihero?

antihero(Character):-
	arrests(Character, villain(_)),
	hurts(Character, villain(_)),
	kills(Character, villain(_)),
	\evilplan(Character, _).
	
% Is this character an antihero from Marvel comics? Who is an antihero from Marvel comics?

antihero(Character, marvel):-
	antihero(Character),
	marvel(Character).

% Is this character an antihero from DC comics? Who is an antihero from DC comics?

antihero(Character, dc):-
	antihero(Character),
	dc(Character).

% Is this character a villain? Who is a villain?

villain(Character):-
	kills(Character, _),
	hurts(Character, _),
	evilPlan(Character, _).

% Is this character a villain from Marvel comics? Who is a villain from Marvel comics?

villain(Character, marvel):-
	villain(Character),
	marvel(Character).

% Is character a villain from DC comics? Who is a villain from DC comics?

villain(Character, dc):-
	villain(Character),
	dc(Character).

% Are these characters from the same creator?

sameCreator(Char1, Char2):-
	Char1 \= Char2,
	creator(C, Char1),
	creator(C, Char2).
	
% Are these characters from different creators?

diferentCreator(Char1, Char2):-
	Char1 \= Char2,
	creator(C1, Char1),
	creator(C2, Char2),
	C1 \= C2.

% What are all the power of a character?

allPowers(Character, Powers):-
	findall(Power, power(Character, Power), P),
	sort(P, Powers).

% What superhero is archenemy of what villain?

archenemy(Superhero, Villain):-
	superhero(Superhero),
	villain(Villain),
	hurts(Villain, Person),
	friends(Superhero, Person).

% Is a character part of an experiment?

experiment(Character):-
	human(Character),
	\metahuman(Character),
	power(_).

% What character have the same powers?
    
samePowers(Char1, Char2, Powers):-
	Char1 \= Char2,
	allPowers(Char1, Ps1),
	allPowers(Char2, Ps2),
	intersection(Ps1, Ps2, Powers).

% What characters are semi competitors?

semiCompetitors(Char1, Char2):-
	superhero(Char1),
	superhero(Char2),
	diferentCreator(Char1, Char2).
	
semiCompetitors(Char1, Char2):-
	villain(Char1),
	villain(Char2),
	differentCreator(Char1, Char2).

% What characters are competitors?

competitors(Char1, Char2):-
	semiCompetitors(Char1, Char2),
	samePowers(Char1, Char2, Powers),
	length(Powers, Len),
	Len >= 3.
	
competitors(Char1, Char2):-
	semiCompetitors(Char1, Char2),
	evilPlan(Char1, X),
	evilPlan(Char2, X).
	
% What characters are full competitors?

% fullCompetitors(Char1, Char2):-     % continue full competitors.
	semiCompetitors(Char1, Char2),
	

% Auxiliary functions

intersection([], _, []).
intersection([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    intersection(T1, L2, Res).
intersection([_|T1], L2, Res) :-
    intersection(T1, L2, Res).
    
% subtract([], _, []).                % correct subtract function
% subtract([H|T], L2, L3) :-
        memberchk(H, L2),
        !,
        subtract(T, L2, L3).
% subtract([H|T1], L2, [H|T3]) :-
        subtract(T1, L2, T3).


% FACTS ————————————————————————————————————————————

creator(marvel, spiderman).
creator(marvel, doctorOctopus).
creator(dc, superman).
creator(marvel, deadpool).
creator(dc, lexLuthor).

arrests(spiderman, villain(_)).
arrests(superman, villain(_)).
arrests(deadpool, villain(_)).

hurts(spiderman, villain(_)).
hurts(doctorOctopus, maryJane).
hurts(doctorOctopus, superhero(_)).
hurts(superman, villain(_)).
hurts(deadpool, villain(_)).
hurts(lexLuthor, superhero(_)).
hurts(lexLuthor, loisLane).

kills(doctorOctopus, anyone).
kills(deadpool, villain(_)).
kills(lexLuthor, anyone).

power(spiderman, strength).
power(spiderman, reflexes).
power(spiderman, equilibrium).
power(spiderman, spiderClaw).
power(doctorOctopus, intelligence).
power(superman, strength).
power(superman, speed).
power(superman, flight).
power(superman, vision).
power(superman, breath).
power(superman, hearing).
power(superman, healing).
power(deadpool, healing).
power(deadpool, martialArts).
power(lexLuthor, intelligence).

weapon(spiderman, spiderWeb).
weapon(doctorOctopus, mechanicalArms).
weapon(deadpool, blades).
weapon(deadpool, guns).
weapon(lexLuthor, kryptonite).
weapon(lexLuthor, strengthSuit).
weapon(lexLuthor, flightSuit).

weakness(doctorOctopus, inferiorityComplex).
weakness(superman, kryptonite).
weakness(lexLuthor, onlyHuman).

born(spiderman, earth).
born(doctorOctopus, earth).
born(superman, krypton).
born(deadpool, earth).
born(lexLuthor, earth).

evilPlan(doctorOctopus, controlLifeAndDeath).
evilPlan(lexLuthor, kills(lexLuthor, superman)).

realName(spiderman, peterParker).
realName(doctorOctopus, ottoOctavius).
realName(superman, kalEl).
realName(superman, clarkKent).
realName(deadpool, wadeWilson).
realName(lexLuthor, alexanderLuthor).

person(maryJane).
person(loisLane).
person(vanessa).

friends(spiderman, maryJane).
friends(doctorOctopus, roselitaOctavius).
friends(superman, loisLane).
friends(deadpool, vanessa).












