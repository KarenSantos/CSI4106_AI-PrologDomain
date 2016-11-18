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

% What characters are competitors?

competitor(Char1, Char2):-
	superhero(Char1, marvel),
	superhero(Char2, dc),
	power(Char1, X),         % ToDo: change this to at least 3 powers that are the same.
	power(Char2, X).
	
competitor(Char1, Char2):-
	villain(Char1),
	villain(Char2),
	evilPlan(Char1, X),
	evilPlan(Char2, X).
	
% What 

% FACTS ————————————————————————————————————————————

creator(marvel, spiderman).
creator(marvel, doctorOctopus).
creator(dc, superman).
creator(marvel, deadpool).

arrests(spiderman, villain(_)).
arrests(superman, villain(_)).
arrests(deadpool, villain(_)).

hurts(spiderman, villain(_)).
hurts(doctorOctopus, maryJane).
hurts(doctorOctopus, superhero(_)).
hurts(superman, villain(_)).
hurts(deadpool, villain(_)).

kills(doctorOctopus, anyone).
kills(deadpool, villain(_)).

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

weapon(spiderman, spiderWeb).
weapon(doctorOctopus, mechanicalArms).
weapon(deadpool, blades).
weapon(deadpool, guns).

born(spiderman, earth).
born(doctorOctopus, earth).
born(superman, krypton).
born(deadpool, earth).

evilPlan(doctorOctopus, controlLifeAndDeath).

realName(spiderman, peterParker).
realName(doctorOctopus, ottoOctavius).
realName(superman, kalEl).
realName(superman, clarkKent).
realName(deadpool, wadeWilson).

person(maryJane).
person(loisLane).
person(vanessa).

friends(spiderman, maryJane).
friends(doctorOctopus, roselitaOctavius).
friends(superman, loisLane).
friends(deadpool, vanessa).












