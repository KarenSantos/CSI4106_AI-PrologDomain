% Comics Characters

% This is a knowledge base of Characters of the comics. A character has a creator, a super power and a weakness. A character can be good or evil, human or not, can use a weapon or not, and can use a cape or not.


% Questions that can be asked ————————————————————————————————————————————————————

% What is a human?

human(Person):-
	born(Person, terra).


% Is X a character from Marvel comics? What is a character X from Marvel comics?

marvel(X):-
	creator(marvel, X).


% What is a character X from DC comics?

dc(X):-
	creator(dc, X).


% Is this Person a Superhero? What is a Superhero X?

superhero(Person):-
	arrests(Person, badGuys);
	arrests(Person, villain(_)).

% Is X a superhero from Marvel comics? What is a superhero X from Marvel comics?

superhero(X, marvel):-
	superHero(X),
	marvel(X).

% Is X a superhero from DC comics? What is a superhero X from DC comics?

superhero(X, dc):-
	superHero(X),
	dc(X).


% Is X a villain? What is a villain X?

villain(X):-
	kills(X, _);
	hurt(X, goodGuys);
	hurt(X, badGuys);
	hurt(X, superhero(Y)).

% Is X a villain from Marvel comics? What is a villain X from Marvel comics?

villain(X, marvel):-
	villain(X),
	marvel(X).

% Is X a villain from DC comics? What is a villain X from DC comics?

villain(X, dc):-
	villain(X),
	dc(X).


% What superhero X is archenemy of villain Y?

archenemy(X, Y):-
	superhero(X),
	villain(Y),
	hurt(Y, Z),
	friends(X, Z).


% Other questions that can be made using the facts —————————————————————————————————

% What character X has power Y?
% What is the real name of character X?
% What character X is human?
% What character X is not human?




% FACTS ————————————————————————————————————————————

creator(marvel, spiderMan).
creator(marvel, doctorOctopus).

creator(dc, superman).
	

arrests(spiderMan, badGuys).
arrests(spiderMan, doctorOctopus).

arrests(superman, badGuys).


hurt(doctorOctopus, maryJane).


kills(doctorOctopus, goodGuys).


power(spiderMan, strength).
power(spiderMan, reflexes).
power(spiderMan, equilibrium).
power(spiderMan, spiderClaw).


weakness(spiderMan, none).


weapon(spiderMan, spiderWeb).


realName(spiderMan, peterParker).


human(spiderMan).


friends(spiderMan, maryJane).

