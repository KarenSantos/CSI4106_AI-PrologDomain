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
	supermeta(Character);
	antimeta(Character);
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

% Is this character a Supermeta? Who is a Supermeta?

supermeta(Character):-
	arrests(Character, villain(_)),
	hurts(Character, villain(_)).

% Is this character a supermeta from Marvel comics? Who is a Supermeta from Marvel comics?

supermeta(Character, marvel):-
	supermeta(Character),
	marvel(Character).

% Is this character a supermeta from DC comics? Who is a supermeta from DC comics?

supermeta(Character, dc):-
	supermeta(Character),
	dc(Character).

% Is this character an Antimeta? Who is an Antimeta?

antimeta(Character):-
	arrests(Character, villain(_)),
	hurts(Character, villain(_)),
	kills(Character, villain(_)),
	\evilplan(Character, _).
	
% Is this character an antimeta from Marvel comics? Who is an antimeta from Marvel comics?

antimeta(Character, marvel):-
	antimeta(Character),
	marvel(Character).

% Is this character an antimeta from DC comics? Who is an antimeta from DC comics?

antimeta(Character, dc):-
	antimeta(Character),
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

differentCreator(Char1, Char2):-
	Char1 \= Char2,
	creator(C1, Char1),
	creator(C2, Char2),
	C1 \= C2.

% What are all the power of a character?

allPowers(Character, Powers):-
	findall(Power, power(Character, Power), P),
	sort(P, Powers).

% What supermeta is archenemy of what villain?

archenemy(Supermeta, Villain):-
	supermeta(Supermeta),
	villain(Villain),
	hurts(Villain, Person),
	friends(Supermeta, Person).

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
	supermeta(Char1),
	supermeta(Char2),
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
	%% semiCompetitors(Char1, Char2),
seek("man","woman").
seek("woman","man").

possibleSoulmate(CharacterA,CharacterB):-
	meta(CharacterA, _, _,SexA,Race,_,_),
	seek(SexA,SexB),
	meta(CharacterB,_, _,SexB,Race,_,_).

% find a character by genere and sex
maleCharacter(Character):-
	meta(Character _, _,"man",_,_,_).

dcMale(Character):-
	maleCharacter(Character),
	dc(Character).

crossOverSoulmate(CharacterA,CharacterB):-
	meta(CharacterA, _, _, SexA,_,_,_),
	seek(SexA,SexB),
	meta(CharacterB,_, _,SexB,_,_,_),
	differentCreator(CharacterA,CharacterB).

%% crossOver(dc,marvel).
%% crossOver(marvel,dc).

crossOverBattle(CharacterA,CharacterB):-
	meta(CharacterA, _, _, _,_,_,AlignmentA),
	AlignmentA =:= 3,
	meta(CharacterB, _, _, _,_,_,_),
	differentCreator(CharacterA,CharacterB);
	meta(CharacterA, _, _, _,_,_,AlignmentA),
	AlignmentA < 3,
	meta(CharacterB, _, _, _,_,_,AlignmentB),
	member(AlignmentB, [3,4,5]),
	differentCreator(CharacterA,CharacterB);
	meta(CharacterA, _, _, _,_,_,AlignmentA),
	AlignmentA > 3,
	meta(CharacterB, _, _, _,_,_,AlignmentB),
	member(AlignmentB, [1,2,3]),
	differentCreator(CharacterA,CharacterB).


%% crossOverBattle(CharacterA,CharacterB):-
%% 	meta(CharacterA, _, _, _,_,_,AlignmentA),
%% 	AlignmentA < 3,
%% 	meta(CharacterB, _, _, _,_,_,AlignmentB),
%% 	member(AlignmentB, [3,4,5]),
%% 	differentCreator(CharacterA,CharacterB).

%% crossOverBattle(CharacterA,CharacterB):-
%% 	meta(CharacterA, _, _, _,_,_,AlignmentA),
%% 	AlignmentA > 3,
%% 	meta(CharacterB, _, _, _,_,_,AlignmentB),
%% 	member(AlignmentB, [1,2,3]),
%% 	differentCreator(CharacterA,CharacterB).
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

creator(marvel, "Spiderman").
creator(marvel, "Doctor Octopus").
creator(dc, "Superman").
creator(marvel, "Deadpool").
creator(dc, "Lex Luthor").

creator(dc, "Batman").
creator(dc, "Wonder Woman").
creator(dc, "Green Lantern").
creator(dc, "Deathstroke").
creator(dc, "Supergirl").
creator(dc, "Doctor Fate").
creator(dc, "Catwoman").
creator(dc, "Atom Girl").
creator(dc, "John Constantine").
creator(dc, "Red Hood").
creator(dc, "Sportsmaster").
creator(dc, "General Zod").
creator(marvel, "Thor").
creator(marvel, "Wolverine").
creator(marvel, "Storm").
creator(marvel, "Silver Surfer").
creator(marvel, "The Punisher").
creator(marvel, "Thing").
creator(marvel, "Rogue").
creator(marvel, "Black Widow").
creator(marvel, "Phoenix").
creator(marvel, "Banshee").
creator(marvel, "Gamora").
creator(marvel, "Goblin Queen").
creator(marvel, "Hawkgirl").
creator(marvel, "Quicksilver").
creator(marvel, "Shadow King").
creator(marvel, "Siryn").


arrests("Spiderman", villain(_)).
arrests("Superman", villain(_)).
arrests("Deadpool", villain(_)).

hurts("Spiderman", villain(_)).
hurts("Doctor Octopus", maryJane).
hurts("Doctor Octopus", supermeta(_)).
hurts("Superman", villain(_)).
hurts("Deadpool", villain(_)).
hurts("Lex Luthor", supermeta(_)).
hurts("Lex Luthor", loisLane).

kills("Doctor Octopus", anyone).
kills("Deadpool", villain(_)).
kills("Lex Luthor", anyone).

power("Spiderman", strength).
power("Spiderman", reflexes).
power("Spiderman", equilibrium).
power("Spiderman", spiderClaw).
power("Doctor Octopus", intelligence).
power("Superman", strength).
power("Superman", speed).
power("Superman", flight).
power("Superman", vision).
power("Superman", breath).
power("Superman", hearing).
power("Superman", healing).
power("Deadpool", healing).
power("Deadpool", martialArts).
power("Lex Luthor", intelligence).

weapon("Spiderman", spiderWeb).
weapon("Doctor Octopus", mechanicalArms).
weapon("Deadpool", blades).
weapon("Deadpool", guns).
weapon("Lex Luthor", kryptonite).
weapon("Lex Luthor", strengthSuit).
weapon("Lex Luthor", flightSuit).

weakness("Doctor Octopus", inferiorityComplex).
weakness("Superman", kryptonite).
weakness("Lex Luthor", onlyHuman).

born("Spiderman", earth).
born("Doctor Octopus", earth).
born("Superman", krypton).
born("Deadpool", earth).
born("Lex Luthor", earth).

evilPlan("Doctor Octopus", controlLifeAndDeath).
evilPlan("Lex Luthor", kills("Lex Luthor", "Superman")).

realName("Spiderman", peterParker).
realName("Doctor Octopus", ottoOctavius).
realName("Superman", kalEl).
realName("Superman", clarkKent).
realName("Deadpool", wadeWilson).
realName("Lex Luthor", alexanderLuthor).

person(maryJane).
person(loisLane).
person(vanessa).

friends("Spiderman", maryJane).
friends("Doctor Octopus", roselitaOctavius).
friends("Superman", loisLane).
friends("Deadpool", vanessa).



%% meta: name height weight sex orgin hairColour alignment

hair(1, "black"). 
hair(2, "brown").
hair(3, "blond").
hair(4, "red").
hair(5, "white").
hair(6, "no hair/bald").

alignment(1, "Very Good").
alignment(2, "Good").
alignment(3, "Neutral").
alignment(4, "Strange").
alignment(5, "Bad").


meta("Doctor Octopus", 178, 76, "man", "mutant",2,5).
meta("Lex Luthor", 188, 95, "man", "human",6,5).
meta("Batman", 188, 95, "man", "human",1,1).
meta("Thor", 198, 105, "man", "god",3,1).
meta("Superman", 190, 100, "man", "alien",1,1).
meta("Spiderman", 178, 64, "man", "mutant",2,1).
meta("Wonder Woman", 183, 60, "woman", "human",1,1).
meta("Wolverine", 176, 166, "man", "mutant",1,2).
meta("Green Lantern", 180, 82, "man", "human",2,2).
meta("Storm", 180, 58, "woman", "mutant",5,1).
meta("Deathstroke", 193, 102, "man", "human",5,5).
meta("Silver Surfer", 195, 92, "man", "alien",6,4).
meta("Supergirl", 171, 54, "woman", "alien",3,1).
meta("The Punisher", 180, 90, "man", "human",1,4).
meta("Doctor Fate", 187, 90, "man", "human",3,3).
meta("Thing", 190, 200, "man", "mutant",6,1).
meta("Catwoman", 175, 61, "woman", "human",1,2).
meta("Rogue", 168, 54, "woman", "mutant",2,2).
meta("Atom Girl", 175, 61, "woman", "human",1,2).
meta("Black Widow", 170, 59, "woman", "mutant",4,2).
meta("Deadpool", 188, 95, "man", "mutant",2,3).
meta("Phoenix", 168, 52, "woman", "mutant",4,2).
meta("Banshee", 183, 77, "woman", "mutant",3,2).
meta("Gamora", 183, 77, "woman", "god",1,2).
meta("Goblin Queen", 183, 77, "woman", "mutant",4,5).
meta("Hawkgirl", 175, 61, "woman", "mutant",4,1).
meta("Quicksilver", 183, 79, "man", "mutant",5,2).
meta("John Constantine", 183, 89, "man", "human",3,2).
meta("Shadow King", 185, 249, "man", "alien",6,2).
meta("Siryn", 168, 52, "woman", "mutant",4,5).
meta("Red Hood", 183, 81, "man", "human",4,5).
meta("Sportsmaster", 180, 90, "man", "human",1,4).
meta("General Zod", 190, 100, "man", "alien",1,5).







%% who_am_I():-
%% write("who am I in Comics?\n"),
%% write("Are you a man?(yes/no): "),
%% read(A),
%% %% manOrWoman(A, Gen), nl,
%% write("hair color you like: "), nl,
%% write("1,coal black"), nl,
%% write("2,brown"), nl,
%% write("3,blond"), nl,
%% write("4,fireRed"), nl,
%% write("5,snowWhite"), nl,
%% write("6 bald"), nl,
%% read(HC),
%% write("Your weight:"),
%% read(B),
%% %% weight(B, W1, W2),
%% write("Your height:"),
%% read(C),
%% meta(X, Y, Z, "man", "alien",HC).











