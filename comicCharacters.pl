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

fullCompetitors(Char1, Char2):-
	semiCompetitors(Char1, Char2),
	allPowers(Char1, Ps1),
	allPowers(Char2, Ps2),
	subtract(Ps1, Ps2, []).
	

possibleSoulmate(CharacterA,CharacterB):-
	meta(CharacterA, _, _,SexA,Race,_,_),
	seek(SexA,SexB),
	meta(CharacterB,_, _,SexB,Race,_,_).

% find a character by genre and sex
maleCharacter(Character):-
	meta(Character, _, _,man,_,_,_).

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
    



% FACTS ————————————————————————————————————————————

creator(marvel, spiderman).
creator(marvel, doctorOctopus).
creator(dc, superman).
creator(marvel, deadpool).
creator(dc, lexLuthor).
creator(dc, batman).

creator(dc, wonderWoman).
creator(dc, greenLantern).
creator(dc, deathstroke).
creator(dc, supergirl).
creator(dc, doctorFate).
creator(dc, catwoman).
creator(dc, atomGirl).
creator(dc, johnConstantine).
creator(dc, redHood).
creator(dc, sportsmaster).
creator(dc, generalZod).
creator(marvel, thor).
creator(marvel, wolverine).
creator(marvel, storm).
creator(marvel, silverSurfer).
creator(marvel, thePunisher).
creator(marvel, thing).
creator(marvel, rogue).
creator(marvel, blackWidow).
creator(marvel, phoenix).
creator(marvel, banshee).
creator(marvel, gamora).
creator(marvel, goblinQueen).
creator(marvel, hawkgirl).
creator(marvel, quicksilver).
creator(marvel, shadowKing).
creator(marvel, siryn).

arrests(spiderman, villain(_)).
arrests(superman, villain(_)).
arrests(deadpool, villain(_)).
arrests(batman, villain(_)).
arrests(wonderWoman, villain(_)).

hurts(spiderman, villain(_)).
hurts(doctorOctopus, maryJane).
hurts(doctorOctopus, supermeta(_)).
hurts(superman, villain(_)).
hurts(deadpool, villain(_)).
hurts(lexLuthor, supermeta(_)).
hurts(lexLuthor, loisLane).
hurts(batman, villain(_)).
hurts(wonderWoman, villain(_)).

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
power(batman, martialArts).
power(wonderWoman, strength).
power(wonderWoman, flight).
power(wonderWoman, speed).
power(wonderWoman, reflexes).

weapon(spiderman, spiderWeb).
weapon(doctorOctopus, mechanicalArms).
weapon(deadpool, blades).
weapon(deadpool, guns).
weapon(lexLuthor, kryptonite).
weapon(lexLuthor, strengthSuit).
weapon(lexLuthor, flightSuit).
weapon(batman, utilityBelt).
weapon(batman, car).
weapon(wonderWoman, lassoOfTruth).
weapon(wonderWoman, magicalBracelets).


weakness(doctorOctopus, inferiorityComplex).
weakness(superman, kryptonite).
weakness(lexLuthor, onlyHuman).
weakness(batman, onlyHuman).


born(spiderman, earth).
born(doctorOctopus, earth).
born(superman, krypton).
born(deadpool, earth).
born(lexLuthor, earth).
born(batman, earth).
born(


evilPlan(doctorOctopus, controlLifeAndDeath).
evilPlan(lexLuthor, kills(lexLuthor, superman)).

realName(spiderman, peterParker).
realName(doctorOctopus, ottoOctavius).
realName(superman, kalEl).
realName(superman, clarkKent).
realName(deadpool, wadeWilson).
realName(lexLuthor, alexanderLuthor).
realName(batman, bruceWayne).

person(maryJane).
person(loisLane).
person(vanessa).

friends(spiderman, maryJane).
friends(doctorOctopus, roselitaOctavius).
friends(superman, loisLane).
friends(deadpool, vanessa).


%% meta: name height weight sex orgin hairColour alignment

hair(1, black). 
hair(2, brown).
hair(3, blond).
hair(4, red).
hair(5, white).
hair(6, noHair).

alignment(1, veryGood).
alignment(2, good).
alignment(3, neutral).
alignment(4, strange).
alignment(5, bad).


meta(doctorOctopus, 178, 76, man, metahuman,2,5).
meta(lexLuthor, 188, 95, man, human,6,5).
meta(batman, 188, 95, man, human,1,1).
meta(thor, 198, 105, man, alien,3,1).
meta(superman, 190, 100, man, alien,1,1).
meta(spiderman, 178, 64, man, metahuman,2,1).
meta(wonderWoman, 183, 60, woman, human,1,1).
meta(wolverine, 176, 166, man, metahuman,1,2).
meta(greenLantern, 180, 82, man, human,2,2).
meta(storm, 180, 58, woman, metahuman,5,1).
meta(deathstroke, 193, 102, man, human,5,5).
meta(silverSurfer, 195, 92, man, alien,6,4).
meta(supergirl, 171, 54, woman, alien,3,1).
meta(thePunisher, 180, 90, man, human,1,4).
meta(doctorFate, 187, 90, man, human,3,3).
meta(thing, 190, 200, man, metahuman,6,1).
meta(catwoman, 175, 61, woman, human,1,2).
meta(rogue, 168, 54, woman, metahuman,2,2).
meta(atomGirl, 175, 61, woman, human,1,2).
meta(blackWidow, 170, 59, woman, metahuman,4,2).
meta(deadpool, 188, 95, man, metahuman,2,3).
meta(phoenix, 168, 52, woman, metahuman,4,2).
meta(banshee, 183, 77, woman, metahuman,3,2).
meta(gamora, 183, 77, woman, alien,1,2).
meta(goblinQueen, 183, 77, woman, metahuman,4,5).
meta(hawkgirl, 175, 61, woman, metahuman,4,1).
meta(quicksilver, 183, 79, man, metahuman,5,2).
meta(johnConstantine, 183, 89, man, human,3,2).
meta(shadowKing, 185, 249, man, alien,6,2).
meta(siryn, 168, 52, woman, metahuman,4,5).
meta(redHood, 183, 81, man, human,4,5).
meta(sportsmaster, 180, 90, man, human,1,4).
meta(generalZod, 190, 100, man, alien,1,5).




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
%% meta(X, Y, Z, man, alien,HC).











