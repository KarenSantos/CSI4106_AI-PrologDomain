% Comics Characters

% This is a knowledge base of Characters of the comic books.

% Questions that can be asked ————————————————————————————————————————————————————

% Who is a human? Is this character a human?

human(Character):-
	born(Character, earth).

% Who is a metahuman? Is this character a metahuman?

metahuman(Character):-
	human(Character),
	bornWithPower(Character).

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

% Is this character a superhero? Who is a superhero?

superhero(Character):-
	arrests(Character, villain(_)),
	hurts(Character, villain(_)).

% Is this character a superhero from Marvel comics? Who is a superhero from Marvel comics?

superhero(Character, marvel):-
	superhero(Character),
	marvel(Character).

% Is this character a superhero from DC comics? Who is a superhero from DC comics?

superhero(Character, dc):-
	superhero(Character),
	dc(Character).

% Is this character an antihero? Who is an antihero?

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

differentCreator(Char1, Char2):-
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
	power(Character, _),
	\metahuman(Character).

% What are the powers in common that two characters have?
    
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
	
% What characters are full competitors?

fullCompetitors(Char1, Char2):-
	semiCompetitors(Char1, Char2),
	allPowers(Char1, Ps1),
	allPowers(Char2, Ps2),
	subtract(Ps1, Ps2, []).

fullCompetitors(Char1, Char2):-
	competitors(Char1, Char2),
	evilPlan(Char1, X),
	evilPlan(Char2, X).
	
% What characters can have a romance?

romance(Char1, Char2):-
	Char1 \= Char2,
	info(Char1, Height1, _, man, Race, _, _),
	info(Char2, Height2, _, woman, Race, _, _),
	Height1 >= Height2.
	
romance(Char1, Char2):-
	Char1 \= Char2,
	info(Char1, Height1, _, woman, Race, _, _),
	info(Char2, Height2, _, man, Race, _, _),
	Height1 =< Height2.

% What characters can have a possible romance?

possibleRomance(Char1, Char2):-
	romance(Char1, Char2),
	sameCreator(Char1, Char2).

% What character can have an impossible romance?

impossibleRomance(Char1, Char2):-
	romance(Char1, Char2),
	differentCreator(Char1, Char2).

% What characters can battle according to their alignments?

crossOverBattle(Char1,Char2):-
	info(Char1, _, _, _,_,_,AlignmentA),
	AlignmentA =:= 3,
	info(Char2, _, _, _,_,_,_),
	differentCreator(Char1,Char2).

crossOverBattle(Char1, Char2):-
	info(Char1, _, _, _,_,_,AlignmentA),
	AlignmentA < 3,
	info(Char2, _, _, _,_,_,AlignmentB),
	member(AlignmentB, [3,4,5]),
	differentCreator(Char1,Char2).

crossOverBattle(Char1, Char2):-
	info(Char1, _, _, _,_,_,AlignmentA),
	AlignmentA > 3,
	info(Char2, _, _, _,_,_,AlignmentB),
	member(AlignmentB, [1,2,3]),
	differentCreator(Char1,Char2).


% Auxiliary function

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
creator(marvel, thePunisher).
creator(dc, catwoman).
creator(dc, atomgirl).
creator(dc, hellblazer).
creator(dc, redHood).
creator(dc, sportsmaster).
creator(dc, generalZod).
creator(marvel, thor).
creator(marvel, wolverine).
creator(marvel, storm).
creator(marvel, silverSurfer).
creator(marvel, thing).
creator(marvel, rogue).
creator(marvel, blackWidow).
creator(marvel, phoenix).
creator(marvel, banshee).
creator(marvel, gamora).
creator(marvel, quicksilver).

arrests(spiderman, villain(_)).
arrests(superman, villain(_)).
arrests(deadpool, villain(_)).
arrests(batman, villain(_)).
arrests(wonderWoman, villain(_)).
arrests(greenLantern, villain(_)).
arrests(supergirl, villain(_)).
arrests(doctorFate, villain(_)).
arrests(thePunisher, villain(_)).
arrests(catwoman, villain(_)).
arrests(atomgirl, villain(_)).
arrests(hellblazer, villain(_)).
arrests(redHood, villain(_)).
arrests(thor, villain(_)).
arrests(wolverine, villain(_)).
arrests(storm, villain(_)).
arrests(thing, villain(_)).
arrests(rogue, villain(_)).
arrests(blackWidow, villain(_)).
arrests(phoenix, villain(_)).
arrests(banshee, villain(_)).
arrests(gamora, villain(_)).
arrests(quicksilver, villain(_)).

hurts(spiderman, villain(_)).
hurts(doctorOctopus, maryJane).
hurts(doctorOctopus, superhero(_)).
hurts(doctorOctopus, anyone).
hurts(superman, villain(_)).
hurts(deadpool, villain(_)).
hurts(lexLuthor, superhero(_)).
hurts(lexLuthor, loisLane).
hurts(lexLuthor, anyone).
hurts(batman, villain(_)).
hurts(wonderWoman, villain(_)).
hurts(greenLantern, villain(_)).
hurts(deathStroke, superhero(_)).
hurts(deathStroke, anyone).
hurts(supergirl, villain(_)).
hurts(doctorFace, villain(_)).
hurts(thePunisher, villain(_)).
hurts(catwoman, villain(_)).
hurts(atomgirl, villain(_)).
hurts(hellblazer, villain(_)).
hurts(redHood, villain(_)).
hurts(sportsmaster, superhero(_)).
hurts(sportsmaster, anyone).
hurts(generalZod, superhero(_)).
hurts(generalZod, anyone).
hurts(thor, villain(_)).
hurts(wolverine, villain(_)).
hurts(storm, villain(_)).
hurts(silverSurfer, superhero(_)).
hurts(silverSurfer, anyone).
hurts(thing, villain(_)).
hurts(rogue, villain(_)).
hurts(blackWidow, villain(_)).
hurts(phoenix, villain(_)).
hurts(banshee, villain(_)).
hurts(gamora, villain(_)).
hurts(quicksilver, villain(_)).

kills(doctorOctopus, anyone).
kills(doctorOctopus, superhero(_)).
kills(deadpool, villain(_)).
kills(lexLuthor, anyone).
kills(lexLuthor, superhero(_)).
kills(deathStroke, anyone).
kills(deathStroke, superhero(_)).
kills(thePunisher, villain(_)).
kills(catwoman, villain(_)).
kills(hellblazer, villain(_)).
kills(redHood, villain(_)).
kills(sportsmaster, superhero(_)).
kills(sportsmaster, anyone).
kills(generalZod, superhero(_)).
kills(generalZod, anyone).
kills(wolverine, villain(_)).
kills(silverSurfer, superhero(_)).
kills(silverSurfer, anyone).
kills(gamora, villain(_)).

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
power(deathStroke, strength).
power(deathStroke, speed).
power(deathStroke, durability).
power(deathStroke, agility).
power(deathStroke, reflexes).
power(deathStroke, martialArts).
power(supergirl, strength).
power(supergirl, speed).
power(supergirl, flight).
power(supergirl, vision).
power(supergirl, breath).
power(supergirl, hearing).
power(supergirl, healing).
power(doctorFate, strength).
power(doctorFate, flight).
power(doctorFate, magic).
power(thePunisher, martialArts).
power(catwoman, burglary).
power(catwoman, gymnastics).
power(atomgirl, shrinking).
power(hellblazer, magic).
power(redHood, intelligence).
power(redHood, firearms).
power(redHood, acrobatics).
power(redHood, martialArts).
power(redHood, swordsmanship).
power(sportsmaster, reflexes).
power(generalZod, strength).
power(generalZod, speed).
power(generalZod, flight).
power(generalZod, vision).
power(generalZod, breath).
power(generalZod, hearing).
power(generalZod, healing).
power(thor, strength).
power(thor, speed).
power(thor, flight).
power(thor, durability).
power(wolverine, claws).
power(wolverine, senses).
power(wolverine, hearing).
power(wolverine, healing).
power(storm, weatherControl).
power(storm, magic).
power(silverSurfer, strength).
power(silverSurfer, flight).
power(silverSurfer, energyAbsorption).
power(silverSurfer, energyManipulation).
power(thing, strength).
power(thing, martialArts).
power(rogue, absorbsAbilities).
power(rogue, absorbsPsyche).
power(blackWidow, martialArts).
power(phoenix, telekinetics).
power(banshee, supersonicVoice).
power(gamora, strength).
power(gamora, speed).
power(gamora, agility).
power(gamora, martialArts).
power(gamora, healing).
power(quicksilver, ultraSpeed).

bornWithPower(storm).
bornWithPower(thing).
bornWithPower(rogue).
bornWithPower(phoenix).
bornWithPower(banshee).
bornWithPower(quicksilver).

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
weapon(greenLantern, magicalRing).
weapon(deathStroke, guns).
weapon(doctorFate, amulete).
weapon(thePunisher, guns).
weapon(catwoman, whip).
weapon(catwoman, claws).
weapon(redHood, guns).
weapon(sportsmaster, sportsWeapons).
weapon(thor, hammer).
weapon(blackWidow, shockDevices).

weakness(doctorOctopus, inferiorityComplex).
weakness(superman, kryptonite).
weakness(lexLuthor, noSuperPower).
weakness(batman, noSuperPower).
weakness(supergirl, kryptonite).
weakness(thePunisher, noSuperPower).
weakness(catwoman, noSuperPower).
weakness(redHood, noSuperPower).
weakness(redHood, rage).
weakness(sportsmaster, noSuperPower).
weakness(generalZod, arrogance).
weakness(generalZod, kryptonite).
weakness(blackWidow, noSuperPower).

born(spiderman, earth).
born(doctorOctopus, earth).
born(superman, krypton).
born(deadpool, earth).
born(lexLuthor, earth).
born(batman, earth).
born(wonderWoman, olympus).
born(greenLantern, earth).
born(deathStroke, earth).
born(supergirl, krypton).
born(doctorFate, earth).
born(thePunisher, earth).
born(catwoman, earth).
born(atomgirl, earth).
born(hellblazer, earth).
born(redHood, earth).
born(sportsmaster, earth).
born(generalZod, krypton).
born(thor, asgard).
born(wolverine, earth).
born(storm, earth).
born(silverSurfer, zennLa).
born(thing, earth).
born(rogue, earth).
born(blackWidow, earth).
born(phoenix, earth).
born(banshee, earth).
born(gamora, zenWhober).
born(quicksilver, earth).

evilPlan(doctorOctopus, controlLifeAndDeath).
evilPlan(lexLuthor, killSuperheroes).
evilPlan(deathStroke, spreadFear).
evilPlan(sportsmaster, spreadFear).
evilPlan(generalZod, killSuperheroes).
evilPlan(silverSurfer, suckEnergy).

realName(spiderman, peterParker).
realName(doctorOctopus, ottoOctavius).
realName(superman, kalEl).
realName(superman, clarkKent).
realName(deadpool, wadeWilson).
realName(lexLuthor, alexanderLuthor).
realName(batman, bruceWayne).
realName(wonder, dianaPrince).
realName(greenLantern, alanScott).
realName(greenLantern, halJordan).
realName(greenLantern, guyGardner).
realName(deathStroke, sladeWilson).
realName(supergirl, karaZorEl).
realName(supergirl, karaKent).
realName(doctorFate, kentNelson).
realName(thePunisher, frankCastle).
realName(catwoman, selinaKyle).
realName(atomgirl, saluDigby).
realName(hellblazer, johnConstantine).
realName(redHood, jasonTodd).
realName(sportsmaster, lawrenceCrock).
realName(generalZod, druZod).
realName(thor, thorOdinson).
realName(wolverine, logan).
realName(storm, ororoMonroe).
realName(silverSurfer, norrinRadd).
realName(thing, benjaminGrimm).
realName(rogue, annaMarie).
realName(blackWidow, natashaRomanoff).
realName(phoenix, jeanGrey).
realName(banshee, seanCassidy).
realName(gamora, gamora).

person(maryJane).
person(loisLane).
person(vanessa).
person(janeFoster).

friends(spiderman, maryJane).
friends(doctorOctopus, roselitaOctavius).
friends(superman, loisLane).
friends(deadpool, vanessa).
friends(thor, janeFoster).
friends(blackWidow, hawkeye).
friends(gamora, starLord).


% Ethical and moral perspective of characters

alignment(1, veryGood).
alignment(2, good).
alignment(3, neutral).
alignment(4, strange).
alignment(5, bad).


% Personal information:
% character, height in cm, weight in kg, sex, race, hair color, degree of goodness.

info(doctorOctopus, 178, 76, man, human, brown, 5).
info(lexLuthor, 188, 95, man, human, bald, 5).
info(batman, 188, 95, man, human, black, 1).
info(thor, 198, 105, man, alien, blond, 1).
info(superman, 190, 100, man, alien, black, 1).
info(spiderman, 178, 64, man, human, brown, 1).
info(wonderWoman, 183, 60, woman, alien, black, 1).
info(wolverine, 176, 166, man, human, black, 2).
info(greenLantern, 180, 82, man, human, brown, 2).
info(storm, 180, 58, woman, metahuman, white, 1).
info(deathstroke, 193, 102, man, human, white, 5).
info(silverSurfer, 195, 92, man, alien, bald, 4).
info(supergirl, 171, 54, woman, alien, blond, 1).
info(thePunisher, 180, 90, man, human, black, 4).
info(doctorFate, 187, 90, man, human, blond, 3).
info(hellblazer, 183, 89, man, human, blond, 2).
info(catwoman, 175, 61, woman, human, black, 2).
info(thing, 190, 200, man, metahuman, bald, 1).
info(rogue, 168, 54, woman, metahuman, brown, 2).
info(atomgirl, 175, 61, woman, human, black, 2).
info(blackWidow, 170, 59, woman, human, red, 2).
info(deadpool, 188, 95, man, human, brown, 3).
info(phoenix, 168, 52, woman, metahuman, red, 2).
info(banshee, 183, 77, woman, metahuman, blond, 2).
info(gamora, 183, 77, woman, alien, black, 2).
info(quicksilver, 183, 79, man, metahuman, white, 2).
info(redHood, 183, 81, man, human, red, 5).
info(sportsmaster, 180, 90, man, human, black, 4).
info(generalZod, 190, 100, man, alien, black, 5).











