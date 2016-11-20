import sys,io,pprint,collections,time
place="earth"
with open('character.txt', 'r') as f:
    for line in f:
        c = line.strip()
        print('bornWithPower('+c+').')
        # print('born('+c+', '+place+').')