import sys,io,pprint,collections,time
temp=[]
with open('power_raw.txt', 'r') as f:
    for line in f:
    	name = line.strip()
    	if name!='':
    		temp.append(name)
    	else:
    		for i in range(1,len(temp)):
    			print('power("'+temp[0]+'",'+temp[i].lower()+').')
    		temp=[]
