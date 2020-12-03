import sys
import random
import time


# part 1, two numbers
# f = open("/Users/samanthali/useful/sandbox/advent/input1.txt", "r")
# lines = f.readlines()
# mapper = set()
# for raw in lines:
# 	if int(raw) in mapper:
# 		print(int(raw) * (2020-int(raw))) 
# 		break
# 	mapper.add(2020 - int(raw))
# part 2, 3 numbers
f = open("/Users/samanthali/useful/sandbox/advent/input1.txt", "r")
lines = f.readlines()
mapper = {}
mapper2 = {}
for raw in lines:
	mapper[2020-int(raw)] = int(raw)
for k, v in mapper.items():
	for raw in lines:
		mapper2[k - int(raw)] = [int(raw), v]
for raw in lines:
	if int(raw) in mapper2:
		pair = mapper2[int(raw)]
		print(int(raw) * pair[0] * pair[1])
		break
