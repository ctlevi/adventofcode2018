reg0 = 0
reg2 = 1
reg3 = 1
reg5 = 10551296

while True:
	if reg5 % reg2 == 0:
		reg0 = reg0 + reg2
	#while True:
	#	if (reg3*reg2) == reg5:
	#		reg0 = reg0 + reg2
	#	reg3 = reg3 + 1
	#	if reg3 > reg5:
	#		break
	reg2 = reg2 + 1
	if reg2 > reg5:
		print reg0
		break
	reg3 = 1
