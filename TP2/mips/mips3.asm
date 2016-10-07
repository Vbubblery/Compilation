# if 2 != 0 then 3 + 4 else 5 * 6
.text 
main:	
	li $a0,2
	li $a1,0
	beq $a0,$a1,cas1

	li $v0,1
	li $a0,3
	li $a1,4
	add $a0,$a0,$a1
	syscall
	li $v0,10
	syscall

cas1:	li $v0,1
	li $a0,5
	li $a1,6
	mul $a0,$a0,$a1
	syscall
	li $v0,10
	syscall
.data