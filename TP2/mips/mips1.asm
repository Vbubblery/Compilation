# true && false

.text
main:	
	li $a0,1
	li $a1,2
	beq $a0,$a1,cas1

	li $v0,1
	li $a0,4
	syscall
	li $v0,10
	syscall


	
cas1:	li $v0,1
	li $a0,3
	syscall
	li $v0,10
	syscall
.data
