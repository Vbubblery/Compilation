# if 1 == 2 then 3 else 4

.text 
 main:
 	li      $v0, 4    # code de print_string
	la      $a0, true   # adresse de la chaîne
	syscall           # appel systeme
	
	la 	$a0,false
	syscall
	
	li      $v0, 10   # exit
	syscall 

.data 
true:   .asciiz "true\n"
false:  .asciiz "false\n"