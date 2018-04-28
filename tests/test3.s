.data
.align 2
L1000: .word 8
.asciiz "Somebody"
L998: .word 6
.asciiz "Nobody"
.text
tig_main:
sw $fp -12($sp)
move $fp, $sp
addi $sp, $fp -16
sw $ra -8($fp)
la $a1, malloc
li $a0, 8
move $a0, $a0  
jal $a1
move $a1, $v0  
la $a0, L998
sw $a0, 0($a1)
li $a0, 1000
sw $a0, 4($a1)
move $a2, $a1  
la $a1, L1000
li $a0, 4
mul $a0, $a0 0
add $a0, $a2 $a0
sw $a1, ($a0)
la $a0, L1001
j $a0 
L1001:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
