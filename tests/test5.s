.data
.align 2
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
li $a0, 0
sw $a0, 0($a1)
li $a0, 0
sw $a0, 4($a1)
move $a0, $a1  
la $a0, L1014
j $a0 
L1014:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
