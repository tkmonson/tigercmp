.data
.align 2
.text
tig_main:
sw $fp -12($sp)
move $fp, $sp
addi $sp, $fp -16
sw $ra -8($fp)
la $a2, tig_initArray
li $a0, 1
addi $a0, $a0 10
move $a0, $a0  
li $a1, 0
move $a1, $a1  
jal $a2
move $a0, $v0  
addi $a0, $a0 4
move $a1, $a0  
li $a0, 10
sw $a0, -4($a1)
move $a0, $a1  
la $a0, L991
j $a0 
L991:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
