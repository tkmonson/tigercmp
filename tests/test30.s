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
move $s0, $a1  
li $a1, 2
li $a0, 0
blt $a1, $a0 L1063
L1064:
li $a1, 2
lw $a0, -4($s0)
bge $a1, $a0 L1065
L1066:
li $a0, 4
mul $a0, $a0 2
add $a0, $s0 $a0
lw $a0, 0($a0)
la $a0, L1067
j $a0 
L1063:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1064
j $a0 
L1065:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1066
j $a0 
L1067:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
