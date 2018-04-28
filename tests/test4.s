.data
.align 2
.text
tig_main:
sw $fp -12($sp)
move $fp, $sp
addi $sp, $fp -16
sw $ra -8($fp)
la $a2, L1003
move $a0, $fp  
li $a1, 10
move $a1, $a1  
jal $a2
la $a0, L1009
j $a0 
L1009:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
L1003:
sw $fp -44($sp)
move $fp, $sp
addi $sp, $fp -48
sw $ra -40($fp)
sw $s0, -4($fp)
sw $s1, -8($fp)
sw $s2, -12($fp)
sw $s3, -16($fp)
sw $s4, -20($fp)
sw $s5, -24($fp)
sw $s6, -28($fp)
sw $s7, -32($fp)
sw $a0, 0($fp)
move $a1, $a1  
li $a0, 0
beq $a1, $a0 L1005
L1006:
move $s0, $a1  
la $a2, L1003
move $a0, $fp  
addi $a1, $a1 -1
move $a1, $a1  
jal $a2
move $a0, $v0  
mul $a0, $s0 $a0
move $a1, $a0  
L1007:
move $v0, $a1  
lw $s0, -4($fp)
lw $s1, -8($fp)
lw $s2, -12($fp)
lw $s3, -16($fp)
lw $s4, -20($fp)
lw $s5, -24($fp)
lw $s6, -28($fp)
lw $s7, -32($fp)
la $a0, L1010
j $a0 
L1005:
li $a1, 1
la $a0, L1007
j $a0 
L1010:
lw $ra -40($fp)
move $sp, $fp
lw $fp -44($sp)
jr $ra
