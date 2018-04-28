.data
.align 2
L1034: .word 4
.asciiz "str2"
L1032: .word 1
.asciiz " "
L1031: .word 3
.asciiz "str"
.text
tig_main:
sw $fp -12($sp)
move $fp, $sp
addi $sp, $fp -16
sw $ra -8($fp)
la $a3, L1027
move $a0, $fp  
li $a1, 0
move $a1, $a1  
la $a2, L1034
move $a2, $a2  
jal $a3
la $a0, L1035
j $a0 
L1035:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
L1028:
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
la $a3, L1027
move $a0, $fp  
move $a1, $a1  
la $a2, L1031
move $a2, $a2  
jal $a3
la $v0, L1032
lw $s0, -4($fp)
lw $s1, -8($fp)
lw $s2, -12($fp)
lw $s3, -16($fp)
lw $s4, -20($fp)
lw $s5, -24($fp)
lw $s6, -28($fp)
lw $s7, -32($fp)
la $a0, L1036
j $a0 
L1036:
lw $ra -40($fp)
move $sp, $fp
lw $fp -44($sp)
jr $ra
L1027:
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
move $a0, $a2  
la $a2, L1028
move $a0, $fp  
addi $a1, $a1 1
move $a1, $a1  
jal $a2
li $v0, 0
lw $s0, -4($fp)
lw $s1, -8($fp)
lw $s2, -12($fp)
lw $s3, -16($fp)
lw $s4, -20($fp)
lw $s5, -24($fp)
lw $s6, -28($fp)
lw $s7, -32($fp)
la $a0, L1037
j $a0 
L1037:
lw $ra -40($fp)
move $sp, $fp
lw $fp -44($sp)
jr $ra
