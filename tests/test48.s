.data
.align 2
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
la $a0, L1187
j $a0 
L1187:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
L1184:
sw $fp -40($sp)
move $fp, $sp
addi $sp, $fp -44
sw $ra -36($fp)
sw $s0, -4($fp)
sw $s1, -8($fp)
sw $s2, -12($fp)
sw $s3, -16($fp)
sw $s4, -20($fp)
sw $s5, -24($fp)
sw $s6, -28($fp)
sw $s7, -32($fp)
sw $a0, 0($fp)
move $a0, $a1  
move $v0, $a0  
lw $s0, -4($fp)
lw $s1, -8($fp)
lw $s2, -12($fp)
lw $s3, -16($fp)
lw $s4, -20($fp)
lw $s5, -24($fp)
lw $s6, -28($fp)
lw $s7, -32($fp)
la $a0, L1188
j $a0 
L1188:
lw $ra -36($fp)
move $sp, $fp
lw $fp -40($sp)
jr $ra
L1182:
sw $fp -40($sp)
move $fp, $sp
addi $sp, $fp -44
sw $ra -36($fp)
sw $s0, -4($fp)
sw $s1, -8($fp)
sw $s2, -12($fp)
sw $s3, -16($fp)
sw $s4, -20($fp)
sw $s5, -24($fp)
sw $s6, -28($fp)
sw $s7, -32($fp)
sw $a0, 0($fp)
move $a0, $a1  
move $v0, $a0  
lw $s0, -4($fp)
lw $s1, -8($fp)
lw $s2, -12($fp)
lw $s3, -16($fp)
lw $s4, -20($fp)
lw $s5, -24($fp)
lw $s6, -28($fp)
lw $s7, -32($fp)
la $a0, L1189
j $a0 
L1189:
lw $ra -36($fp)
move $sp, $fp
lw $fp -40($sp)
jr $ra
