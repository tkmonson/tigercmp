.data
.align 2
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
li $a1, 0
li $a0, 0
beq $a1, $a0 L1173
L1173:
li $a0, 1
li $a0, 0
bne $a1, $a0 L1174
L1175:
li $a0, 0
L1174:
la $a0, L1176
j $a0 
L1176:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
