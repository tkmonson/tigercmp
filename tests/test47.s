.data
.align 2
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
li $a0, 4
la $a0, L1180
j $a0 
L1180:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
