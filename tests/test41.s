.data
.align 2
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
la $a0, L1078
j $a0 
L1078:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
