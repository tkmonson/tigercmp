.data
.align 2
L1071: .word 1
.asciiz " "
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
li $a0, 0
la $a0, L1071
la $a0, L1073
j $a0 
L1073:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
