.data
.align 2
L448: .word 2
.asciiz "df"
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
li $a1, 3
la $a0, L448
bgt $a1, $a0 L449
L449:
la $a0, L450
j $a0 
L450:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
