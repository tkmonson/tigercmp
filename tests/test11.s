.data
.align 2
L442: .word 1
.asciiz " "
.text
tig_main:
sw $fp -12($sp)
move $fp, $sp
addi $sp, $fp -16
sw $ra -8($fp)
li $a0, 10
sw $a0, -4($fp)
lw $a1, -4($fp)
la $a0, L442
blt $a1, $a0 L444
L441:
la $a0, L445
j $a0 
L443:
lw $a0, -4($fp)
addi $a0, $a0 1
sw $a0, -4($fp)
L444:
lw $a0, -4($fp)
addi $a0, $a0 -1
sw $a0, -4($fp)
lw $a1, -4($fp)
la $a0, L442
blt $a1, $a0 L443
L446:
la $a0, L441
j $a0 
L445:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
