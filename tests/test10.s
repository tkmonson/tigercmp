.data
.align 2
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
L438:
li $a2, 1
li $a1, 10
li $a0, 5
bgt $a1, $a0 L435
L436:
li $a2, 0
L435:
li $a0, 0
bne $a2, $a0 L437
L434:
la $a0, L439
j $a0 
L437:
li $a0, 5
addi $a0, $a0 6
la $a0, L438
j $a0 
L439:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
