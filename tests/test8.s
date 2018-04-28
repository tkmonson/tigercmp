.data
.align 2
.text
tig_main:
sw $fp -8($sp)
move $fp, $sp
addi $sp, $fp -12
sw $ra -4($fp)
li $a2, 1
li $a1, 10
li $a0, 20
bgt $a1, $a0 L1041
L1042:
li $a2, 0
L1041:
li $a0, 0
bne $a2, $a0 L1043
L1044:
li $a0, 40
L1045:
la $a0, L1046
j $a0 
L1043:
li $a0, 30
la $a0, L1045
j $a0 
L1046:
lw $ra -4($fp)
move $sp, $fp
lw $fp -8($sp)
jr $ra
