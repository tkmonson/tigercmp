.data
.align 2
L1119: .word 3
.asciiz "sdf"
L1113: .word 3
.asciiz "sfd"
L1102: .word 4
.asciiz "kati"
L1090: .word 5
.asciiz "Allos"
L1088: .word 5
.asciiz "Kapou"
L1087: .word 7
.asciiz "Kapoios"
L1085: .word 0
.asciiz ""
L1083: .word 9
.asciiz "somewhere"
L1082: .word 5
.asciiz "aname"
.text
tig_main:
sw $fp -12($sp)
move $fp, $sp
addi $sp, $fp -16
sw $ra -8($fp)
la $a2, tig_initArray
li $a0, 1
addi $a0, $a0 10
move $a0, $a0  
li $a1, 0
move $a1, $a1  
jal $a2
move $a0, $v0  
addi $a0, $a0 4
move $a1, $a0  
li $a0, 10
sw $a0, -4($a1)
move $s0, $a1  
li $a0, 1
addi $a0, $a0 5
move $s1, $a0  
la $a1, malloc
li $a0, 16
move $a0, $a0  
jal $a1
move $a1, $v0  
la $a0, L1082
sw $a0, 0($a1)
la $a0, L1083
sw $a0, 4($a1)
li $a0, 0
sw $a0, 8($a1)
li $a0, 0
sw $a0, 12($a1)
la $a2, tig_initArray
move $a0, $s1  
move $a1, $a1  
jal $a2
move $a0, $v0  
addi $a0, $a0 4
move $a1, $a0  
li $a0, 5
sw $a0, -4($a1)
move $s1, $a1  
la $a2, tig_initArray
li $a0, 1
addi $a0, $a0 100
move $a0, $a0  
la $a1, L1085
move $a1, $a1  
jal $a2
move $a0, $v0  
addi $a0, $a0 4
move $a1, $a0  
li $a0, 100
sw $a0, -4($a1)
move $s2, $a1  
la $a1, malloc
li $a0, 16
move $a0, $a0  
jal $a1
move $a1, $v0  
la $a0, L1087
sw $a0, 0($a1)
la $a0, L1088
sw $a0, 4($a1)
li $a0, 2432
sw $a0, 8($a1)
li $a0, 44
sw $a0, 12($a1)
move $s3, $a1  
la $a1, malloc
li $a0, 8
move $a0, $a0  
jal $a1
move $s4, $v0  
la $a0, L1090
sw $a0, 0($s4)
addi $a0, $s4 4
move $s5, $a0  
la $a2, tig_initArray
li $a0, 1
addi $a0, $a0 3
move $a0, $a0  
li $a1, 1900
move $a1, $a1  
jal $a2
move $a0, $v0  
addi $a0, $a0 4
move $a1, $a0  
li $a0, 3
sw $a0, -4($a1)
sw $a1, ($s5)
move $s4, $s4  
li $a1, 0
li $a0, 0
blt $a1, $a0 L1131
L1132:
li $a1, 0
lw $a0, -4($s0)
bge $a1, $a0 L1133
L1134:
li $a1, 1
li $a0, 4
mul $a0, $a0 0
add $a0, $s0 $a0
sw $a1, ($a0)
li $a1, 9
li $a0, 0
blt $a1, $a0 L1136
L1137:
li $a1, 9
lw $a0, -4($s0)
bge $a1, $a0 L1138
L1139:
li $a1, 3
li $a0, 4
mul $a0, $a0 9
add $a0, $s0 $a0
sw $a1, ($a0)
li $a1, 3
li $a0, 0
blt $a1, $a0 L1141
L1142:
li $a1, 3
lw $a0, -4($s1)
bge $a1, $a0 L1143
L1144:
la $a0, L1102
li $a1, 4
mul $a1, $a1 3
add $a1, $s1 $a1
lw $a2, 0($a1)
li $a1, 4
mul $a1, $a1 0
add $a1, $a2 $a1
sw $a0, ($a1)
li $a1, 1
li $a0, 0
blt $a1, $a0 L1146
L1147:
li $a1, 1
lw $a0, -4($s1)
bge $a1, $a0 L1148
L1149:
li $a0, 23
li $a1, 4
mul $a1, $a1 1
add $a1, $s1 $a1
lw $a2, 0($a1)
li $a1, 4
mul $a1, $a1 3
add $a1, $a2 $a1
sw $a0, ($a1)
li $a1, 34
li $a0, 0
blt $a1, $a0 L1151
L1152:
li $a1, 34
lw $a0, -4($s2)
bge $a1, $a0 L1153
L1154:
la $a1, L1113
li $a0, 4
mul $a0, $a0 34
add $a0, $s2 $a0
sw $a1, ($a0)
la $a1, L1119
li $a0, 4
mul $a0, $a0 0
add $a0, $s3 $a0
sw $a1, ($a0)
li $a1, 0
li $a0, 0
blt $a1, $a0 L1156
L1157:
li $a1, 0
li $a0, 4
mul $a0, $a0 1
add $a0, $s4 $a0
lw $a0, 0($a0)
lw $a0, -4($a0)
bge $a1, $a0 L1158
L1159:
li $a2, 2323
li $a0, 4
mul $a0, $a0 1
add $a0, $s4 $a0
lw $a1, 0($a0)
li $a0, 4
mul $a0, $a0 0
add $a0, $a1 $a0
sw $a2, ($a0)
li $a1, 2
li $a0, 0
blt $a1, $a0 L1161
L1162:
li $a1, 2
li $a0, 4
mul $a0, $a0 1
add $a0, $s4 $a0
lw $a0, 0($a0)
lw $a0, -4($a0)
bge $a1, $a0 L1163
L1164:
li $a2, 2323
li $a0, 4
mul $a0, $a0 1
add $a0, $s4 $a0
lw $a1, 0($a0)
li $a0, 4
mul $a0, $a0 2
add $a0, $a1 $a0
sw $a2, ($a0)
la $a0, L1165
j $a0 
L1131:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1132
j $a0 
L1133:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1134
j $a0 
L1136:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1137
j $a0 
L1138:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1139
j $a0 
L1141:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1142
j $a0 
L1143:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1144
j $a0 
L1146:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1147
j $a0 
L1148:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1149
j $a0 
L1151:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1152
j $a0 
L1153:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1154
j $a0 
L1156:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1157
j $a0 
L1158:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1159
j $a0 
L1161:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1162
j $a0 
L1163:
la $a1, tig_exit
li $a0, 1
move $a0, $a0  
jal $a1
la $a0, L1164
j $a0 
L1165:
lw $ra -8($fp)
move $sp, $fp
lw $fp -12($sp)
jr $ra
