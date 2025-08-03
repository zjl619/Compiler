.text

    .globl fibonacci
fibonacci:
    addi sp, sp, -16
    sw ra, 0(sp)
    sw a0, 4(sp)

    lw a0, 0(sp)
    li a0, 1
slt a0, a0, a0
    xori a0, a0, 1
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    lw a0, 0(sp)
    li a0, 1
sub a0, a0, a0    call fibonacci
    lw a0, 0(sp)
    li a0, 2
sub a0, a0, a0    call fibonacci
add a0, a0, a0

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

    j .Lif_end2
.Lif_then0:
    lw a0, 0(sp)

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

.Lif_end2:
    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret



    .globl gcd
gcd:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    lw a0, 8(sp)
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    j .Lif_end2
.Lif_then0:
    lw a0, 0(sp)

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a0, 8(sp)    lw a0, 0(sp)
    lw a0, 8(sp)
rem a0, a0, a0
    mv a1, a0    call gcd

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl isPrime
isPrime:
    addi sp, sp, -16
    sw ra, 0(sp)
    sw a0, 4(sp)

    lw a0, 0(sp)
    li a0, 1
slt a0, a0, a0
    xori a0, a0, 1
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    j .Lif_end2
.Lif_then0:
    li a0, 0

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

.Lif_end2:
    lw a0, 0(sp)
    li a0, 3
slt a0, a0, a0
    xori a0, a0, 1
    beqz a0, .Lif_else4
    j .Lif_then3
.Lif_else4:
    j .Lif_end5
.Lif_then3:
    li a0, 1

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

.Lif_end5:
    lw a0, 0(sp)
    li a0, 2
rem a0, a0, a0
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    lw a0, 0(sp)
    li a0, 3
rem a0, a0, a0
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
or a0, a0, a0
    beqz a0, .Lif_else7
    j .Lif_then6
.Lif_else7:
    j .Lif_end8
.Lif_then6:
    li a0, 0

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

.Lif_end8:
    li a0, 5
    sw a0, 16(sp)
.Lloop_begin9:    lw a0, 16(sp)
    lw a0, 16(sp)
mul a0, a0, a0
    lw a0, 0(sp)
slt a0, a0, a0
    xori a0, a0, 1
    beqz a0, .Lloop_end10
    lw a0, 0(sp)
    lw a0, 16(sp)
rem a0, a0, a0
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    lw a0, 0(sp)
    lw a0, 16(sp)
    li a0, 2
add a0, a0, a0
rem a0, a0, a0
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
or a0, a0, a0
    beqz a0, .Lif_else12
    j .Lif_then11
.Lif_else12:
    j .Lif_end13
.Lif_then11:
    li a0, 0

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 20
    ret

.Lif_end13:
    lw a0, 16(sp)
    li a0, 6
add a0, a0, a0
    sw a0, 16(sp)
    j .Lloop_begin9
.Lloop_end10:
    li a0, 1

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 20
    ret

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret



    .globl factorial
factorial:
    addi sp, sp, -16
    sw ra, 0(sp)
    sw a0, 4(sp)

    li a0, 1
    sw a0, 16(sp)
.Lloop_begin0:    lw a0, 0(sp)
    li a0, 0
slt a0, a0, a0
    beqz a0, .Lloop_end1
    lw a0, 16(sp)
    lw a0, 0(sp)
mul a0, a0, a0
    sw a0, 16(sp)
    lw a0, 0(sp)
    li a0, 1
sub a0, a0, a0
    sw a0, 0(sp)
    j .Lloop_begin0
.Lloop_end1:
    lw a0, 16(sp)

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 20
    ret

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret



    .globl combination
combination:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    lw a0, 8(sp)
    lw a0, 0(sp)
slt a0, a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    j .Lif_end2
.Lif_then0:
    li a0, 0

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a0, 8(sp)
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    lw a0, 8(sp)
    lw a0, 0(sp)
sub a0, a0, a0
    seqz a0, a0
or a0, a0, a0
    beqz a0, .Lif_else4
    j .Lif_then3
.Lif_else4:
    j .Lif_end5
.Lif_then3:
    li a0, 1

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end5:
    lw a0, 0(sp)    call factorial
    lw a0, 8(sp)    call factorial
    lw a0, 0(sp)
    lw a0, 8(sp)
sub a0, a0, a0    call factorial
mul a0, a0, a0
div a0, a0, a0

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl power
power:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    li a0, 1
    sw a0, 32(sp)
.Lloop_begin0:    lw a0, 8(sp)
    li a0, 0
slt a0, a0, a0
    beqz a0, .Lloop_end1
    lw a0, 8(sp)
    li a0, 2
rem a0, a0, a0
    li a0, 1
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else3
    j .Lif_then2
.Lif_else3:
    j .Lif_end4
.Lif_then2:
    lw a0, 32(sp)
    lw a0, 0(sp)
mul a0, a0, a0
    sw a0, 32(sp)
.Lif_end4:
    lw a0, 0(sp)
    lw a0, 0(sp)
mul a0, a0, a0
    sw a0, 0(sp)
    lw a0, 8(sp)
    li a0, 2
div a0, a0, a0
    sw a0, 8(sp)
    j .Lloop_begin0
.Lloop_end1:
    lw a0, 32(sp)

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 36
    ret

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl complexFunction
complexFunction:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a2, 4(sp)
    sw a1, 8(sp)
    sw a0, 12(sp)

    li a0, 0
    sw a0, 32(sp)
    lw a0, 0(sp)
    lw a0, 8(sp)
slt a0, a0, a0
    lw a0, 8(sp)
    lw a0, 16(sp)
slt a0, a0, a0
and a0, a0, a0
seqz a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:    lw a0, 0(sp)
    lw a0, 16(sp)
slt a0, a0, a0
seqz a0, a0
    lw a0, 16(sp)
    lw a0, 8(sp)
slt a0, a0, a0
or a0, a0, a0
    beqz a0, .Lif_else4
    j .Lif_then3
.Lif_else4:    lw a0, 8(sp)
    lw a0, 0(sp)
slt a0, a0, a0
    xori a0, a0, 1
    lw a0, 0(sp)
    lw a0, 16(sp)
slt a0, a0, a0
    xori a0, a0, 1
and a0, a0, a0
    lw a0, 8(sp)
    lw a0, 16(sp)
slt a0, a0, a0
    xori a0, a0, 1
or a0, a0, a0
    beqz a0, .Lif_else7
    j .Lif_then6
.Lif_else7:    lw a0, 8(sp)
    lw a0, 16(sp)
slt a0, a0, a0
    lw a0, 16(sp)
    lw a0, 0(sp)
slt a0, a0, a0
    lw a0, 0(sp)
    lw a0, 8(sp)
slt a0, a0, a0
    xori a0, a0, 1
and a0, a0, a0
or a0, a0, a0
    beqz a0, .Lif_else10
    j .Lif_then9
.Lif_else10:    lw a0, 16(sp)
    lw a0, 0(sp)
slt a0, a0, a0
    lw a0, 8(sp)
    lw a0, 0(sp)
sub a0, a0, a0
    snez a0, a0
or a0, a0, a0
seqz a0, a0
    lw a0, 0(sp)
    lw a0, 8(sp)
sub a0, a0, a0
    seqz a0, a0
and a0, a0, a0
    beqz a0, .Lif_else13
    j .Lif_then12
.Lif_else13:
    lw a0, 16(sp)
    lw a0, 8(sp)
    lw a0, 0(sp)
neg a0, a0
sub a0, a0, a0
    li a0, 6
neg a0, a0
sub a0, a0, a0
mul a0, a0, a0
    sw a0, 32(sp)
    j .Lif_end14
.Lif_then12:
    lw a0, 16(sp)
    lw a0, 0(sp)
mul a0, a0, a0
    lw a0, 8(sp)
    li a0, 5
neg a0, a0
add a0, a0, a0
mv a0, a0
sub a0, a0, a0
    sw a0, 32(sp)
.Lif_end14:
    j .Lif_end11
.Lif_then9:
    lw a0, 8(sp)
    lw a0, 16(sp)
    lw a0, 0(sp)
mv a0, a0
sub a0, a0, a0
    li a0, 4
neg a0, a0
sub a0, a0, a0
mul a0, a0, a0
    sw a0, 32(sp)
.Lif_end11:
    j .Lif_end8
.Lif_then6:
    lw a0, 8(sp)
    lw a0, 0(sp)
mul a0, a0, a0
    lw a0, 16(sp)
    li a0, 3
neg a0, a0
add a0, a0, a0
neg a0, a0
sub a0, a0, a0
    sw a0, 32(sp)
.Lif_end8:
    j .Lif_end5
.Lif_then3:
    lw a0, 0(sp)
    lw a0, 16(sp)
    lw a0, 8(sp)
mv a0, a0
sub a0, a0, a0
    li a0, 2
neg a0, a0
sub a0, a0, a0
mul a0, a0, a0
    sw a0, 32(sp)
.Lif_end5:
    j .Lif_end2
.Lif_then0:
    lw a0, 0(sp)
    lw a0, 8(sp)
mul a0, a0, a0
    lw a0, 16(sp)
    li a0, 1
neg a0, a0
add a0, a0, a0
neg a0, a0
sub a0, a0, a0
    sw a0, 32(sp)
.Lif_end2:
    li a0, 0
    sw a0, 36(sp)
.Lloop_begin15:    lw a0, 36(sp)
    li a0, 10
slt a0, a0, a0
    beqz a0, .Lloop_end16
    lw a0, 36(sp)
    li a0, 1
add a0, a0, a0
    sw a0, 36(sp)
    lw a0, 36(sp)
    li a0, 3
rem a0, a0, a0
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else18
    j .Lif_then17
.Lif_else18:    lw a0, 36(sp)
    li a0, 3
rem a0, a0, a0
    li a0, 1
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else21
    j .Lif_then20
.Lif_else21:
    lw a0, 32(sp)
    li a0, 2
mul a0, a0, a0
    sw a0, 32(sp)
    lw a0, 32(sp)
    li a0, 50
slt a0, a0, a0
    beqz a0, .Lif_else24
    j .Lif_then23
.Lif_else24:
    j .Lif_end25
.Lif_then23:
    j .Lloop_begin15
.Lif_end25:
    lw a0, 32(sp)
    li a0, 1
add a0, a0, a0
    sw a0, 32(sp)
    lw a0, 32(sp)
    li a0, 100
slt a0, a0, a0
    beqz a0, .Lif_else27
    j .Lif_then26
.Lif_else27:
    j .Lif_end28
.Lif_then26:
    j .Lloop_end16
.Lif_end28:
    j .Lif_end22
.Lif_then20:
    lw a0, 32(sp)
    lw a0, 36(sp)
sub a0, a0, a0
    sw a0, 32(sp)
.Lif_end22:
    j .Lif_end19
.Lif_then17:
    lw a0, 32(sp)
    lw a0, 36(sp)
add a0, a0, a0
    sw a0, 32(sp)
.Lif_end19:
    j .Lloop_begin15
.Lloop_end16:
    lw a0, 32(sp)

    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 40
    ret

    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl shortCircuit
shortCircuit:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    li a0, 0
    sw a0, 32(sp)
    lw a0, 0(sp)
    li a0, 0
slt a0, a0, a0
    lw a0, 8(sp)
    lw a0, 0(sp)
div a0, a0, a0
    li a0, 2
slt a0, a0, a0
and a0, a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    j .Lif_end2
.Lif_then0:
    lw a0, 32(sp)
    li a0, 12
add a0, a0, a0
    sw a0, 32(sp)
.Lif_end2:
    lw a0, 0(sp)
    li a0, 0
slt a0, a0, a0
    lw a0, 8(sp)
    lw a0, 0(sp)
    lw a0, 0(sp)
sub a0, a0, a0
    li a0, 1
add a0, a0, a0
div a0, a0, a0
    li a0, 0
slt a0, a0, a0
or a0, a0, a0
    beqz a0, .Lif_else4
    j .Lif_then3
.Lif_else4:
    j .Lif_end5
.Lif_then3:
    lw a0, 32(sp)
    li a0, 30
add a0, a0, a0
    sw a0, 32(sp)
.Lif_end5:
    lw a0, 32(sp)

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 36
    ret

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl nestedLoopsAndConditions
nestedLoopsAndConditions:
    addi sp, sp, -16
    sw ra, 0(sp)
    sw a0, 4(sp)

    li a0, 0
    sw a0, 16(sp)
    li a0, 0
    sw a0, 20(sp)
.Lloop_begin0:    lw a0, 20(sp)
    lw a0, 0(sp)
slt a0, a0, a0
    beqz a0, .Lloop_end1
    li a0, 0
    sw a0, 24(sp)
.Lloop_begin2:    lw a0, 24(sp)
    lw a0, 20(sp)
slt a0, a0, a0
    beqz a0, .Lloop_end3
    lw a0, 20(sp)
    lw a0, 24(sp)
add a0, a0, a0
    li a0, 2
rem a0, a0, a0
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else5
    j .Lif_then4
.Lif_else5:
    lw a0, 16(sp)
    lw a0, 20(sp)
    lw a0, 24(sp)
mul a0, a0, a0
add a0, a0, a0
    sw a0, 16(sp)
    lw a0, 16(sp)
    li a0, 0
slt a0, a0, a0
    beqz a0, .Lif_else8
    j .Lif_then7
.Lif_else8:
    j .Lif_end9
.Lif_then7:
    li a0, 0
    sw a0, 16(sp)
    j .Lloop_begin2
.Lif_end9:
    j .Lif_end6
.Lif_then4:
    lw a0, 16(sp)
    lw a0, 20(sp)
    lw a0, 24(sp)
mul a0, a0, a0
sub a0, a0, a0
    sw a0, 16(sp)
.Lif_end6:
    lw a0, 16(sp)
    li a0, 1053
slt a0, a0, a0
    beqz a0, .Lif_else11
    j .Lif_then10
.Lif_else11:
    j .Lif_end12
.Lif_then10:
    j .Lloop_end3
.Lif_end12:
    lw a0, 24(sp)
    li a0, 1
add a0, a0, a0
    sw a0, 24(sp)
    j .Lloop_begin2
.Lloop_end3:
    lw a0, 16(sp)
    li a0, 913
slt a0, a0, a0
    beqz a0, .Lif_else5
    j .Lif_then4
.Lif_else5:
    j .Lif_end6
.Lif_then4:
    j .Lloop_end1
.Lif_end6:
    lw a0, 20(sp)
    li a0, 1
add a0, a0, a0
    sw a0, 20(sp)
    j .Lloop_begin0
.Lloop_end1:
    lw a0, 16(sp)

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 24
    ret

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret



    .globl func1
func1:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a2, 4(sp)
    sw a1, 8(sp)
    sw a0, 12(sp)

    lw a0, 16(sp)
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    lw a0, 0(sp)    lw a0, 8(sp)
    lw a0, 16(sp)
sub a0, a0, a0
    mv a1, a0    li a0, 0
    mv a2, a0    call func1

    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    j .Lif_end2
.Lif_then0:
    lw a0, 0(sp)
mv a0, a0
    lw a0, 8(sp)
mul a0, a0, a0

    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl func2
func2:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    lw a0, 8(sp)
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    lw a0, 0(sp)

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    j .Lif_end2
.Lif_then0:
    lw a0, 0(sp)
    lw a0, 8(sp)
mv a0, a0
rem a0, a0, a0    li a0, 0
    mv a1, a0    call func2

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl func3
func3:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    lw a0, 8(sp)
    li a0, 0
sub a0, a0, a0
    seqz a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    lw a0, 0(sp)
    lw a0, 8(sp)
add a0, a0, a0    li a0, 0
    mv a1, a0    call func3

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    j .Lif_end2
.Lif_then0:
    lw a0, 0(sp)
    li a0, 1
add a0, a0, a0

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl func4
func4:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a2, 4(sp)
    sw a1, 8(sp)
    sw a0, 12(sp)

    lw a0, 0(sp)
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    lw a0, 16(sp)

    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    j .Lif_end2
.Lif_then0:
    lw a0, 8(sp)

    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a2, 4(sp)
    lw a1, 8(sp)
    lw a0, 12(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl func5
func5:
    addi sp, sp, -16
    sw ra, 0(sp)
    sw a0, 4(sp)

    lw a0, 0(sp)
neg a0, a0

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret



    .globl func6
func6:
    addi sp, sp, -32
    sw ra, 0(sp)
    sw a1, 4(sp)
    sw a0, 8(sp)

    lw a0, 0(sp)
    lw a0, 8(sp)
and a0, a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    li a0, 0

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

    j .Lif_end2
.Lif_then0:
    li a0, 1

    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret

.Lif_end2:
    lw a1, 4(sp)
    lw a0, 8(sp)
    lw ra, 0(sp)
    addi sp, sp, 32
    ret



    .globl func7
func7:
    addi sp, sp, -16
    sw ra, 0(sp)
    sw a0, 4(sp)

    lw a0, 0(sp)
seqz a0, a0
    beqz a0, .Lif_else1
    j .Lif_then0
.Lif_else1:
    li a0, 0

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

    j .Lif_end2
.Lif_then0:
    li a0, 1

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret

.Lif_end2:
    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 16
    ret



    .globl nestedCalls
nestedCalls:
    addi sp, sp, -80
    sw ra, 0(sp)
    sw a7, 4(sp)
    sw a6, 8(sp)
    sw a5, 12(sp)
    sw a4, 16(sp)
    sw a3, 20(sp)
    sw a2, 24(sp)
    sw a1, 28(sp)
    sw a0, 32(sp)

    li a0, 2
    sw a0, 80(sp)
    li a0, 8
    sw a0, 84(sp)
    li a0, 8
    sw a0, 88(sp)
    li a0, 9
    sw a0, 92(sp)
    lw a0, 80(sp)    call func7    lw a0, 84(sp)    call func5
    mv a1, a0    call func6    lw a0, 88(sp)
    mv a1, a0    call func2    lw a0, 92(sp)
    mv a1, a0    call func3    call func5    lw a0, 0(sp)
    mv a1, a0    lw a0, 8(sp)    call func5    lw a0, 16(sp)    lw a0, 24(sp)    call func7
    mv a1, a0    call func6
    mv a1, a0    lw a0, 32(sp)    lw a0, 40(sp)    call func7
    mv a1, a0    call func2
    mv a2, a0    call func4    lw a0, 48(sp)
    mv a1, a0    call func3    lw a0, 56(sp)
    mv a1, a0    call func2    lw a0, 64(sp)    lw a0, 68(sp)    call func7
    mv a1, a0    call func3
    mv a1, a0    lw a0, 80(sp)
    mv a2, a0    call func1
    mv a2, a0    call func4    lw a0, 84(sp)    lw a0, 88(sp)    call func7    lw a0, 92(sp)
    mv a1, a0    call func3
    mv a1, a0    call func2
    mv a1, a0    call func3    lw a0, 0(sp)
    mv a1, a0    lw a0, 8(sp)
    mv a2, a0    call func1    lw a0, 16(sp)
    mv a1, a0    call func2    lw a0, 24(sp)
    mv a1, a0    lw a0, 32(sp)    lw a0, 40(sp)    call func5
    mv a1, a0    call func3    lw a0, 48(sp)    call func5
    mv a1, a0    call func2    lw a0, 56(sp)
    mv a1, a0    lw a0, 64(sp)    call func7
    mv a2, a0    call func1    lw a0, 68(sp)    call func5
    mv a1, a0    call func2    lw a0, 80(sp)
    mv a1, a0    call func3
    mv a2, a0    call func1
    sw a0, 96(sp)
    lw a0, 96(sp)

    lw a7, 4(sp)
    lw a6, 8(sp)
    lw a5, 12(sp)
    lw a4, 16(sp)
    lw a3, 20(sp)
    lw a2, 24(sp)
    lw a1, 28(sp)
    lw a0, 32(sp)
    lw ra, 0(sp)
    addi sp, sp, 100
    ret

    lw a7, 4(sp)
    lw a6, 8(sp)
    lw a5, 12(sp)
    lw a4, 16(sp)
    lw a3, 20(sp)
    lw a2, 24(sp)
    lw a1, 28(sp)
    lw a0, 32(sp)
    lw ra, 0(sp)
    addi sp, sp, 80
    ret



    .globl main
main:
    addi sp, sp, -16
    sw ra, 0(sp)


    li a0, 0
    sw a0, 16(sp)
    li a0, 12    call fibonacci
    sw a0, 20(sp)
    li a0, 22    li a0, 15
    mv a1, a0    call gcd
    sw a0, 24(sp)
    li a0, 17    call isPrime
    sw a0, 28(sp)
    li a0, 8    call factorial
    sw a0, 32(sp)
    li a0, 7    li a0, 3
    mv a1, a0    call combination
    sw a0, 36(sp)
    li a0, 3    li a0, 11
    mv a1, a0    call power
    sw a0, 40(sp)
    li a0, 3    li a0, 5
    mv a1, a0    li a0, 1
    mv a2, a0    call complexFunction
    sw a0, 44(sp)
    li a0, 5
neg a0, a0    li a0, 10
    mv a1, a0    call shortCircuit
    sw a0, 48(sp)
    li a0, 10    call nestedLoopsAndConditions
    sw a0, 52(sp)
    addi sp, sp, -16
    li a0, 1    li a0, 2
    mv a1, a0    li a0, 3
    mv a2, a0    li a0, 4
    mv a3, a0    li a0, 5
    mv a4, a0    li a0, 6
    mv a5, a0    li a0, 7
    mv a6, a0    li a0, 8
    mv a7, a0    li a0, 9
    sw a0, 0(sp)    li a0, 10
    sw a0, 4(sp)    call nestedCalls
    addi sp, sp, 16
    sw a0, 56(sp)
    lw a0, 20(sp)
    lw a0, 24(sp)
add a0, a0, a0
    lw a0, 28(sp)
add a0, a0, a0
    lw a0, 32(sp)
add a0, a0, a0
    lw a0, 36(sp)
sub a0, a0, a0
    lw a0, 40(sp)
add a0, a0, a0
    lw a0, 52(sp)
sub a0, a0, a0
    li a0, 256
rem a0, a0, a0
    sw a0, 16(sp)
    lw a0, 16(sp)

    lw a0, 4(sp)
    lw ra, 0(sp)
    addi sp, sp, 64
    ret


    lw ra, 0(sp)
    addi sp, sp, 16
    ret
