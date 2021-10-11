; You can build this on Linux with:
; nasm -f elf64 basiclinux64.asm
; gcc basiclinux.o
; And then run with: ./a.out

;-----------------------------
; exports
;-----------------------------
GLOBAL main

;-----------------------------
; imports
;-----------------------------
extern printf
extern scanf
extern exit

;-----------------------------
; initialized data
;-----------------------------
section .data

value3          dq 3
s0              db "Basics.txt:",0x0d,0x0a,0
stringPrinter   db "%s",0
numberPrinter   db "%d",0x0d,0x0a,0
int_format      db "%i", 0
s1              db "Enter a number:",0x0d,0x0a,0
s2              db "The number is:",0x0d,0x0a,0

;-----------------------------
; uninitialized data
;-----------------------------
section .bss

addResult       resd    1       ; an int
mulResult       resd    1       ; an int
subResult       resd    1       ; an int
expResult       resd    1       ; an int
myNum           resq    1       ;the int we are reading in

;-----------------------------
; Code! (execution starts at main
;-----------------------------
section .text
printInt:
        ; We need to call printf, but we are using rax, rbx, and rcx.  printf
        ; may destroy rax and rcx so we will save these before the call and
        ; restore them afterwards.
        push    rbp                     ; Avoid stack alignment isses
        push    rax                     ; save rax and rcx
        push    rcx

        mov     rdi, numberPrinter      ; set printf format parameter
        mov     rsi, rax                ; set printf value paramete
        xor     rax, rax                ; set rax to 0 (number of float/vector regs used is 0)

        call    [rel printf wrt ..got]
        pop     rcx                     ; restore rcx
        pop     rax                     ; restore rax
        pop     rbp                     ; Avoid stack alignment issues
        ret

printString:
        ; We need to call printf, but we are using rax, rbx, and rcx.  printf
        ; may destroy rax and rcx so we will save these before the call and
        ; restore them afterwards.
        push    rbp                     ; Avoid stack alignment issues
        push    rax                     ; save rax and rcx
        push    rcx

        mov     rdi, stringPrinter      ; set printf format parameter
        mov     rsi, rax                ; set printf value paramete
        xor     rax, rax                ; set rax to 0 (number of float/vector regs used is 0)

        call    [rel printf wrt ..got]
        pop     rcx                     ; restore rcx
        pop     rax                     ; restore rax
        pop     rbp                     ; Avoid stack alignment issues
        ret


main:

        ; Run an add (3 + 10)
        mov     rax, [qword value3]                    ; 64 bit value loading of data values only through rax
        add     rax, 10
        mov     [qword addResult], rax
        call    printInt

        ; Run a multiply (3 * 10)
        mov     rax, [qword value3]
        imul    rax, 10
        mov     [qword mulResult], rax
        call    printInt

        ; Run a subtract (3 - 10)
        mov     rax, [qword value3]
        sub     rax, 10
        mov     [qword subResult], rax
        call    printInt

        ; Run an exponent loop (10^3)
        xor     rdi, rdi                               ; clear out the counter
        mov     rcx, 1
        mov     rax, [qword value3]                    ; Computing 10^3
        mov     rdx, rax
exp_start:
        cmp     rdi, rdx
        jz      exp_done                              ; jump out if done
        imul    rax, 10
        inc     rdi
        jmp     exp_start
        ; result of 10^3 is in rax
exp_done:
        call    printInt
        mov     [qword expResult], rax

        ; Get an int from the user
        ; mov     rax, s1
        ; call    printString
        ; mov     rdi, int_format
        ; mov     rsi, myNum
        ; xor     rax, rax
        ; push    rbp
        ; call    [rel scanf wrt ..got]
        ; pop     rbp
        ; mov     rax, [qword myNum]
        ; call    printInt


my_exit:

        ; All done.
        mov       rax, 60                 ; system call for exit
        xor       rdi, rdi                ; Put an exit code of 0 into the rdi register
        syscall                           ; invoke operating system to exit
