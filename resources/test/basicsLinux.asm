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

_n_1_num2	dd 3
s0		db "Basics.txt:",0x0d,0x0a,0
stringPrinter	db "%s",0
numberPrinter	db "%d",0x0d,0x0a,0
int_format	db "%i", 0
s1		db "Enter a number:",0x0d,0x0a,0
s2		db "The number is:",0x0d,0x0a,0

;-----------------------------
; uninitialized data 
;-----------------------------
section .bss

_n_0_num1	resd	1	; an int
_n_2_num3	resd	1	; an int
myNum		resd	1	;the int we are reading in

;-----------------------------
; Code! (execution starts at main 
;-----------------------------
section .code
	main:

	mov	edi,	DWORD[_n_1_num2]		; move result into temp reg
	mov	DWORD[_n_2_num3],	edi

	mov	edi,	DWORD[_n_2_num3]		; move op1 into dest reg
	add	edi,	10				; op1 = op1 + op2
	mov	DWORD[_n_2_num3],	edi		; Finish the assignment.

	mov	edi,	DWORD[_n_2_num3]		; move op1 into dest reg
	add	edi,	DWORD[_n_1_num2]		; op1 = op1 + op2
	mov	DWORD[_n_1_num2],	edi		; Finish the assignment.

	mov	edi,	2				; move op1 into dest reg
	imul	edi,	5				; op1 = op1 * op2
	mov	DWORD[_n_0_num1],	edi		; Finish the assignment.

	mov	edi,	DWORD[_n_2_num3]		; move op1 into dest reg
	imul	edi,	DWORD[_n_1_num2]		; op1 = op1 * op2
	mov	DWORD[_n_0_num1],	edi		; Finish the assignment.
	
	mov	edi,	8				; move op1 into dest reg
	sub	edi,	5				; op1 = op1 - op2
	mov	DWORD[_n_1_num2],	edi		; Finish the assignment.
	
	mov	edi,	8				; move op1 into dest reg
	sub	edi,	5				; op1 = op1 - op2
	mov	DWORD[_n_1_num2],	edi		; Finish the assignment.

	; exp loop initialization
	xor	edi,	edi				; clear out the counter
	mov	eax,	0x00000001
	_exp_top_0:
	cmp	edi,	6
	jz	_exp_out_0				; jump out if done
	imul	eax,	8				; result --> eax
	inc	edi
	jmp	_exp_top_0
	; result of exponentiation is in eax
	_exp_out_0:
	mov	DWORD[_n_2_num3],	eax		; Finish the assignment.
	
	push	s0
	push	stringPrinter
	call	printf
	add	esp,	0x04				; clear the arg off the stack
	
	push	DWORD[_n_0_num1]
	push	numberPrinter
	call	printf
	add	esp,	0x04				; clear the arg off the stack
	
	push	DWORD[_n_1_num2]
	push	numberPrinter
	call	printf
	add	esp,	0x04				; clear the arg off the stack

	push	DWORD[_n_2_num3]
	push	numberPrinter
		call	printf
	add	esp,	0x04				; clear the arg off the stack

	push	s1
	push	stringPrinter
		call	printf
	add	esp,	0x04				; clear the arg off the stack

							;here starts the code from asm_io.asm
	;enter	4,0
	;pusha
	;pushf
	lea	eax, [myNum]
	;lea	eax, [ebp-4]
	push	eax
	push	dword int_format
	call	scanf
	;pop	ecx
	;pop	ecx
	add	esp,	0x04				; clear the arg off the stack
	;popf
	;popa
	;mov	eax, [ebp-4]
	;mov	DWORD[myNum],	eax
	;leave

	push	s2
	push	stringPrinter
		call	printf
	add	esp,	0x04				; clear the arg off the stack

	push	DWORD[myNum]
	push	numberPrinter
		call	printf
	add	esp,	0x04				; clear the arg off the stack

	my_exit:

	; All done.
	mov	eax, 0x0
	;call	ExitProcess@4
	call	exit;
	
	; (eof)
