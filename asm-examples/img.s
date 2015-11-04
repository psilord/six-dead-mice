/*
Set up a non-preemptive echo server over serial port.
*/

.equ EXEC_START,			0x402F0400 @ Where Execution will start.
.equ STACK_START,			0x4030CDFC @ Where Stack will start.

.equ CM_PER_GPIO1_CLKCTRL,	0x44e000AC
.equ GPIO1_OE,				0x4804C134
.equ GPIO1_SETDATAOUT,		0x4804C194
.equ CONF_UART0_RXD,		0x44E10970
.equ CONF_UART0_TXD,		0x44E10974
.equ CM_WKUP_CLKSTCTRL,		0x44E00400
.equ CM_PER_L4HS_CLKSTCTRL,	0x44E0011C
.equ CM_WKUP_UART0_CLKCTRL,	0x44E004B4
.equ CM_PER_UART0_CLKCTRL,	0x44E0006C
.equ UART0_SYSC,			0x44E09054
.equ UART0_SYSS,			0x44E09058
.equ UART0_BASE,			0x44E09000

.arm

# Used for storing ALL user registers, not just the normal ones for
# subroutines.
.macro	m_all_regs_store
	# First, immediately save *all* of the registers.
	push 	{r0-r12,r14}
	# Then save pc...
	mov		r0, pc 
	push	{r0}
	# Then save apsr...
	mrs		r0, apsr
	push	{r0}
	# Then save sp...
	mov		r0, sp
	push	{r0}
	# undo my machinations on r0 by retrieving r0 from the stack.
	# 0 is sp, 1 is apsr, 2 is pc, 3 is r0, ..., 14 is r12, 15 is r14
	ldr		r0, [sp, #(3 * 4)]
.endm

# Used for restoring ALL user registers, not just the normal ones for
# subroutines.
.macro m_all_regs_restore
	# Restore sp....
	pop		{r0}
	mov		sp, r0
	# Restore apsr (but throw away the value)
	pop		{r0}
	# Restore pc....
	pop		{r0}
	# but DON'T restore the PC to actually be the PC; we throw away its value

	# ....then we get the rest of the registers normally
	pop 	{r0-r12,r14}
.endm




_start:
	# first initialize all registers (except r15/PC) to zero.
	eors	r0, r0, r0
	eors	r1, r1, r1
	eors	r2, r2, r2
	eors	r3, r3, r3
	eors	r4, r4, r4
	eors	r5, r5, r5
	eors	r6, r6, r6
	eors	r7, r7, r7
	eors	r8, r8, r8
	eors	r9, r9, r9
	eors	r10, r10, r10
	eors	r11, r11, r11
	eors	r12, r12, r12
	eors	r13, r13, r13
	eors	r14, r14, r14

	# Now we start.

	# set up a small stack for function calls

	# setup stack pointer to the MAX 6 kB stack
	ldr		sp, =STACK_START

	# set clock for GPIO1 (for the on board LEDs), TRM 8.1.12.1.29
	ldr		r0, =CM_PER_GPIO1_CLKCTRL
	ldr		r1, =0x40002
	str		r1, [r0]

	# set pin 21,22,23,24 for output, led USR0/1/2/3,
	# TRM 25.3.4.3 
	# TRM 25.4.1.16
	ldr		r0, =GPIO1_OE
	ldr		r1, [r0]
	# In the next line we use 0xf to indicate 4 bits in a row we are clearing.
	bic		r1, r1, #(0xf<<21)
	str		r1, [r0]

	# logical 1 turns on the led0, TRM 25.3.4.2.2.2
	ldr		r0, =GPIO1_SETDATAOUT
	ldr		r1, =(1<<21)
	str		r1, [r0]

	# set uart mux config
	ldr		r0, =CONF_UART0_RXD
	ldr		r1, =(0x1<<4)|(0x1<<5)
	str		r1, [r0]
	ldr		r0, =CONF_UART0_TXD
	ldr		r1, =0x0
	str		r1, [r0]

	# setup_clocks_for_console
	ldr		r0, =CM_WKUP_CLKSTCTRL
	ldr		r1, [r0]
	and		r1, r1, #~0x3
	orr		r1, #0x2
	str		r1, [r0]
	ldr		r0, =CM_PER_L4HS_CLKSTCTRL
	ldr		r1, [r0]
	and		r1, #~0x3
	orr		r1, #0x2
	str		r1, [r0]
	ldr		r0, =CM_WKUP_UART0_CLKCTRL
	ldr		r1, [r0]
	and		r1, #~0x3
	orr		r1, #0x2
	str		r1, [r0]
	ldr		r0, =CM_PER_UART0_CLKCTRL
	ldr		r1, [r0]
	and		r1, #~0x3
	orr		r1, #0x2
	str		r1, [r0]

	# UART soft reset
	ldr		r0, =UART0_SYSC
	ldr		r1, [r0]
	orr		r1, #0x2
	str		r1, [r0]
	ldr		r0, =UART0_SYSS

uart_soft_reset:
	ldr		r1, [r0]
	ands	r1, #0x1
	beq		uart_soft_reset

	# turn off smart idle

	ldr		r0, =UART0_SYSC
	ldr		r1, [r0]
	orr		r1, #(0x1 << 0x3)
	str		r1, [r0]

	# initialize UART
	ldr		r0, =UART0_BASE
	# 0x1a: [1] even parity, [1] enable parity, [0] 1 stop bit, [10] 7bits
	ldr		r1, =0x1a 

uart_init:
	ldrb	r3, [r0, #0x14] @ LSR_UART
	uxtb	r3, r3
	tst		r3, #0x40
	beq		uart_init
	mov		r3, #0
	strb	r3, [r0, #0x04] @ IER_UART
	mov		r3, #7
	strb	r3, [r0, #0x20] @ MDR1
	mvn		r3, #0x7c
	strb	r3, [r0, #0x0c] @ LCR
	mov		r3, #0
	strb	r3, [r0]
	strb	r3, [r0, #0x04] @ IER_UART
	mov		r3, #3
	strb	r3, [r0, #0x0c] @ LCR
	strb	r3, [r0, #0x10] @ MCR
	mov		r3, #7
	strb	r3, [r0, #0x08] @ IIR_UART
	mvn		r3, #0x7c
	strb	r3, [r0, #0x0c] @ LCR
	uxtb	r3, r1
	strb	r3, [r0]
	ubfx	r1, r1, #8, #8
	strb	r1, [r0, #0x04] @ IER_UART
	mov		r3, #3
	strb	r3, [r0, #0x0c] @ LCR
	mov		r3, #0
	strb	r3, [r0, #0x20] @ MDR1

	/* turn on second led */
	ldr		r2, =GPIO1_SETDATAOUT
	ldr		r1, =(1<<22)
	str		r1, [r2]

	ldr		r1, =UART0_BASE

	ldr		r0, ='A'

	b		main_echo_server

hlt:
	b		hlt

/* ************************************* */
/* not a function */
main_echo_server:
	bl		emit_prompt 

echo_it:
	/* read a character, store in r0 */
	bl		uart_readc
	mov		r4, r0

	/* emit a debug map of registers when I hit see a ESC character */
	/* don't echo the escape back. */
	cmp 	r4, #0x1b
	bne		echo_it_echo
	bl		emit_register_dump
	bl		emit_newline
	bl		emit_prompt 
	b		echo_it

echo_it_echo:
	/* echo whatever non command character I read back to the console */
	mov		r0, r4
	bl		uart_putc

	cmp		r4, #'\n'
	bleq	emit_prompt 
	b		echo_it


/* ************************************* */
/* Emit a prompt (Don't have a .bss or .data yet. so use literal pools) */
emit_prompt:
	push	{lr}
	adrl	r0, lp_prompt_string
	bl		string_emit
	pop		{lr}
	bx		lr

/* ************************************* */
/* Emit an asciz string with r0 being the address of the string */
string_emit:
	push	{r4, lr}
	mov		r4, r0

string_loop:
	ldrb	r0, [r4], #1
	uxtb	r0, r0
	cmp		r0, #0
	beq		string_done
	bl		uart_putc
	b		string_loop

string_done:
	pop		{r4, lr}
	bx		lr

/* ************************************* */
/* Emit the value in register r0 as ASCII hex value to the serial port */

emit_reg_value:
	push	{r4-r9, lr}

	# Save the argument for processing
	mov		r9, r0

	mov		r0, #'0'
	bl		uart_putc
	mov		r0, #'x'
	bl		uart_putc

	# The translation table to go from nibble to ascii
	adrl	r8, hex_map
	# nibble counter (goes until 8)
	mov		r4, #0
	# bit position of most significant nibble
	mov		r5, #28
	# The mask we will be shifting to get the nibbles
	ldr		r6, =0xF0000000

emit_reg_nibble:
	# store arg r0 into a temporary
	mov		r7, r9

	# mask the nibble we desire using the mask
	and		r7, r7, r6
	# shift nibble to right by r4 bits to get it to least significant nibble
	asr		r7, r7, r5
	# The carry flag was undefined for the above operation, so squash out any
	# bits it may have polluted during the shift right. This instruction is
	# easier to do than doing a read-modify-write on the APSR to clear the
	# carry flag.
	and		r7, r7, #0x0F

	# lookup the nibble in the hex_map table to get the char we need into r0.
	ldrb	r0, [r8, r7]
	# emit it (uart_putc expects r0 as argument).
	bl		uart_putc

	# shift mask right one nibble
	asr		r6, r6, #4
	# decrement how many bits we need to shift to get the next nibble.
	sub		r5, r5, #4
	# increment the count of how many nibbles we're processing
	add		r4, r4, #1

	# Am I done?
	cmp		r4, #8
	blo		emit_reg_nibble

	# all done.
emit_reg_nibble_done:
	pop		{r4-r9, lr}
	bx		lr


/* ************************************* */
/* emit value of r0 */
emit_r0_value:
	push 	{r4, lr}
	mov		r4, r0
	adrl	r0, string_register_r0
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr


/* ************************************* */
/* emit value of r1 */
emit_r1_value:
	push 	{r4, lr}
	mov		r4, r1
	adrl	r0, string_register_r1
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r2 */
emit_r2_value:
	push 	{r4, lr}
	mov		r4, r2
	adrl	r0, string_register_r2
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r3 */
emit_r3_value:
	push 	{r4, lr}
	mov		r4, r3
	adrl	r0, string_register_r3
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r4 */
emit_r4_value:
	push 	{r5, lr}
	mov		r5, r4
	adrl	r0, string_register_r4
	bl		string_emit
	mov		r0, r5
	bl		emit_reg_value
	pop 	{r5, lr}
	bx		lr

/* ************************************* */
/* emit value of r5 */
emit_r5_value:
	push 	{r4, lr}
	mov		r4, r5
	adrl	r0, string_register_r5
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r6 */
emit_r6_value:
	push 	{r4, lr}
	mov		r4, r6
	adrl	r0, string_register_r6
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r7 */
emit_r7_value:
	push 	{r4, lr}
	mov		r4, r7
	adrl	r0, string_register_r7
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r8 */
emit_r8_value:
	push 	{r4, lr}
	mov		r4, r8
	adrl	r0, string_register_r8
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r9 */
emit_r9_value:
	push 	{r4, lr}
	mov		r4, r9
	adrl	r0, string_register_r9
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r10 */
emit_r10_value:
	push 	{r4, lr}
	mov		r4, r10
	adrl	r0, string_register_r10
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r11 */
emit_r11_value:
	push 	{r4, lr}
	mov		r4, r11
	adrl	r0, string_register_r11
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r12 */
emit_r12_value:
	push 	{r4, lr}
	mov		r4, r12
	adrl	r0, string_register_r12
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r13 */
emit_r13_value:
	push 	{r4, lr}
	mov		r4, r13
	adrl	r0, string_register_r13
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r14 */
emit_r14_value:
	push 	{r4, lr}
	mov		r4, r14
	adrl	r0, string_register_r14
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of r15 */
emit_r15_value:
	push 	{r4, lr}
	mov		r4, r15
	adrl	r0, string_register_r15
	bl		string_emit
	mov		r0, r4
	bl		emit_reg_value
	pop 	{r4, lr}
	bx		lr

/* ************************************* */
/* emit value of APSR */
emit_apsr_value:
	push 	{lr}
	adrl	r0, string_register_apsr
	bl		string_emit
	mrs		r0, apsr
	bl		emit_reg_value
	pop 	{lr}
	bx		lr

/* ************************************* */
/* print values of all registers to serial port. */
emit_register_dump:
	m_all_regs_store

	# allow this so I can destroy registers.
	m_all_regs_store
	bl		emit_newline
	adrl	r0, string_register_dump
	bl		string_emit
	bl		emit_newline
	m_all_regs_restore

	bl		emit_r0_value
	bl		emit_newline
	bl		emit_r1_value
	bl		emit_newline
	bl		emit_r2_value
	bl		emit_newline
	bl		emit_r3_value
	bl		emit_newline
	bl		emit_r4_value
	bl		emit_newline
	bl		emit_r5_value
	bl		emit_newline
	bl		emit_r6_value
	bl		emit_newline
	bl		emit_r7_value
	bl		emit_newline
	bl		emit_r8_value
	bl		emit_newline
	bl		emit_r9_value
	bl		emit_newline
	bl		emit_r10_value
	bl		emit_newline
	bl		emit_r11_value
	bl		emit_newline
	bl		emit_r12_value
	bl		emit_newline
	bl		emit_r13_value
	bl		emit_newline
	bl		emit_r14_value
	bl		emit_newline
	bl		emit_r15_value
	bl		emit_newline
	bl		emit_apsr_value
	bl		emit_newline

	m_all_regs_restore
	bx		lr


# TODO: add functions for the rest of the registers. 

/* ************************************* */
/* emit a newline character */

emit_newline:
	/* save caller vars and the previous link register */
	push	{lr}

	/* emit the newline */
	mov		r0, #'\r'
	bl		uart_putc
	mov		r0, #'\n'
	bl		uart_putc

	/* restore the caller vars and the previous link register */
	pop		{lr}

	/* and return. */
	bx		lr


/* ************************************* */
/* character to print out in r0 */

uart_putc:
	push	{r4-r5, lr}
	ldr		r4, =UART0_BASE

is_line_ready_write:
	ldrb	r5, [r4, #20]
	uxtb	r5, r5
	tst		r5, #32
	beq		is_line_ready_write

	/* Yup, so emit the character. */
	strb	r0, [r4]

	/* and return. */
	pop		{r4-r5, lr}
	bx		lr

/* ************************************* */
/* character read in r0 */

uart_readc:
	push	{r4-r5, lr}
	ldr		r4, =UART0_BASE

is_line_ready_read:
	ldrb	r5, [r4, #20]
	uxtb	r5, r5
	tst		r5, #1
	beq		is_line_ready_read

	/* Yup, so read and zero extend the byte character into r0. */
	ldrb	r0, [r4]

	/* TODO: we get rid of high bits to keep it in pure ascii! */
	/* Why were there high bits here to begin with, did kermit send it? 
		did we read it out of the serial register? WHat's up? */
	and		r0, r0, #0x7f

	/* and return. */
	pop		{r4-r5, lr}
	bx		lr




/* Data section */

/* Hrm, this is in sore need of a data segment and a linker! These things
	must be aligned to 4 bytes, too, if instructions come after it.
*/
lp_prompt_string: 
	.asciz "SDM v0.1 > "
hex_map:
	.ascii "0123456789abcdef"
string_register_dump:
	.asciz "Register DUMP:"
string_register_r0:
	.asciz "  r0(a1):     "
string_register_r1:
	.asciz "  r1(a2):     "
string_register_r2:
	.asciz "  r2(a3):     "
string_register_r3:
	.asciz "  r3(a4):     "
string_register_r4:
	.asciz "  r4(v1):     "
string_register_r5:
	.asciz "  r5(v2):     "
string_register_r6:
	.asciz "  r6(v3):     "
string_register_r7:
	.asciz "  r7(v4):     "
string_register_r8:
	.asciz "  r8(v5):     "
string_register_r9:
	.asciz "  r9(v6/sb):  "
string_register_r10:
	.asciz "  r10(v7/sl): "
string_register_r11:
	.asciz "  r11(v8/fp): "
string_register_r12:
	.asciz "  r12(ip):    "
string_register_r13:
string_register_sp:
	.asciz "  r13(sp):    "
string_register_r14:
string_register_lr:
	.asciz "  r14(lr):    "
string_register_r15:
string_register_pc:
	.asciz "  r15(pc):    "
string_register_apsr:
	.asciz "  apsr:       "

# TODO: add more strings here to dump registers.






