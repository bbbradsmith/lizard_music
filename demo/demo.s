;
; Lizard Music Engine demo
; Brad Smith, 2019
; http://lizardnes.com
;

.macpack longbranch
.include "../music.inc"


.segment "ZEROPAGE"

; rendering
nmi_on:    .res 1 ; if 0 NMI does nothing but increment nmi_count
nmi_count: .res 1 ; increments every NMI
nmi_ready:  .res 1 ; controls PPU updates during NMI (0 for none, returned to 0 after NMI)
nmi_addr:  .res 2 ; address of PPU update (see nmi_buffer)
scroll_x:  .res 1
scroll_y:  .res 1
ppu_2000:  .res 1
ppu_2001:  .res 1

; gamepad
gamepad:      .res 1 ; combined gamepad poll state
gamepad_old:  .res 1 ; previous value of gamepad
gamepad_new:  .res 1 ; buttons pressed since last poll
gamepads:     .res 2 ; individual gamepads
gamepads_old: .res 2
gamepads_new: .res 2

; generic pointer
ptr: .res 2

select_music: .res 1
select_sfx: .res 1
select_pos: .res 1

.segment "STACK"
nmi_buffer: .res 128

.segment "OAM"
.align 256
oam: .res 256

.segment "RAM"
nmi_palette: .res 32

.segment "CODE"

;
; main
;

main:
	jsr screen_init
	; setup music player
	jsr cpu_speed_detect
	sta player_pal
	jsr music_init
	; begin rendering and NMI
	lda #%00011110
	sta ppu_2001
	lda #%10000000
	sta ppu_2000
	sta $2000
	lda #1
	sta nmi_on
main_loop:
	jsr poll_gamepads
	lda gamepad_new
	and #PAD_D
	beq :+
		inc select_pos
		lda select_pos
		cmp #3
		bcc :+
		lda #2
		sta select_pos
	:
	lda gamepad_new
	and #PAD_U
	beq :+
		dec select_pos
		lda select_pos
		cmp #3
		bcc :+
		lda #0
		sta select_pos
	:
	ldx select_pos
	cpx #2
	bcs @horizontal_skip
	lda gamepad_new
	and #PAD_R
	beq @right_end
		cpx #0
		bne :+
			inc select_music
			lda select_music
			cmp #MUSIC_COUNT
			bcc @right_end
			lda #0
			sta select_music
			jmp @right_end
		:
			inc select_sfx
			lda select_sfx
			cmp #SFX_COUNT
			bcc @right_end
			lda #0
			sta select_sfx
			;jmp @right_end
		;
	@right_end:
	lda gamepad_new
	and #PAD_L
	beq @left_end
		cpx #0
		bne :+
			dec select_music
			lda select_music
			cmp #MUSIC_COUNT
			bcc @left_end
			lda #MUSIC_COUNT-1
			sta select_music
			jmp @left_end
		:
			dec select_sfx
			lda select_sfx
			cmp #SFX_COUNT
			bcc @left_end
			lda #SFX_COUNT-1
			sta select_sfx
			;jmp @left_end
		;
	@left_end:
	@horizontal_skip:
	lda gamepad_new
	and #(PAD_A|PAD_B|PAD_START)
	beq @start_end
		lda select_pos
		bne :+
			lda select_music
			sta player_next_music
			jmp @start_end
		:
		cmp #1
		bne :+
			ldx select_sfx
			lda sfx_type, X
			tax
			lda select_sfx
			sta player_next_sound, X
			jmp @start_end
		:
		cmp #2
		bne :+
			lda player_pause
			eor #1
			sta player_pause
			;jmp @start_end
		:
	@start_end:
	; redraw screen and wait for NMI to post the update
	jsr screen_refresh
	:
		lda nmi_ready
		bne :-
	; greyscale for profiling, rought timing of music routine finish
	lda gamepads+0
	and #PAD_SELECT
	beq :+
		lda #%00011111
		sta $2001
	:
	jmp main_loop

;
; music engine demo stuff
;

; normally square vs. noise SFX is resolved by the PLAY_SOUND macro,
; but this only works for immediate values. This generates a lookup
; table to determine square (0) vs. noise (1).
sfx_type:
.repeat SFX_COUNT, I
	.byte .ident(.sprintf("SOUND_MODE__%d",I))
.endrepeat

.include "..\output\data_music_strings.inc" ; string tables

music_string: ; A = index
	asl
	sta ptr+0
	lda #0
	rol
	sta ptr+1
	lda #<data_music_titles
	clc
	adc ptr+0
	sta ptr+0
	lda #>data_music_titles
	adc ptr+1
	sta ptr+1
	ldy #0
	lda (ptr), Y
	tax
	iny
	lda (ptr), Y
	sta ptr+1
	stx ptr+0
	rts

sfx_string: ; A = index
	asl
	sta ptr+0
	lda #0
	rol
	sta ptr+1
	lda #<data_sfx_titles
	clc
	adc ptr+0
	sta ptr+0
	lda #>data_sfx_titles
	adc ptr+1
	sta ptr+1
	ldy #0
	lda (ptr), Y
	tax
	iny
	lda (ptr), Y
	sta ptr+1
	stx ptr+0
	rts

load_nmi_string: ; cut string to 24 characters in length, fill rest with background
	ldy #0
	:
		lda (ptr), Y
		beq :+
		sta nmi_buffer+3, Y
		iny
		cpy #24
		bcc :-
		rts
	:
	lda #2
	:
		cpy #24
		bcs :+
		sta nmi_buffer+3, Y
		iny
		bne :- ; always jump
	:
	rts

hex_table: .byte "0123456789ABCDEF"

load_nmi_hex:
	pha
	and #$0F
	tax
	lda hex_table, X
	sta nmi_buffer+3+24+4
	pla
	lsr
	lsr
	lsr
	lsr
	tax
	lda hex_table, X
	sta nmi_buffer+3+24+3
	rts

draw_string:
	ldy #0
	:
		lda (ptr), Y
		beq :+
		sta $2007
		iny
		bne :- ; "always" jump, or cancel at 256
	:
	rts

.macro PPU_LATCH addr_
	bit $2002
	lda #>addr_
	sta $2006
	lda #<addr_
	sta $2006
.endmacro

.macro NMI_LATCH addr_
	lda #>addr_
	sta nmi_buffer+1
	lda #<addr_
	sta nmi_buffer+2
.endmacro

.macro NMI_HEX addr_
	jsr load_nmi_hex
	lda #2
	sta nmi_buffer+3+24+0
	lda #>addr_
	sta nmi_buffer+3+24+1
	lda #<addr_
	sta nmi_buffer+3+24+2
.endmacro

.macro LOAD_PTR addr_
	lda #<addr_
	sta ptr+0
	lda #>addr_
	sta ptr+1
.endmacro

.macro DRAW_STRING string_, addr_
	PPU_LATCH addr_
	LOAD_PTR string_
	jsr draw_string
.endmacro

text_title:   .asciiz "Lizard Music Engine Demo"
text_music:   .asciiz "Music: 00"
text_sfx:     .asciiz "SFX:   00"
text_playing: .asciiz "Playing"
text_paused:  .asciiz "Paused!"

startup_palette:
.byte $0F, $09, $19, $20
.byte $0F, $09, $19, $20
.byte $0F, $09, $19, $20
.byte $0F, $09, $19, $20
.byte $0F, $00, $16, $26
.byte $0F, $00, $16, $26
.byte $0F, $00, $16, $26
.byte $0F, $00, $16, $26

ARROW_X0 = 3*8
arrow_x1: .byte  28*8,   28*8,     11*8
arrow_y:  .byte (6*8)-1, (9*8)-1, (11*8)-1

screen_refresh:
	lda select_pos
	beq @music
	cmp #1
	beq @sfx
	cmp #2
	beq @pause
@music:
	lda select_music
	NMI_HEX $20AB
	NMI_LATCH $20C4
	lda select_music
	jsr music_string
	jmp @finish
@sfx:
	lda select_sfx
	NMI_HEX $210B
	NMI_LATCH $2124
	lda select_sfx
	jsr sfx_string
	jmp @finish
@pause:
	lda #0
	sta nmi_buffer+3+24
	NMI_LATCH $2164
	lda player_pause
	bne :+
		LOAD_PTR text_playing
		jmp :++
	:
		LOAD_PTR text_paused
	:
	;jmp @finish
@finish:
	jsr load_nmi_string
	lda #24
	sta nmi_buffer+0
	; arrow sprite position
	lda #ARROW_X0
	sta oam+3
	ldx select_pos
	lda arrow_x1, X
	sta oam+7
	lda arrow_y, X
	sta oam+0
	sta oam+4
	; NMI is ready
	lda #1
	sta nmi_ready
	rts

screen_init:
	; load palette
	bit $2002
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	ldx #0
	:
		lda startup_palette, X
		sta nmi_palette, X
		inx
		cpx #32
		bcc :-
	; reset sprites (two arrows)
	ldx #0
	lda #$FF
	:
		sta oam, X
		inx
		bne :-
	lda #$04 ; arrow tile
	sta oam+1
	sta oam+5
	lda #$00
	sta oam+2
	lda #$40 ; flip horizontal
	sta oam+6
	; reset nametables
	bit $2002
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	lda #2
	ldy #16
	ldx #0
	:
		sta $2007
		dex
		bne :-
		dey
		bne :-
	; prepare text
	DRAW_STRING text_title,   $2064
	DRAW_STRING text_music,   $20A4
	DRAW_STRING text_sfx,     $2104
	DRAW_STRING text_playing, $2164
	PPU_LATCH                 $2124
	lda #0
	jsr sfx_string
	jsr draw_string
	jsr screen_refresh
	rts

;
; gamepads
;

.enum
	PAD_R = 1
	PAD_L = 2
	PAD_D = 4
	PAD_U = 8
	PAD_START = 16
	PAD_SELECT = 32
	PAD_B = 64
	PAD_A = 128
.endenum

poll_gamepads:
	lda gamepads+0
	sta gamepads_old+0
	lda gamepads+1
	sta gamepads_old+1
	lda gamepad
	sta gamepad_old
	ldx #1
	stx $4016
	ldx #0
	stx $4016
	:
		lda $4016
		and #3
		cmp #1
		rol gamepads+0
		lda $4017
		and #3
		cmp #1
		rol gamepads+1
		inx
		cpx #8
		bcc :-
	; onset from either controller goes into gamepad_new
	lda gamepads+0
	eor gamepads_old+0
	and gamepads+0
	sta gamepad_new
	lda gamepads+1
	eor gamepads_old+1
	and gamepads+1
	ora gamepad_new
	sta gamepad_new
	; both controllers combine into gamepad
	lda gamepads+0
	ora gamepads+1
	sta gamepad
	rts

;
; NTSC/PAL detection
;

.segment "ALIGN"

; Detects NTSC vs PAL
; http://forums.nesdev.com/viewtopic.php?p=163258#p163258
; A = 0 NTSC
;     1 PAL
;     2 Dendy
.align 32
cpu_speed_detect:
	; count increments between 2 vblanks
	ldx #0
	ldy #0
	bit $2002
:
	bit $2002
	bpl :-
:
	inx
	bne :+
		iny
	:
	bit $2002
	bpl :--
	; compensate for a double-frame in case $2002 was accidentally suppressed once
	tya
	cmp #16
	bcc :+
	lsr
:
	sec
	sbc #9
	cmp #3
	bcc :+
	lda #0 ; 3+ is invalid, maybe a bad emulator? assume NTSC
:
	rts

;
; vector handlers
;

.segment "CODE"

nmi:
	pha
	txa
	pha
	tya
	pha
	inc nmi_count
	lda nmi_on
	jeq @skip_all
	lda nmi_ready
	beq @skip_ppu
	; OAM DMA
	lda #0
	sta $2003
	lda #>oam
	sta $4014
	; palettes
	bit $2002
	ldx #0
	stx $2000 ; horizontal increment
	lda #$3F
	sta $2006
	stx $2006
	:
		lda nmi_palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; nametable/etc.
	tsx ; swap stack pointer with nmi_buffer
	txa
	.assert (>nmi_buffer) = $01, error, "nmi_buffer not in stack?"
	ldx #<(nmi_buffer-1)
	txs
	tax
	@packet:
		pla
		beq @packet_end
		tay
		pla
		sta $2006
		pla
		sta $2006
		:
			pla
			sta $2007
			dey
			bne :-
		jmp @packet
	@packet_end:
	txs ; restore stack pointer
@finish:
	lda ppu_2000
	sta $2000
	lda ppu_2001
	sta $2001
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
	lda #0
	sta nmi_ready
@skip_ppu:
	jsr music_tick
@skip_all:
	pla
	tay
	pla
	tax
	pla
	rti

irq:
	; turn screen dim grey and enter infinite loop
	; (to use BRK as a error condition for debugging)
	lda #%11111111
	sta $2001
	:
	jmp :-

reset:
	sei       ; disable maskable interrupts
	lda #0
	sta $2000 ; disable non-maskable interrupt
	lda #0
	sta $2001 ; rendering off
	sta $4010 ; disable DMC IRQ
	sta $4015 ; disable APU sound
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; setup stack
	; wait for vblank #1
	bit $2002
	:
		bit $2002
		bpl :-
	; clear RAM
	lda #0
	tax
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; wait for vblank #2
	:
		bit $2002
		bpl :-
	; ready
	jmp main

;
; vectors
;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;
; CHR graphics tiles
;

.segment "CHR"
.incbin "demo.chr"

;
; iNES header
;

.segment "HEADER"

INES_MAPPER     = 0 ; NROM
INES_MIRROR     = 0 ; vertical nametables
INES_PRG_16K    = 2 ; 32K
INES_CHR_8K     = 1 ; 8K
INES_BATTERY    = 0
INES2           = %00001000 ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = 0
INES2_PRGBAT    = 0
INES2_CHRRAM    = 0
INES2_CHRBAT    = 0
INES2_REGION    = 2 ; 0=NTSC, 1=PAL, 2=Dual

; iNES 1 header
.byte 'N', 'E', 'S', $1A ; ID
.byte <INES_PRG_16K
.byte INES_CHR_8K
.byte INES_MIRROR | (INES_BATTERY << 1) | ((INES_MAPPER & $f) << 4)
.byte (<INES_MAPPER & %11110000) | INES2
; iNES 2 section
.byte (INES2_SUBMAPPER << 4) | (INES_MAPPER>>8)
.byte ((INES_CHR_8K >> 8) << 4) | (INES_PRG_16K >> 8)
.byte (INES2_PRGBAT << 4) | INES2_PRGRAM
.byte (INES2_CHRBAT << 4) | INES2_CHRRAM
.byte INES2_REGION
.byte $00 ; VS system
.byte $00, $00 ; padding/reserved
.assert * = 16, error, "NES header must be 16 bytes."
