    .inesprg 1
    .ineschr 1
    .inesmap 0
    .inesmir 1

; --------------------------------------------------------------------------

PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
OAMADDR   = $2003
OAMDATA   = $2004
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007
OAMDMA    = $4014
JOYPAD1 = $4016
JOYPAD2 = $4017

BUTTON_A      = %10000000
BUTTON_B      = %01000000
BUTTON_SELECT = %00100000
BUTTON_START  = %00010000
BUTTON_UP     = %00001000
BUTTON_DOWN   = %00000100
BUTTON_LEFT   = %00000010
BUTTON_RIGHT  = %00000001

    .rsset $0000
seed                .rs 2
joypad1_state       .rs 1
nametable_address   .rs 2
scroll_x            .rs 1
scroll_page         .rs 1
generate_x          .rs 1
generate_counter    .rs 1
generate_pipe_y     .rs 1
player_speed        .rs 2   ; in subpixels/frame -- 16 bits
player_position_sub .rs 1   ; in subpixels

    .rsset $0200
sprite_player       .rs 4

    .rsset $0000
SPRITE_Y            .rs 1
SPRITE_TILE         .rs 1
SPRITE_ATTRIB       .rs 1
SPRITE_X            .rs 1

PIPE_DISTANCE          = 12
PIPE_GAP               = 6
PIPE_RANDOM_MASK       = 15
PIPE_DISTANCE_FROM_TOP = 5

GRAVITY                = 10               ; in subpixels/frame^2
FLAP_SPEED             = -(1 * 256 + 128) ; in subpixels/frame
SCREEN_BOTTOM_Y        = 224

    .bank 0
    .org $C000

; Initialisation code based on https://wiki.nesdev.com/w/index.php/Init_code
RESET:
    SEI          ; ignore IRQs
    CLD          ; disable decimal mode
    LDX #$40
    STX $4017    ; disable APU frame IRQ
    LDX #$ff
    TXS          ; Set up stack
    INX          ; now X = 0
    STX PPUCTRL  ; disable NMI
    STX PPUMASK  ; disable rendering
    STX $4010    ; disable DMC IRQs

    ; Optional (omitted):
    ; Set up mapper and jmp to further init code here.

    ; If the user presses Reset during vblank, the PPU may reset
    ; with the vblank flag still true.  This has about a 1 in 13
    ; chance of happening on NTSC or 2 in 9 on PAL.  Clear the
    ; flag now so the @vblankwait1 loop sees an actual vblank.
    BIT PPUSTATUS

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
vblankwait1:  
    BIT PPUSTATUS
    BPL vblankwait1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.
    ; One thing we can do with this time is put RAM in a known state.
    ; Here we fill it with $00, which matches what (say) a C compiler
    ; expects for BSS.  Conveniently, X is still 0.
    TXA
clrmem:
    LDA #0
    STA $000,x
    STA $100,x
    STA $300,x
    STA $400,x
    STA $500,x
    STA $600,x
    STA $700,x  ; Remove this if you're storing reset-persistent data

    ; We skipped $200,x on purpose.  Usually, RAM page 2 is used for the
    ; display list to be copied to OAM.  OAM needs to be initialized to
    ; $EF-$FF, not 0, or you'll get a bunch of garbage sprites at (0, 0).

    LDA #$FF
    STA $200,x

    INX
    BNE clrmem

    ; Other things you can do between vblank waits are set up audio
    ; or set up other mapper registers.
   
vblankwait2:
    BIT PPUSTATUS
    BPL vblankwait2

    ; End of initialisation code

    JSR InitialiseGame

    LDA  #%10000000 ; Enable NMI
    STA PPUCTRL

    LDA #%00011000  ; Enable sprites and background
    STA PPUMASK

    LDA #0
    STA PPUSCROLL   ; Set x scroll
    STA PPUSCROLL   ; Set y scroll

    ; Enter an infinite loop
forever:
    JMP forever

; --------------------------------------------------------------------------

InitialiseGame: ; begin subroutine
    ; Seed the random number generator
    LDA #$12
    STA seed
    LDA #$34
    STA seed+1

    ; Reset the PPU high/low latch
    LDA PPUSTATUS

    ; Write address 3F00 (background palette) to the PPU
    LDA #$3F
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    ; Write the background palette
    LDA #$31
    STA PPUDATA
    LDA #$09
    STA PPUDATA
    LDA #$19
    STA PPUDATA
    LDA #$29
    STA PPUDATA
    LDA #$31
    STA PPUDATA
    LDA #$05
    STA PPUDATA
    LDA #$15
    STA PPUDATA
    LDA #$25
    STA PPUDATA


    ; Write address 3F10 (sprite palette) to the PPU
    LDA #$3F
    STA PPUADDR
    LDA #$10
    STA PPUADDR

    ; Write the background colour
    LDA #$30
    STA PPUDATA

    ; Write the palette colours
    LDA #$30
    STA PPUDATA
    LDA #$19
    STA PPUDATA
    LDA #$05
    STA PPUDATA

    ; Write sprite data for sprite 0
    LDA #120     ; Y position
    STA sprite_player + SPRITE_Y
    LDA #1       ; Tile number
    STA sprite_player + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_player + SPRITE_ATTRIB
    LDA #128     ; X position
    STA sprite_player + SPRITE_X    

    ; Generate initial level
InitialGeneration_Loop:
    JSR GenerateColumn
    LDA generate_x
    CMP #36
    BCC InitialGeneration_Loop

    ; Load attribute data
    LDA #$23             ; Write address $23C0 to PPUADDR register
    STA PPUADDR
    LDA #$C0
    STA PPUADDR

    LDA #%00000000
    LDX #64

LoadAttributes_Loop:
    STA PPUDATA
    DEX
    BNE LoadAttributes_Loop

    ; Load nametable data
    LDA #$24             ; Write address $2400 to PPUADDR register
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDA #LOW(NametableData)
    STA nametable_address
    LDA #HIGH(NametableData)
    STA nametable_address+1  
LoadNametable2_OuterLoop:
    LDY #0
LoadNametable2_InnerLoop:
    LDA [nametable_address], Y
    BEQ LoadNametable2_End
    STA PPUDATA
    INY
    BNE LoadNametable2_InnerLoop
    INC nametable_address+1
    JMP LoadNametable2_OuterLoop
LoadNametable2_End:

    ; Load attribute data
    LDA #$27             ; Write address $27C0 to PPUADDR register
    STA PPUADDR
    LDA #$C0
    STA PPUADDR

    LDA #%00000000
    LDX #64

LoadAttributes2_Loop:
    STA PPUDATA
    DEX
    BNE LoadAttributes2_Loop


    RTS ; End subroutine

; --------------------------------------------------------------------------

; NMI is called on every frame
NMI:
    ; Scroll
    LDA scroll_x
    CLC 
    ADC #1
    STA scroll_x
    STA PPUSCROLL
    BCC scroll_NoWrap
    ; scroll_x has wrapped, so switch scroll page
    LDA scroll_page
    EOR #1
    STA scroll_page
scroll_NoWrap:
    LDA #0
    STA PPUSCROLL

    ; Check if a column of background needs to be generated
    LDA scroll_x
    AND #7
    BNE scroll_NoGenerate
    JSR GenerateColumn
scroll_NoGenerate:

    ; Initialise controller 1
    LDA #1
    STA JOYPAD1
    LDA #0
    STA JOYPAD1

    ; Read joypad state
    LDX #0
    STX joypad1_state
ReadController:
    LDA JOYPAD1
    LSR A
    ROL joypad1_state
    INX
    CPX #8
    BNE ReadController

    ; React to A button
    LDA joypad1_state
    AND #BUTTON_A
    BEQ ReadA_Done  
    ; Set player speed
    LDA #Low(FLAP_SPEED)
    STA player_speed
    LDA #HIGH(FLAP_SPEED)
    STA player_speed+1
ReadA_Done:

    ; Update player sprite
    ; First, update speed
    LDA player_speed   ; Low 8 bits
    CLC
    ADC #LOW(GRAVITY)
    STA player_speed
    LDA player_speed+1
    ADC #HIGH(GRAVITY) ; High 8 bits
    STA player_speed+1 ; NB: *don't* clear the carry flag!

    ; Second, udpate position
    LDA player_position_sub    ; Low 8 bits
    CLC
    ADC player_speed
    STA player_position_sub
    LDA sprite_player+SPRITE_Y ; High 8 bits
    ADC player_speed+1         ;NB: *don't* clear the carry flag!
    STA sprite_player+SPRITE_Y

    ; Check for top or bottom of screen
    CMP #SCREEN_BOTTOM_Y     ; Accumulator already contains player y position
    BCC UpdatePlayer_NoClamp
    ; Check sign of speed
    LDA player_speed+1
    BMI UpdatePlayer_ClampToTop
    LDA #SCREEN_BOTTOM_Y-1   ; Clamp to bottom
    JMP UpdatePlayer_DoClamping
UpdatePlayer_ClampToTop:
    LDA #0                   ; Clamp to top
UpdatePlayer_DoClamping:
    STA sprite_player+SPRITE_Y
    LDA #0                   ; Set player speed to zero
    STA player_speed         ; (both bytes)
    STA player_speed+1
UpdatePlayer_NoClamp:

    ; Copy sprite data to the PPU
    LDA #0
    STA OAMADDR
    LDA #$02
    STA OAMDMA

    ; Set PPUCTRL register
    LDA scroll_page
    ORA #%10000000
    STA PPUCTRL

    RTI        ; Return from interrupt

; --------------------------------------------------------------------------

; prng  -- https://wiki.nesdev.com/w/index.php/Random_number_generator
;
; Returns a random 8-bit number in A (0-255), clobbers X (0).
;
; Requires a 2-byte value on the zero page called "seed".
; Initialize seed to any value except 0 before the first call to prng.
; (A seed value of 0 will cause prng to always return 0.)
;
; This is a 16-bit Galois linear feedback shift register with polynomial $002D.
; The sequence of numbers it generates will repeat after 65535 calls.
;
; Execution time is an average of 125 cycles (excluding jsr and rts)

.code
prng:
	LDX #8     ; iteration count (generates 8 bits)
	LDA seed+0    
prng_1:
	ASL A       ; shift the register
	ROL seed+1
	BCC prng_2
	EOR #$2D   ; apply XOR feedback whenever a 1 bit is shifted out
prng_2:
	DEX
	BNE prng_1
	STA seed+0
	CMP #0     ; reload flags
	RTS

; --------------------------------------------------------------------------

GenerateColumn:
    ; Put PPU into add 32 mode
    LDA #%00000100
    STA PPUCTRL

    ; Find most significant byte of PPU address
    LDA generate_x
    AND #32             ; Accumlator = 0 for nametable $2000, 32 for nametable $2400
    LSR A               ; Divide by 8 to get accumulator = 0 or 4
    LSR A
    LSR A               ; This also clears the carry flag
    ADC #$20            ; Accumulator now = $20 or $24
    STA PPUADDR

    ; Find least significant byte of PPU address
    LDA generate_x
    AND #31
    STA PPUADDR

    ; Write the data
    LDA generate_counter
    BNE GenerateColumn_ExistingPipe
    ; Set up new pipes
    JSR prng
    AND #PIPE_RANDOM_MASK
    CLC
    ADC #PIPE_DISTANCE_FROM_TOP
    STA generate_pipe_y
    LDA generate_counter
GenerateColumn_ExistingPipe

    ; If generate_counter >= 4, generate an empty column
    CMP #4
    BCS GenerateColumn_Empty
    ; Else, generate pipes
    ; Body of top pipe -- length is generate_pipe_y - 2
    LDX generate_pipe_y
    DEX
    DEX
    AND #$03
    ORA #$30           ; Use tile 30, 31, 32 or 33 depending on generate_counter
.Loop_1:
    STA PPUDATA
    DEX
    BNE .Loop_1

    ; Rim of top pipe
    AND #$03
    ORA #$10          ; Use tile 10, 11, 12 or 13 depending on generate_counter
    STA PPUDATA
    AND #$03
    ORA #$20          ; Use tile 20, 21, 22 or 23 depending on generate_counter
    STA PPUDATA

    ; Empty space between pipes
    LDX #PIPE_GAP
    LDY #$03
.Loop_2:
    STY PPUDATA
    DEX
    BNE .Loop_2

    ; Rim of the bottom pipe
    AND #$03
    ORA #$10          ; Use tile 10, 11, 12 or 13 depending on generate_counter
    STA PPUDATA
    AND #$03
    ORA #$20          ; Use tile 20, 21, 22 or 23 depending on generate_counter
    STA PPUDATA

    ; Body of bottom pipe -- length is 30 - 2 - PIPE_GAP - generate_pipe_y
    AND #$03
    ORA #$30           ; Use tile 30, 31, 32 or 33 depending on generate_counter
    TAY                ; Store tile number in Y register so we can use the accumulator
    LDA #30 - 2 - PIPE_GAP
    SEC
    SBC generate_pipe_y
    TAX 
.Loop_3
    STY PPUDATA
    DEX
    BNE .Loop_3
    JMP GenerateColumn_End

GenerateColumn_Empty:
    LDX #30         ; 30 rows
    LDA #$00        ; Tile 0
GenerateColumn_Empty_Loop:
    STA PPUDATA
    DEX
    BNE GenerateColumn_Empty_Loop

GenerateColumn_End:
    ; Increment generate_x
    LDA generate_x
    CLC
    ADC #1
    AND #63            ; Wrap back to zero at 64
    STA generate_x

    ; Increment generate_counter
    LDA generate_counter
    CLC
    ADC #1
    CMP #PIPE_DISTANCE
    BCC GenerateColumn_NoCounterWrap
    LDA #0
GenerateColumn_NoCounterWrap:
    STA generate_counter

    RTS
    
NametableData:
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$10,$11,$12,$13,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$20,$21,$22,$23,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$10,$11,$12,$13,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$20,$21,$22,$23,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$10,$11,$12,$13,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$20,$21,$22,$23,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$10,$11,$12,$13,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$20,$21,$22,$23,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $00  ; null terminator
; --------------------------------------------------------------------------


    .bank 1
    .org $FFFA
    .dw NMI
    .dw RESET
    .dw 0

; --------------------------------------------------------------------------

    .bank 2
    .org $0000
    .incbin "comp310.chr"

