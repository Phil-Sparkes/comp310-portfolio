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
joypad1_state       .rs 1
nametable_address   .rs 2
scroll_x            .rs 1
scroll_page         .rs 1

    .rsset $0200
sprite_player       .rs 4

    .rsset $0000
SPRITE_Y            .rs 1
SPRITE_TILE         .rs 1
SPRITE_ATTRIB       .rs 1
SPRITE_X            .rs 1

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
    LDA #0       ; Tile number
    STA sprite_player + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_player + SPRITE_ATTRIB
    LDA #128     ; X position
    STA sprite_player + SPRITE_X    

    ; Load nametable data
    LDA #$20             ; Write address $2000 to PPUADDR register
    STA PPUADDR
    LDA #$00
    STA PPUADDR

    LDA #LOW(NametableData)
    STA nametable_address
    LDA #HIGH(NametableData)
    STA nametable_address+1  
LoadNametable_OuterLoop:
    LDY #0
LoadNametable_InnerLoop:
    LDA [nametable_address], Y
    BEQ LoadNametable_End
    STA PPUDATA
    INY
    BNE LoadNametable_InnerLoop
    INC nametable_address+1
    JMP LoadNametable_OuterLoop
LoadNametable_End:

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

    LDA #%01010101
    LDX #64

LoadAttributes2_Loop:
    STA PPUDATA
    DEX
    BNE LoadAttributes2_Loop


    RTS ; End subroutine

; --------------------------------------------------------------------------

; NMI is called on every frame
NMI:
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

 ; React to Up button
    LDA joypad1_state
    AND #BUTTON_UP
    BEQ ReadUp_Done  
    LDA sprite_player + SPRITE_Y
    CLC
    ADC #-1
    STA sprite_player + SPRITE_Y
ReadUp_Done:

    ; React to Down button
    LDA joypad1_state
    AND #BUTTON_DOWN
    BEQ ReadDown_Done  
    LDA sprite_player + SPRITE_Y
    CLC
    ADC #1
    STA sprite_player + SPRITE_Y
ReadDown_Done:

    ; React to Left button
    LDA joypad1_state
    AND #BUTTON_LEFT
    BEQ ReadLeft_Done 
    LDA sprite_player + SPRITE_X
    CLC
    ADC #-1
    STA sprite_player + SPRITE_X
ReadLeft_Done:

    ; React to Right button
    LDA joypad1_state
    AND #BUTTON_RIGHT
    BEQ ReadRight_Done  
    LDA sprite_player + SPRITE_X
    CLC
    ADC #1
    STA sprite_player + SPRITE_X
ReadRight_Done:

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
    ORA #%10000000
    STA PPUCTRL
scroll_NoWrap:
    LDA #0
    STA PPUSCROLL


    ; Copy sprite data to the PPU
    LDA #0
    STA OAMADDR
    LDA #$02
    STA OAMDMA

    RTI        ; Return from interrupt

; --------------------------------------------------------------------------
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
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$10,$11,$12,$13,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$20,$21,$22,$23,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
    .db $03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$30,$31,$32,$33,$03,$03,$03,$03,$03,$03,$03,$03 
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

