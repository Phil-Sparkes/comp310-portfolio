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

PLAYER_HITBOX_WIDTH  = 8
PLAYER_HITBOX_HEIGHT = 8
BALL_HITBOX_X        = 3
BALL_HITBOX_Y        = 1
BALL_HITBOX_WIDTH    = 8
BALL_HITBOX_HEIGHT   = 8

    .rsset $0000
joypad1_state       .rs 1
nametable_address   .rs 2
player_speed        .rs 2   ; in subpixels/frame -- 16 bits
player_position_sub .rs 1   ; in subpixels
ball_speed          .rs 2   ; in subpixels/frame -- 16 bits
ball_position_sub   .rs 1   ; in subpixels

    .rsset $0200
sprite_player       .rs 4
sprite_ball         .rs 4

    .rsset $0000
SPRITE_Y            .rs 1
SPRITE_TILE         .rs 1
SPRITE_ATTRIB       .rs 1
SPRITE_X            .rs 1

GRAVITY                = 10               ; in subpixels/frame^2
JUMP_SPEED             = -(1 * 256 + 128) ; in subpixels/frame
SCREEN_BOTTOM_Y        = 160

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
    LDA #$21
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
    LDA #$0F
    STA PPUDATA

    ; Write the palette colours
    LDA #$34
    STA PPUDATA
    LDA #$14
    STA PPUDATA
    LDA #$06
    STA PPUDATA

    ; Write sprite data for sprite 0
    LDA #200     ; Y position
    STA sprite_player + SPRITE_Y
    LDA #1       ; Tile number
    STA sprite_player + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_player + SPRITE_ATTRIB
    LDA #30     ; X position
    STA sprite_player + SPRITE_X    

    ; Write sprite data for sprite 1
    LDA #125     ; Y position
    STA sprite_ball + SPRITE_Y
    LDA #2       ; Tile number
    STA sprite_ball + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_ball + SPRITE_ATTRIB
    LDA #128     ; X position
    STA sprite_ball + SPRITE_X    

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

    LDA #%00000000
    LDX #32

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

 ; React to A button
    LDA joypad1_state
    AND #BUTTON_A
    BEQ ReadA_Done  
    ; Set player speed
    LDA #Low(JUMP_SPEED)
    STA player_speed
    LDA #HIGH(JUMP_SPEED)
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

  ; Update ball sprite
    ; First, update speed
    LDA ball_speed   ; Low 8 bits
    CLC
    ADC #LOW(GRAVITY)
    STA ball_speed
    LDA ball_speed+1
    ADC #HIGH(GRAVITY) ; High 8 bits
    STA ball_speed+1 ; NB: *don't* clear the carry flag!

    ; Second, udpate position
    LDA ball_position_sub    ; Low 8 bits
    CLC
    ADC ball_speed
    STA ball_position_sub
    LDA sprite_ball+SPRITE_Y ; High 8 bits
    ADC ball_speed+1         ;NB: *don't* clear the carry flag!
    STA sprite_ball+SPRITE_Y

    ; Check for top or bottom of screen
    CMP #SCREEN_BOTTOM_Y     ; Accumulator already contains ball y position
    BCC Updateball_NoClamp
    ; Check sign of speed
    LDA ball_speed+1
    BMI Updateball_ClampToTop
    LDA #SCREEN_BOTTOM_Y-1   ; Clamp to bottom
    JMP Updateball_DoClamping
Updateball_ClampToTop:
    LDA #0                   ; Clamp to top
Updateball_DoClamping:
    STA sprite_ball+SPRITE_Y
    LDA #0                   ; Set ball speed to zero
    STA ball_speed         ; (both bytes)
    STA ball_speed+1
Updateball_NoClamp:

; Check collision with ball
    LDA sprite_player+SPRITE_X, x  ; Calculate x_player - width ball (x1-w2)
    SEC 
    SBC #8      ; Assume w2 = 8
    CMP sprite_ball+SPRITE_X      ; Compare with x_ball (x2)
    BCS UpdateEnemies_NoCollision ; Branch if x1-w2-1-ball_HITBOX_X => x2 i.e. w1-w2 > x2               IT'S DOING THIS
    CLC
    ADC #16  ; Calculate x_player + w_player (x1 + w1), assuming w1 = 8
    CMP sprite_ball+SPRITE_X       ; Compare with x_ball (x2)
    BCC UpdateEnemies_NoCollision  ; Branching if x1+w1 < x2
    LDA sprite_player+SPRITE_Y, x  ; Calculate y_player - height ball (y1-h2)
    SBC #8   ; Assume h2 = 8
    CMP sprite_ball+SPRITE_Y    ; Compare with y_ball (y2)
    BCS UpdateEnemies_NoCollision ; Branch if y1-h2 >= y2
    CLC
    ADC #16 ; Calculate y_player + h_player (y1 + h1), assuming h1 = 8
    CMP sprite_ball+SPRITE_Y    ; Compare with y_ball (y2)
    BCC UpdateEnemies_NoCollision ; Branching if y1+h1 < y2
    ; Handle collision
    NOP    
    LDA #$FF ; Destroy the ball and the player
    STA sprite_ball+SPRITE_Y    
    STA sprite_player+SPRITE_Y, x
UpdateEnemies_NoCollision:



    ; Copy sprite data to the PPU
    LDA #0
    STA OAMADDR
    LDA #$02
    STA OAMDMA

    RTI        ; Return from interrupt

; --------------------------------------------------------------------------
NametableData:
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
    .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .db $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
    .db $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
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
    .incbin "RL.chr"

