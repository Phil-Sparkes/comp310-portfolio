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
JOYPAD1   = $4016
JOYPAD2   = $4017

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
joypad1_state        .rs 1
joypad2_state        .rs 1
nametable_address    .rs 2

player1_speed        .rs 2   ; in subpixels/frame -- 16 bits
player2_speed        .rs 2   ; in subpixels/frame -- 16 bits
player1_position_sub .rs 1   ; in subpixels
player2_position_sub .rs 1   ; in subpixels

ball_speed_x         .rs 2   ; in subpixels/frame -- 16 bits
ball_speed_y         .rs 2   ; in subpixels/frame -- 16 bits
ball_position_sub_x  .rs 1   ; in subpixels
ball_position_sub_y  .rs 1   ; in subpixels

    .rsset $0200
sprite_player1       .rs 4
sprite_player2       .rs 4
sprite_ball          .rs 4
sprite_net1High      .rs 4
sprite_net1Low       .rs 4
sprite_net2High      .rs 4
sprite_net2Low       .rs 4

    .rsset $0000
SPRITE_Y             .rs 1
SPRITE_TILE          .rs 1
SPRITE_ATTRIB        .rs 1
SPRITE_X             .rs 1

GRAVITY                = 9                ; in subpixels/frame^2
JUMP_SPEED             = -(0 * 256 + 196) ; in subpixels/frame
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

    ; Write sprite data for sprite 0 ([Player1])
    LDA #100     ; Y position
    STA sprite_player1 + SPRITE_Y
    LDA #1       ; Tile number
    STA sprite_player1 + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_player1 + SPRITE_ATTRIB
    LDA #30     ; X position
    STA sprite_player1 + SPRITE_X    

    ; Write sprite data for sprite 1 ([Player2])
    LDA #100     ; Y position
    STA sprite_player2 + SPRITE_Y
    LDA #2       ; Tile number
    STA sprite_player2 + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_player2 + SPRITE_ATTRIB
    LDA #230     ; X position
    STA sprite_player2 + SPRITE_X    

    ; Write sprite data for sprite 2 ([BALL])
    LDA #125     ; Y position
    STA sprite_ball + SPRITE_Y
    LDA #3       ; Tile number
    STA sprite_ball + SPRITE_TILE
    LDA #0       ; Attributes
    STA sprite_ball + SPRITE_ATTRIB
    LDA #128     ; X position
    STA sprite_ball + SPRITE_X    

    ; Write sprite data for sprite 3 ([NET1High])
    LDA #156       ; Y position
    STA sprite_net1High + SPRITE_Y
    LDA #$14       ; Tile number
    STA sprite_net1High + SPRITE_TILE
    LDA #0         ; Attributes
    STA sprite_net1High + SPRITE_ATTRIB
    LDA #9         ; X position
    STA sprite_net1High + SPRITE_X    

    ; Write sprite data for sprite 4 ([NET1Low])
    LDA #164      ; Y position
    STA sprite_net1Low + SPRITE_Y
    LDA #$24       ; Tile number
    STA sprite_net1Low + SPRITE_TILE
    LDA #0        ; Attributes
    STA sprite_net1Low + SPRITE_ATTRIB
    LDA #9      ; X position
    STA sprite_net1Low + SPRITE_X    

    ; Write sprite data for sprite 3 ([NET2High])
    LDA #156       ; Y position
    STA sprite_net2High + SPRITE_Y
    LDA #$15       ; Tile number
    STA sprite_net2High + SPRITE_TILE
    LDA #0         ; Attributes
    STA sprite_net2High + SPRITE_ATTRIB
    LDA #247         ; X position
    STA sprite_net2High + SPRITE_X    

    ; Write sprite data for sprite 4 ([NET2Low])
    LDA #164      ; Y position
    STA sprite_net2Low + SPRITE_Y
    LDA #$25       ; Tile number
    STA sprite_net2Low + SPRITE_TILE
    LDA #0        ; Attributes
    STA sprite_net2Low + SPRITE_ATTRIB
    LDA #247      ; X position
    STA sprite_net2Low + SPRITE_X   

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
    LDA sprite_player1 + SPRITE_Y
    CLC
    ADC #-1
    STA sprite_player1 + SPRITE_Y
ReadUp_Done:

    ; React to Down button
    LDA joypad1_state
    AND #BUTTON_DOWN
    BEQ ReadDown_Done  
    LDA sprite_player1 + SPRITE_Y
    CLC
    ADC #1
    STA sprite_player1 + SPRITE_Y
ReadDown_Done:

    ; React to Left button
    LDA joypad1_state
    AND #BUTTON_LEFT
    BEQ ReadLeft_Done 
    LDA sprite_player1 + SPRITE_X
    CLC
    ADC #-1
    STA sprite_player1 + SPRITE_X
ReadLeft_Done:

    ; React to Right button
    LDA joypad1_state
    AND #BUTTON_RIGHT
    BEQ ReadRight_Done  
    LDA sprite_player1 + SPRITE_X
    CLC
    ADC #1
    STA sprite_player1 + SPRITE_X
ReadRight_Done:

 ; React to A button
    LDA joypad1_state
    AND #BUTTON_A
    BEQ ReadA_Done  
    ; Set player speed
    LDA #Low(JUMP_SPEED)
    STA player1_speed
    LDA #HIGH(JUMP_SPEED)
    STA player1_speed+1
ReadA_Done:

    ; Initialise controller 2
    LDA #1
    STA JOYPAD2
    LDA #0
    STA JOYPAD2

    ; Read joypad state
    LDX #0
    STX joypad2_state

; second controller
ReadController2:
    LDA JOYPAD2
    LSR A
    ROL joypad2_state
    INX
    CPX #8
    BNE ReadController2

 ; React to Up button
    LDA joypad2_state
    AND #BUTTON_UP
    BEQ ReadUp2_Done  
    LDA sprite_player2 + SPRITE_Y
    CLC
    ADC #-1
    STA sprite_player2 + SPRITE_Y
ReadUp2_Done:

    ; React to Down button
    LDA joypad2_state
    AND #BUTTON_DOWN
    BEQ ReadDown2_Done  
    LDA sprite_player2 + SPRITE_Y
    CLC
    ADC #1
    STA sprite_player2 + SPRITE_Y
ReadDown2_Done:

    ; React to Left button
    LDA joypad2_state
    AND #BUTTON_LEFT
    BEQ ReadLeft2_Done 
    LDA sprite_player2 + SPRITE_X
    CLC
    ADC #-1
    STA sprite_player2 + SPRITE_X
ReadLeft2_Done:

    ; React to Right button
    LDA joypad2_state
    AND #BUTTON_RIGHT
    BEQ ReadRight2_Done  
    LDA sprite_player2 + SPRITE_X
    CLC
    ADC #1
    STA sprite_player2 + SPRITE_X
ReadRight2_Done:

 ; React to A button
    LDA joypad2_state
    AND #BUTTON_A
    BEQ ReadA2_Done  
    ; Set player speed
    LDA #Low(JUMP_SPEED)
    STA player2_speed
    LDA #HIGH(JUMP_SPEED)
    STA player2_speed+1
ReadA2_Done:

    ; Update player sprite
    ; First, update speed
    LDA player1_speed   ; Low 8 bits
    CLC
    ADC #LOW(GRAVITY)
    STA player1_speed
    LDA player1_speed+1
    ADC #HIGH(GRAVITY) ; High 8 bits
    STA player1_speed+1 ; NB: *don't* clear the carry flag!

    ; Second, udpate position
    LDA player1_position_sub    ; Low 8 bits
    CLC
    ADC player1_speed
    STA player1_position_sub
    LDA sprite_player1+SPRITE_Y ; High 8 bits
    ADC player1_speed+1         ;NB: *don't* clear the carry flag!
    STA sprite_player1+SPRITE_Y

    ; Check for top or bottom of screen
    CMP #SCREEN_BOTTOM_Y     ; Accumulator already contains player y position
    BCC UpdatePlayer_NoClamp
    ; Check sign of speed
    LDA player1_speed+1
    BMI UpdatePlayer_ClampToTop
    LDA #SCREEN_BOTTOM_Y-1   ; Clamp to bottom
    JMP UpdatePlayer_DoClamping
UpdatePlayer_ClampToTop:
    LDA #0                   ; Clamp to top
UpdatePlayer_DoClamping:
    STA sprite_player1+SPRITE_Y
    LDA #0                   ; Set player speed to zero
    STA player1_speed         ; (both bytes)
    STA player1_speed+1
UpdatePlayer_NoClamp:
    ; Update player sprite
    ; First, update speed
    LDA player2_speed   ; Low 8 bits
    CLC
    ADC #LOW(GRAVITY)
    STA player2_speed
    LDA player2_speed+1
    ADC #HIGH(GRAVITY) ; High 8 bits
    STA player2_speed+1 ; NB: *don't* clear the carry flag!

    ; Second, udpate position
    LDA player2_position_sub    ; Low 8 bits
    CLC
    ADC player2_speed
    STA player2_position_sub
    LDA sprite_player2+SPRITE_Y ; High 8 bits
    ADC player2_speed+1         ;NB: *don't* clear the carry flag!
    STA sprite_player2+SPRITE_Y

    ; Check for top or bottom of screen
    CMP #SCREEN_BOTTOM_Y     ; Accumulator already contains player y position
    BCC UpdatePlayer2_NoClamp
    ; Check sign of speed
    LDA player2_speed+1
    BMI UpdatePlayer2_ClampToTop
    LDA #SCREEN_BOTTOM_Y-1   ; Clamp to bottom
    JMP UpdatePlayer2_DoClamping
UpdatePlayer2_ClampToTop:
    LDA #0                   ; Clamp to top
UpdatePlayer2_DoClamping:
    STA sprite_player2+SPRITE_Y
    LDA #0                   ; Set player speed to zero
    STA player2_speed         ; (both bytes)
    STA player2_speed+1
UpdatePlayer2_NoClamp:

    ; Update ball sprite
    ; First, update speed
    LDA ball_speed_y   ; Low 8 bits
    CLC
    ADC #LOW(GRAVITY)
    STA ball_speed_y
    LDA ball_speed_y+1
    ADC #HIGH(GRAVITY) ; High 8 bits
    STA ball_speed_y+1 ; NB: *don't* clear the carry flag!

    ; Second, udpate position
    LDA ball_position_sub_y    ; Low 8 bits
    CLC
    ADC ball_speed_y
    STA ball_position_sub_y
    LDA sprite_ball+SPRITE_Y ; High 8 bits
    ADC ball_speed_y+1         ;NB: *don't* clear the carry flag!
    STA sprite_ball+SPRITE_Y

    ; Check for top or bottom of screen
    CMP #SCREEN_BOTTOM_Y     ; Accumulator already contains ball y position
    BCC Updateball_NoClampY
    ; Check sign of speed
    LDA ball_speed_y+1
    BMI Updateball_ClampToTop
    LDA #SCREEN_BOTTOM_Y-1   ; Clamp to bottom
    JMP Updateball_DoClampingY
Updateball_ClampToTop:
    LDA #0                   ; Clamp to top
Updateball_DoClampingY:
    STA sprite_ball+SPRITE_Y
    LDA #0
    SEC
    SBC ball_speed_y 
    STA ball_speed_y  
    LDA #0
    SEC
    SBC ball_speed_y+1         
    STA ball_speed_y+1
Updateball_NoClampY:

    LDA ball_speed_x                 ; load speed
    CLC
    ADC sprite_ball+SPRITE_X         ; add speed to sprite
    STA sprite_ball+SPRITE_X         ; save sprite location

  ; Check for side of screen
    CMP #0     ; Accumulator already contains ball y position
    BNE Updateball_NoClampX
    JMP Updateball_DoClampingX                ; Clamp to top
Updateball_DoClampingX:
    LDA sprite_ball+SPRITE_Y
    CMP #158
    BPL Updateball_Score
    LDA #0
    SEC
    SBC ball_speed_x
    STA ball_speed_x        
    JMP Updateball_NoClampX 
Updateball_Score:
    LDA #0
    ball_speed_x
Updateball_NoClampX:


                              ;             \1        \2        \3                     
CheckCollisionWithBall .macro ; parameters: player_x, player_y, no_collision_label
; Check collision with ball
    LDA \1 ;sprite_player1+SPRITE_X      ; Calculate x_player - width ball (x1-w2)
    SEC 
    SBC #8                           ; Assume w2 = 8
    CMP sprite_ball+SPRITE_X         ; Compare with x_ball (x2)
    BCS \3 ;UpdateBall_NoCollision       ; Branch if x1-w2-1-ball_HITBOX_X => x2 i.e. w1-w2 > x2            
    CLC
    ADC #16                          ; Calculate x_player + w_player (x1 + w1), assuming w1 = 8
    CMP sprite_ball+SPRITE_X         ; Compare with x_ball (x2)
    BCC \3 ;UpdateBall_NoCollision       ; Branching if x1+w1 < x2
    LDA \2 ;sprite_player1+SPRITE_Y       ; Calculate y_player - height ball (y1-h2)
    SBC #8                           ; Assume h2 = 8
    CMP sprite_ball+SPRITE_Y         ; Compare with y_ball (y2)
    BCS \3 ;UpdateBall_NoCollision       ; Branch if y1-h2 >= y2
    CLC
    ADC #16                          ; Calculate y_player + h_player (y1 + h1), assuming h1 = 8
    CMP sprite_ball+SPRITE_Y         ; Compare with y_ball (y2)
    BCC \3 ;UpdateBall_NoCollisio22n       ; Branching if y1+h1 < y2
    
    ; Handle collision
    LDA #254
    STA ball_speed_y+1
    ;LDA #254                          
    STA ball_speed_x                 ; set speed
    LDA sprite_ball+SPRITE_X         ; get ball position
    CMP \1 ;sprite_player1+SPRITE_X
    BMI \3 ;UpdateBall_CollisionRight    ; collision from right

    LDA #02                          ; subtract player position
    STA ball_speed_x                 ; set speed as result
    .endm    

    CheckCollisionWithBall sprite_player1+SPRITE_X, sprite_player1+SPRITE_Y, UpdateBall_NoCollision 
UpdateBall_NoCollision: 
   CheckCollisionWithBall sprite_player2+SPRITE_X, sprite_player2+SPRITE_Y, UpdateBall_NoCollision2 
UpdateBall_NoCollision2: 

    ; Copy sprite data to the PPU
    LDA #0
    STA OAMADDR
    LDA #$02
    STA OAMDMA

    RTI        ; Return from interrupt

; --------------------------------------------------------------------------
NametableData:
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
    .db $30,$30,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$31,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$33
    .db $40,$60,$61,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$41,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$62,$63
    .db $40,$40,$43,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$10,$11,$12,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$40,$43
    .db $40,$40,$43,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$20,$21,$22,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$40,$43
    .db $40,$70,$71,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$41,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$42,$72,$73
    .db $50,$50,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$51,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$52,$53
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
    .db $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
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

