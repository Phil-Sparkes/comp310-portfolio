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

ENEMY_SQUAD_WIDTH    = 6
ENEMY_SQUAD_HEIGHT   = 4
NUM_ENEMIES          = ENEMY_SQUAD_WIDTH * ENEMY_SQUAD_HEIGHT
ENEMY_SPACING        = 16
ENEMY_DESCENT_SPEED  = 4

ENEMY_HITBOX_WIDTH   = 8
ENEMY_HITBOX_HEIGHT  = 8
BULLET_HITBOX_X      = 3
BULLET_HITBOX_Y      = 1
BULLET_HITBOX_WIDTH  = 8
BULLET_HITBOX_HEIGHT = 8

    .rsset $0000
joypad1_state       .rs 1
bullet_active       .rs 1
temp_x              .rs 1
temp_y              .rs 1
enemy_info          .rs 4 * NUM_ENEMIES

    .rsset $0200
sprite_player       .rs 4
sprite_bullet       .rs 4
sprite_enemy        .rs 4 * NUM_ENEMIES

    .rsset $0000
SPRITE_Y            .rs 1
SPRITE_TILE         .rs 1
SPRITE_ATTRIB       .rs 1
SPRITE_X            .rs 1

    .rsset $0000
ENEMY_SPEED         .rs 1
ENEMY_ALIVE         .rs 1


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
    
    ; Reset the PPU high/low latch
    LDA PPUSTATUS

    ; Write address 3F10 (background colour) to the PPU
    LDA #$3F
    STA PPUADDR
    LDA #$10
    STA PPUADDR

    ; Write the background colour
    LDA #$0
    STA PPUDATA

    ; Write the palette colours
    LDA #$30
    STA PPUDATA
    LDA #$19
    STA PPUDATA
    LDA #$05
    STA PPUDATA

    LDA #$0
    STA PPUDATA
    LDA #$25
    STA PPUDATA
    LDA #$13
    STA PPUDATA
    LDA #$07
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

    ; Initialise enemies
    LDX #0
    LDA #ENEMY_SQUAD_HEIGHT * ENEMY_SPACING
    STA temp_y
InitEnemies_LoopY:
    LDA #ENEMY_SQUAD_WIDTH * ENEMY_SPACING
    STA temp_x
InitEnemies_LoopX:
    ; Accumulator = temp_x here
    STA sprite_enemy + SPRITE_X, x 
    LDA temp_y
    STA sprite_enemy+SPRITE_Y, x 
    LDA #3
    STA sprite_enemy+SPRITE_TILE, x 
    LDA #0
    STA sprite_enemy+SPRITE_ATTRIB, x
    LDA #1
    STA enemy_info+ENEMY_SPEED, x 
    STA enemy_info+ENEMY_ALIVE, x 
    ; Increment x by 4
    INX
    INX
    INX
    INX
    ; loop check for x value
    LDA temp_x
    SEC
    SBC #ENEMY_SPACING
    STA temp_x
    BNE InitEnemies_LoopX
    ; loop check for y value
    LDA temp_y
    SEC
    SBC #ENEMY_SPACING
    STA temp_y
    BNE InitEnemies_LoopY


    LDA  #%10000000 ; Enable NMI
    STA PPUCTRL

    LDA #%00010000  ; Enable sprites
    STA PPUMASK

    ; Enter an infinite loop
forever:
    JMP forever

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
    ; Spawn a bullet if one is not active
    LDA bullet_active
    BNE ReadA_Done
    ; no bullet active, so spawn one
    LDA #1
    STA bullet_active
    LDA sprite_player + SPRITE_Y    ; Y position
    STA sprite_bullet+ SPRITE_Y
    LDA #16                         ; Tile number
    STA sprite_bullet + SPRITE_TILE
    LDA #0                          ; Attributes
    STA sprite_bullet + SPRITE_ATTRIB
    LDA sprite_player + SPRITE_X    ; X position
    STA sprite_bullet + SPRITE_X    
ReadA_Done:

    ; Update the bullet
    LDA bullet_active
    BEQ UpdateBullet_Done
    LDA sprite_bullet + SPRITE_Y
    SEC 
    SBC #1
    STA sprite_bullet + SPRITE_Y
    BCS UpdateBullet_Done
    ; if carry play is clear, bullet has left the top of the screen - destroy it
    LDA #0
    sta bullet_active
UpdateBullet_Done:

    ; Update enemies
    LDX #(NUM_ENEMIES-1)*4
UpdateEnemies_Loop:
    ; Check if enemy is alive
    LDA enemy_info+ENEMY_ALIVE, x 
    BEQ UpdateEnemies_Next
    LDA sprite_enemy+SPRITE_X, x 
    CLC
    ADC enemy_info+ENEMY_SPEED, x 
    STA sprite_enemy+SPRITE_X, x 
    CMP #256 - ENEMY_SPACING
    BCS UpdateEnemies_Reverse
    CMP #ENEMY_SPACING
    BCC UpdateEnemies_Reverse
    JMP UpdateEnemies_NoReverse
UpdateEnemies_Reverse:
    ; Reverse direction and descend
    LDA #0
    SEC
    SBC enemy_info+ENEMY_SPEED, x 
    STA enemy_info+ENEMY_SPEED, x 
    LDA sprite_enemy+SPRITE_Y, x 
    CLC
    ADC #ENEMY_DESCENT_SPEED
    STA sprite_enemy+SPRITE_Y, x
    LDA sprite_enemy+SPRITE_ATTRIB, x 
    EOR #%01000000
    STA sprite_enemy+SPRITE_ATTRIB, x 
UpdateEnemies_NoReverse:
    ; Check collision with bullet
    LDA sprite_enemy+SPRITE_X, x  ; Calculate x_enemy - width bullet (x1-w2)
    SEC 
    SBC #BULLET_HITBOX_X
    SEC
    SBC #BULLET_HITBOX_WIDTH+1    ; Assume w2 = 8
    CMP sprite_bullet+SPRITE_X    ; Compare with x_bullet (x2)
    BCS UpdateEnemies_NoCollision ; Branch if x1-w2-1-BULLET_HITBOX_X => x2i.e. w1-w2 > x2
    CLC
    ADC #BULLET_HITBOX_WIDTH+1+ENEMY_HITBOX_WIDTH  ; Calculate x_enemy + w_enemy (x1 + w1), assuming w1 = 8
    CMP sprite_bullet+SPRITE_X    ; Compare with x_bullet (x2)
    BCC UpdateEnemies_NoCollision ; Branching if x1+w1 < x2
    LDA sprite_enemy+SPRITE_Y, x  ; Calculate y_enemy - height bullet (y1-h2)
    SEC 
    SBC #BULLET_HITBOX_Y
    SEC
    SBC #BULLET_HITBOX_HEIGHT+1   ; Assume h2 = 8
    CMP sprite_bullet+SPRITE_Y    ; Compare with y_bullet (y2)
    BCS UpdateEnemies_NoCollision ; Branch if y1-h2 >= y2
    CLC
    ADC #BULLET_HITBOX_HEIGHT+1+ENEMY_HITBOX_HEIGHT ; Calculate y_enemy + h_enemy (y1 + h1), assuming h1 = 8
    CMP sprite_bullet+SPRITE_Y    ; Compare with y_bullet (y2)
    BCC UpdateEnemies_NoCollision ; Branching if y1+h1 < y2
    ; Handle collision
    NOP
    LDA #0                        ; Destroy the bullet and the enemy
    STA bullet_active   
    STA enemy_info+ENEMY_ALIVE, x       
    LDA #$FF
    STA sprite_bullet+SPRITE_Y    
    STA sprite_enemy+SPRITE_Y, x
UpdateEnemies_NoCollision:
UpdateEnemies_Next:
    DEX
    DEX
    DEX
    DEX
    BPL UpdateEnemies_Loop

    ; Copy sprite data to the PPU
    LDA #0
    STA OAMADDR
    LDA #$02
    STA OAMDMA

    RTI        ; Return from interrupt

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

