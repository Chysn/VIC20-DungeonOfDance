;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                               Dungeon of Dance
;                            (c)2021, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4110
* = $1001
Launcher:   .byte $0b,$10,$2a,$00,$9e,$34,$31,$31
            .byte $30,$00,$00,$00,$00


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Configuration
HEIGHT      = 17                ; Size of maze, with each cell having a "wall"
WIDTH       = 22                ;   surrounding it. 16x16 maze has 8x8 cells
Y_OFFSET    = 6                 ; Y offset of maze
SEED        = $c100             ; Start of pseudo-random data table
HP_ARMOR    = 5                 ; Max HP per armor
ST_ENERGY   = 200               ; Starting energy (loss of 1 per move)
HP_ENERGY   = 50                ; Energy gained per HP consumed
FOOD_ENERGY = 50                ; Energy gained per food found
POTION_HP   = 10                ; Max HP gained per potion used
MAX_ITEMS   = 8                 ; Maximum armor and potions (each)

; Characters
CHR_WALL    = $20               ; Wall character
CHR_OPEN    = $a0               ; Open character
CHR_DOOR    = $1f               ; Door character
CHR_POTION  = $3a               ; Potion character
CHR_ARMOR   = $3b               ; Armor character
CHR_FOOD    = $3c               ; Food character
CHR_GOBLIN  = $3d               ; Goblin character
CHR_WRAITH  = $3e               ; Wraith character
CHR_DRAGON  = $3f               ; Dragon character
CHR_PL_L    = $19               ; Player facing left
CHR_PL_R    = $1a               ; Player facing right
CHR_KEY     = $1b               ; Key character
CHR_FILL    = $1c               ; Dungeon fill (all directions)
CHR_READY   = $02               ; Ready to dance!

; Colors
COL_DOOR    = 3                 ; Door is cyan
COL_WALL    = 7                 ; Wall is yellow
COL_POTION  = 1                 ; Potion is white
COL_ARMOR   = 1                 ; Armor is white
COL_FOOD    = 5                 ; Food is green
COL_GOBLIN  = 4                 ; Goblin is purple
COL_WRAITH  = 1                 ; Wraith is white
COL_DRAGON  = 2                 ; Dragon is red

; Constants
NORTH       = 1                 ; Compass Points, used in bitfields for
EAST        = 2                 ;   specifying GROUPS of directions
SOUTH       = 4                 ;   ,,
WEST        = 8                 ;   ,,
JS_NORTH    = 1                 ; Direction codes, used by the joystick
JS_EAST     = 2                 ;   routine, and in various tables that
JS_SOUTH    = 3                 ;   index a specific direction
JS_WEST     = 4                 ;   ,,
JS_FIRE     = 5                 ; Oh, and Fire, which in this game means "drink"

; System Resources
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eb12             ; System ISR return point
HORIZ       = $9000             ; Screen position
VERT        = $9001             ; ,,
VICCR5      = $9005             ; Character map register
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register
FX_VOICE    = $900c             ; Sound effects voice
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color
SCRCOL      = $900f             ; Screen color
VIA1DD      = $9113             ; Data direction register for joystick
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA2DD      = $9122             ; Data direction register for joystick
VIA2PB      = $9120             ; Joystick port (for right)
PRTSTR      = $cb1e             ; Print from data (Y,A)
CASECT      = $0291             ; Disable Commodore case
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
TIME_L      = $a2               ; Jiffy counter low
TIME_M      = $a1               ; Jiffy counter middle
BASRND      = $e094             ; Routine for BASIC's RND() function
RNDNUM      = $8d               ; Result storage location for RND()
CLSR        = $e55f             ; Clear screen/home
PLOT        = $fff0             ; PLOT 
CHROUT      = $ffd2             ; Output one character

; Memory
CHANGED     = $05               ; Non-energy resource quantity has changed
SCRPAD      = $f8               ; Value scratchpad
COOR_Y      = $f9               ; y coordinate
COOR_X      = $fa               ; x coordinate
PTR         = $fb               ; Screen pointer (2 bytes)
LEVELS      = $fd               ; Number of levels on the stack
P_RAND      = $a3               ; Pointer to pseudorandom table (2 bytes)
XP          = $b0               ; Experience points (2 bytes)
HP          = $b2               ; Hit points
ENERGY      = $b3               ; Energy
POTIONS     = $b4               ; Potions available
ARMOR       = $b5               ; Armor (Max HP = Armor * 4)
LEVEL       = $b6               ; Dungeon/Character Level
KEYSTAT     = $06               ; Flag for key (bit 7=has key, bit 6=found door)
JOYREG      = $10               ; Joystick register
UNDER       = $12               ; Character under player
COL_PTR     = $13               ; Color pointer (2 bytes)
TMP_COOR_Y  = $13               ; Temporary y coordinate
TMP_COOR_X  = $14               ; Temporary x coordinate
PLAYER_CH   = $15               ; Current player graphic
LAST_ENC    = $fe               ; Last monster encounter
TMP_IX      = $07               ; Temporary index
PATT_LEN    = $a5               ; Pattern length
PATTERN     = $033c             ; Current pattern
ENC_PTR     = $00               ; Monster encounter save

; Sound Memory
FX_REG      = $a6               ; Current effect frequency register
FX_LEN      = $a7               ; Current effect length
FX_COUNT    = $a8               ; Effect countdown for current effect
FX_DIR      = $a9               ; Effect direction ($00 = left, $80 = right)
FX_SPEED    = $aa               ; Effect countdown reset value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Welcome:    jsr SetupHW         ; Set up hardware
            jsr CLSR            ; Clear the screen
            lda #<Name          ; Show Name
            ldy #>Name          ; ,,
            jsr PRTSTR          ; ,,
            lda #<Intro         ; Show Intro
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,
            lda #7              ; Set maze color
            jsr WipeColor       ; ,,
            lda #$00            ; Create introductory maze
            sta VOICEH          ; Shut off voices
            sta VOICEM          ; ,,
            sta VOICEL          ; ,,
            sta NOISE           ; ,,
            sta P_RAND          ; Create introductory maze
            lda #>SEED          ; ,,
            sta P_RAND+1        ; ,,
            jsr MakeMaze        ; ,,
            lda #0              ; Reset KEYSTAT so that dragon appears
            sta KEYSTAT         ; ,,
            jsr DragDoor        ; Show dragon and door
            lda #CHR_PL_R       ; Place the player
            sta $1e9b           ; ,,
            lda #5              ; Make the player green
            sta $969b           ; ,,
            ; Fall through to game start
            
; Start a new game            
Start:      jsr Wait4Fire
            jsr InitGame
            ; Fall through to level start
            
; Start next level            
NextLevel:  jsr InitLevel
            ; Fall through to main action loop

; Main action loop            
Main:       lda KEYSTAT         ; Check for level completion with bit 5 of
            cmp #%00100000      ;   KEYSTAT
            beq LevelUp         ;   ,,
            lda HP              ; Check for player death
            beq QuestOver       ; ,,
read_js:    jsr Joystick        ; Wait for joystick
            beq read_js         ; ,,
            cmp #JS_FIRE        ; Use a potion if fire button was pressed
            beq UsePotion       ; ,,
            pha                 ; Save direction
            ldy #CHR_PL_R       ; Set player graphic (left or right-facing)
            cmp #JS_EAST        ;   if the player moves east or west.
            beq set_char        ;   Otherwise, leave the player graphic
            cmp #JS_WEST        ;   as it is.
            bne leave_char      ;   ,,
            dey                 ;   ,,
set_char:   sty PLAYER_CH       ;   ,, 
leave_char: lda UNDER           ; Replace the character under the player as
            jsr PlotChar        ;   the player leaves the cell
            ldx COOR_X          ; Set temporary coordinates, in case this
            stx TMP_COOR_X      ;   move needs to be reverted
            ldx COOR_Y          ;   ,,
            stx TMP_COOR_Y      ;   ,,
            pla                 ; Get direction back
            pha                 ; Save the direction for playing the tone
            tax                 ; Convert joystick code to compass point
            lda Compass,x       ; ,,
            jsr MoveCoor        ; Move coordinate based on compass point
            jsr DecEnergy       ; Use one energy for the movement
            jsr Encounter       ; Process encounter
            jsr DrawPlayer      ; Draw the player
            pla                 ; Play the tone in A
            ldy #8              ; ,,
            jsr DirTone         ; ,,
-debounce:  jsr Joystick        ; Debounce the joystick
            bne debounce        ; ,,
            beq Main
 
; Quest Over        
QuestOver:  lda #<GameOver      ; Show Game Over notification
            ldy #>GameOver      ; ,,
            jsr PRTSTR          ; ,,
            lda #110            ; Set screen color to reveal maze
            sta SCRCOL          ; ,,
            lda #CHR_OPEN       ; Disappear the player
            jsr PlotChar        ; ,,
            jmp Start           ; Back to wait for a new game
            
; Level Up
LevelUp:    lda #6              ; Launch level up effect
            jsr FXLaunch        ; ,,
            lda #7              ; Show the rest of the maze
            jsr WipeColor       ; ,,
            lda #5              ; Earn 5XP per level
            jsr LevelMult       ; ,,
            jsr IncXP           ; ,,
            jsr ShowScore       ; Show the score with the new XP
            lda #180            ; Delay for 3 seconds
            jsr Delay           ; ,,
            jmp NextLevel       ; before starting the next level
     
; Use Potion            
UsePotion:  lda POTIONS         ; Are there any potions to drink?
            bne potion_dec      ; If so, drink one
            jmp Main            ; Otherwise, back to Main
potion_dec: dec POTIONS         ; Take away one potion
            bit TIME_L          ; Check bit 6 of TIME_L, which should be lit up
            bvs Transport       ;   25% of the time. In this case, teleport
AddHP:      lda #05             ; Launch healing potion effect
            jsr FXLaunch        ; ,,
            lda #POTION_HP      ; Add HP based on configuration value
            jsr IncHP           ; ,,
            jsr update_sc       ; Update and show score
Transport:  lda #4              ; Launch transport out effect
            jsr FXLaunch        ; ,,
            lda #CHR_OPEN       ; Disappear for a bit...
            sta UNDER           ; ,,
            jsr PlotChar        ; ,,
new_x:      jsr BASRND          ; Find a new X location...
            lda RNDNUM          ; ,,
            cmp #WIDTH          ; ...until it's in range
            bcs new_x           ; ,,
            sta COOR_X          ; Set it
new_y:      jsr BASRND          ; Find a new Y location...
            lda RNDNUM          ; ,,
            cmp #HEIGHT         ; ...until it's in range
            bcs new_y           ; ,,
            sta COOR_Y          ; Set it
            jsr Coor2Ptr        ; Convert new coordinates to 16-bit pointer
            ldx #0              ; See what's currently there
            lda (PTR,x)         ; ,,
            cmp #CHR_OPEN       ; If the new location is occupied, then try
            bne new_x           ;   again
            jsr DrawPlayer      ; If unoccupied, draw the player there
update_sc:  sec                 ; Mark the scoresheet changed, so that the
            ror CHANGED         ;   number of potions in inventory is refreshed
            jsr ShowScore       ; Show the score header
            jmp debounce         
                        
; Interrupt Service Routine            
ISR:        inc TIME_L          ; Circumventing BASIC's clock, so advance it
            jsr FXService       ; Play any sound effects that are going on
            lda KEYSTAT         ; If the dragon hasn't already been defeated,
            bne isr_r           ;   flash the dragon half the time
            bit TIME_L          ;   ,,
            bmi flash_dr        ;   ,,
            lda #COL_DRAGON     ; The other half of the time, keep the dragon
            bne paint_dr        ;   red
flash_dr:   lda $9755           ; Get the dragon's current color
            eor #$07            ; 010 => 101, and back again
paint_dr:   sta $9755           ; ,,
isr_r:      jmp IRQ


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLAY MECHANICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw Player
; At COOR_X, COOR_Y
DrawPlayer: jsr ShowScore       ; Show changed values
            jsr DragDoor        ; Show dragon and door
            lda PLAYER_CH       ; Draw the player
            jsr PlotChar        ; ,,
            lda #5              ; And set its color to green
            jsr SetColor        ; ,,
            ; Fall through to Automap

; Automap
; All 8 cells adjacent to PTR
Automap:    lda PTR
            sec
            sbc #23
            sta PTR
            bcs light
            dec PTR+1
light:      ldy #7
-loop:      lda Autopatt,y
            clc
            adc PTR
            sta PTR
            bcc find_col
            inc PTR+1
find_col:   ldx #0              ; Get character at pointer position
            lda (PTR,x)         ; ,,
            cmp #CHR_OPEN       ; If it's an open space, do nothing
            beq skip_col        ; ,,
            cmp #$3a            ; Indexed items start at $3a
            bcs color_item      ; If it's an indexed item, find its color
            lda #COL_WALL       ; Otherwise, it's a wall
            bne set_color       ; ,,
color_item: sec                 ; Subtract $3a to get the color index
            sbc #$3a            ; ,,
            tax                 ; Put the color index in X
            lda ItemColors,x    ;   and get the color
set_color:  jsr SetColor        ; Set color at PTR to A
skip_col:   dey
            bpl loop
            rts
           
; Show Dragon and Door            
DragDoor:   lda #CHR_DOOR       ; Place the door, always in the same position
            sta $1fe3           ; ,,
            lda #COL_DOOR       ; Make the door cyan
            sta $97e3           ; ,,
            lda KEYSTAT         ; If the dragon has been defeated, do not
            bne dragdoor_r      ;   display
            lda #CHR_DRAGON     ; Place the dragon, always in the same position
            sta $1f55           ; ,,
            lda #COL_DRAGON     ; Make the dragon red
            sta $9755           ; ,,
dragdoor_r: rts
            
; Show Score
; And inventory
ShowScore:  bit CHANGED         ; If nothing has changed, only show energy
            bmi show_all        ;   to save some time
            jmp ShowEnergy      ;   ,,
show_all:   lsr CHANGED         ; Clear the CHANGED flag
            lda #<Name          ; Show Name
            ldy #>Name          ; ,,
            jsr PRTSTR          ; ,,
            lda #<Labels        ; Show Score Labels
            ldy #>Labels        ; ,,
            jsr PRTSTR          ; ,,
            bit KEYSTAT         ; Show the key if the dragon has been defeated
            bvc lev_num         ; ,,
            lda #CHR_KEY        ; ,,
            sta $1e62           ; ,,
            lda #7              ; ,,
            sta $9662           ; ,,
lev_num:    ldy #4              ; Show the level number
            ldx #2              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx LEVEL           ; ,,
            lda #0              ; ,,
            jsr PRTFIX          ; ,,
            ldy #4              ; Show hit points
            ldx #3              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx HP              ; ,,
            lda #0              ; ,,
            jsr PRTFIX          ; ,,
            lda #$20            ; Space at the end to deal with decrease
            jsr CHROUT          ; ,,
            ldy #15             ; Show XP
            ldx #2              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            lda XP              ; (Check if XP is zero. If it is, only show
            bne get_xp          ;   the decorative multiplier below
            lda XP+1            ;   ,,
            beq xp_zero         ;   ,,)
get_xp:     ldx XP              ; ,,
            lda XP+1            ; ,,
            jsr PRTFIX          ; ,,
xp_zero:    lda #"0"            ; Multiply XP by 10. Why not?
            jsr CHROUT          ; ,,
            ldy #1              ; Show Armor
            ldx #4              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            lda #$05            ; White
            jsr $ffd2           ; ,,
            ldy ARMOR           ; ,,
            lda #";"            ; ,,
-loop:      jsr CHROUT          ; ,,
            dey                 ; ,,
            bne loop            ; ,,
            ldy #12             ; Show Potions
            ldx #4              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            lda #$05            ; White
            jsr $ffd2           ; ,,
            ldy POTIONS         ; ,,
            beq no_potion       ; ,,
            lda #":"            ; ,,
-loop:      jsr CHROUT          ; ,,
            dey                 ; ,,
            bne loop            ; ,,
no_potion:  lda #$20            ; Space to handle potion use
            jsr CHROUT          ; ,,
            ; Fall through to ShowEnergy

; Show only energy
; To save time when nothing else has changed                        
ShowEnergy: lda #$1e            ; Make color cyan
            jsr CHROUT          ; ,,
            ldy #15
            ldx #3
            clc
            jsr PLOT
            ldx ENERGY
            lda #0
            jsr PRTFIX
            lda #$20            ; Space at the end to deal with decreases
            jmp CHROUT          ; ,,
          
; Increase Energy
; By amount specified in A
; Observing maximum of ST_ENERGY            
IncEnergy:  clc
            adc ENERGY
            sta ENERGY
            cmp #ST_ENERGY
            bcc incen_r
            lda #ST_ENERGY
            sta ENERGY
incen_r:    rts

; Decrease Energy
; By 1
; If hits 0, then one HP is spent to add energy
DecEnergy:  dec ENERGY
            bne decen_r
            lda #1
            jsr DecHP
            lda #HP_ENERGY
            jsr IncEnergy
            lda #7
            jmp FXLaunch        ; Launch HP to Energy effect
decen_r:    rts 

; Increase Hit Points
; By amount specified in A
; Observing maximum of HP_AMOUNT x ARMOR
IncHP:      sec                 ; Set the flag indicating HP change
            ror CHANGED         ; ,,
            clc
            adc HP
            sta HP
            lda #0              ; Calculate maximum HP
            ldy ARMOR           ; ,,
            clc                 ; ,,
hp_mult:    adc #HP_ARMOR       ; ,,
            dey                 ; ,,
            bne hp_mult         ; ,,
            cmp HP              ; If HP is equal to or greater than
            bcs inchp_r         ;   the calculated maximum, constrain
            sta HP              ;   HP to the maximum
inchp_r:    rts            

; Decrease Hit Points
; By amount specified in A
DecHP:      sec                 ; Set the flag indicating HP change
            ror CHANGED         ; ,,
            sta SCRPAD          ; Subtract A from HP
            lda HP              ; ,,
            sec                 ; ,,
            sbc SCRPAD          ; ,,
            sta HP              ; Store HP
            bpl dechp_r         ; If HP is positive, return
            lda #0              ; Otherwise, set to 0
            sta HP              ; ,,
dechp_r:    rts

; Increase Experience Points
; By amount specified in A
IncXP:      sec                 ; Set the flag indicating HP change
            ror CHANGED         ; ,,
            clc
            adc XP
            sta XP
            bcc incxp_r
            inc XP+1
incxp_r:    rts            
                                    
; Encounter
; Dispatch various mechanics based on the newly-entered cell
Encounter:  jsr Coor2Ptr        ; Update pointer based on new coordinates
            ldx #0              ; What's in the new cell?
            lda (PTR,x)         ; ,,
            cmp #CHR_DOOR       ; Process door
            bne enc_key         ;   ,,
            bit KEYSTAT         ;   If the key has been picked up on this level,
            bvc locked          ;   bit 5 of KEYSTAT will be set. Otherwise, the
            lsr KEYSTAT         ;   door is considered "locked," and nothing
locked:     rts                 ;   will happen.
enc_key:    cmp #CHR_KEY        ; Process key
            bne enc_wall        ;   If the key is encountered, launch the key
            lda #8              ;   sound effect...
            jsr FXLaunch        ;   ,,
            jsr RemoveItem      ;   ...remove the key from the maze
            lsr KEYSTAT         ;   then set bit 5 so door can be unlocked
            rts                 ;   ,,
enc_wall:   cmp #$3a            ; Process an item
            bcs enc_item        ; ,,
Revert:     lda TMP_COOR_Y      ; Process a wall by reverting to the previous
            sta COOR_Y          ;   cell
            lda TMP_COOR_X      ;   ,,
            sta COOR_X          ;   ,,
            lda #$01            ; Compensate for the lost energy of movement
            jmp IncEnergy       ; ,,
enc_item:   sta UNDER           ; Set the item as under the player now
            cmp #CHR_OPEN       ; Just walking into empty passageway
            beq enc_r           ; ,,
            cmp #CHR_POTION     ; Handle potion acquisition
            bne chk_armor       ; ,,
            lda #0              ; Launch pick-up effect
            jsr FXLaunch        ; ,,
            inc POTIONS         ; ,,
            lda POTIONS         ; ,,
            cmp #MAX_ITEMS+1    ; Enforce maximum number of potions
            bcc potion_r        ; ,,
            lda #MAX_ITEMS      ; ,,
            sta POTIONS         ; ,,
            rts                 ; ,,
potion_r:   jmp RemoveItem      ; If a potion was picked up, remove it
chk_armor:  cmp #CHR_ARMOR      ; Handle armor acquisition
            bne chk_food        ; ,,
            lda #0              ; Launch pick-up effect
            jsr FXLaunch        ; ,,
            inc ARMOR           ; ,,
            lda ARMOR           ; ,,
            cmp #MAX_ITEMS+1    ; Enforce maximum number of armor
            bcc armor_r         ; ,,
            lda #MAX_ITEMS      ; ,,
            sta ARMOR           ; ,,
            rts                 ; ,,
armor_r:    lda #HP_ARMOR       ; Increase HP with new armor
            jsr IncHP           ; ,,
            jmp RemoveItem      ; If armor was picked up, remove it
chk_food:   cmp #CHR_FOOD       ; Handle food acquisition
            bne enc_monst       ; ,,
            lda #3              ; Launch eat food effect
            jsr FXLaunch        ; ,,
            lda #FOOD_ENERGY    ; ,,
            jsr IncEnergy       ; ,,
            jmp RemoveItem      ; If food was picked up, remove it
enc_monst:  cmp #CHR_GOBLIN     ; Handle
            bcs DanceOff
enc_r:      rts
                        
; Remove Item
; Set the UNDER value to open, so when the player moves off it, it will be gone
RemoveItem: lda #CHR_OPEN
            sta UNDER
            sec
            ror CHANGED
            rts
 
; Return Level * A
; In A           
LevelMult:  sta SCRPAD
            ldy LEVEL
            clc
            lda #0
mult_xp:    adc SCRPAD
            dey
            bne mult_xp
            rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DANCE OFF (COMBAT) ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
; Dance Off
DanceOff:   jsr PattMake
            ldy PTR             ; Store the monster position, so it can be
            sty ENC_PTR         ;   cleared if the monster is defeated
            ldy PTR+1           ;   ,,
            sty ENC_PTR+1       ;   ,,
            jsr Revert          ; Put the player back where he or she was
            jsr Coor2Ptr        ;   in order to display the dance moves
            jsr PattPlay        ; Play the pattern and listen for response.
            bne lose            ;   Z flag is set if player won
            lda LAST_ENC        ; Get the monster strength
            pha
win:        lda #1              ; Launch victory effect
            jsr FXLaunch        ; ,,
inc_xp:     jsr LevelMult       ; Increase XP based on level
            jsr IncXP           ; ,,            
            lda #CHR_OPEN       ; Remove the vanquished foe from the dungeon
            sta UNDER           ; ,,
            ldx #0              ; ,,
            sta (ENC_PTR,x)     ; ,,
            pla
            cmp #$04            ; Is this the dragon?
            bne ch_wraith       ; ,,
            sec                 ; If so, set the KEYSTAT flag when the dragon
            ror KEYSTAT         ;   has been defeated
            lda #CHR_KEY        ; Put the key on the screen
            sta $1f55           ; ,,
ch_wraith:  cmp #2              ; Wraiths don't leave the maze when defeated,
            bne reset_enc       ;   they just pick another spot to haunt
            lda #CHR_WRAITH     ;   ,,
            sec                 ;   ,,
            jsr Position        ;   ,,
            tay                 ;   ,,
            lda #$78            ; Set the color of the moved wraith to black
            clc                 ; ,,
            adc PTR+1           ; ,,
            sta PTR+1           ; ,,
            lda #0              ; ,,
            sta (PTR),y         ; ,,
reset_enc:  lda #0              ; Reset last encounter so there's a new pattern
            sta LAST_ENC        ;   for the next monster of this kind
            rts
lose:       lda #2              ; Launch defeat effect
            jsr FXLaunch        ; ,,
            lda #10             ; Red border = defeat
            sta SCRCOL          ; ,,
            inc HORIZ           ; Rock the screen a bit, because you lost
            jsr Delay2          ; ,,
            dec VERT            ; ,,
            jsr Delay2          ; ,,
            dec HORIZ           ; ,,
            jsr Delay2          ; ,,
            inc VERT            ; ,,
            jsr Delay2          ; ,,
            dec HORIZ           ; ,,
            jsr Delay2          ; ,,
            inc HORIZ           ; ,,
            jsr Delay2          ; ,,
            inc VERT            ; ,,
            jsr Delay2          ; ,,
            dec VERT            ; ,,
            jsr Delay2          ; ,,
            lda LAST_ENC        ; Multiply monster strength times level number
            jsr LevelMult       ; ,,
            and TIME_L          ; AND with the jiffy clock to possibly remove
            ora LAST_ENC        ;   bits, then ORA with the monster strength for
            jsr DecHP           ;   a minimum HP loss
            lda #CHR_OPEN       ; Clear the cell under the player
            sta UNDER           ; ,,
            lda LAST_ENC        ; If the player was defeated by a wraith, there
            cmp #2              ;   is the possibility of additional mischief.
            bne lose_r          ;   About a quarter of the time, the wraith will
            bit TIME_L          ;   take away the player's map
            bvc lose_r          ;   ,,
            lda #0              ;   ,,
            jsr WipeColor       ;   ,,
lose_r:     lda #15             ; Put screen color back
            sta SCRCOL          ; ,,
            rts
            
PattMake:   sec                 ; Set encounter to monster level, where
            sbc #CHR_GOBLIN-1   ; 1 = Goblin, 2 = Wraith, 4 = Dragon
            cmp #$03            ; Increment monster level for dragon
            bne ch_last         ; ,,
            lda #$04            ; ,,
ch_last:    cmp LAST_ENC        ; If the same monster was encountered last,
            bne new_patt        ;   then just return and use the same pattern
            rts                 ;   again
new_patt:   sta LAST_ENC        ; Set the last encounter
            clc                 ; Length = Monster strength + level - 1
            adc LEVEL           ; ,,
            sbc #$00            ; ,, (Carry is clear, so subtracting 1)
            sta PATT_LEN        ; ,,
            tay                 ; Y is the index, add notes to pattern backwards
            lda #>SEED
            sta P_RAND+1
-loop:      jsr RandDir
            clc                 ; Add one to the random number so that it's a
            adc #1              ;   valid direction (1=N 2=E 3=S 4=W)
            sta PATTERN,y       ; Store the pattern in reverse order
            dey
            bne loop            
            rts
            
; Play a Pattern            
PattPlay:   ldy PATT_LEN
-loop:      lda PATTERN,y
            tax
            lda Note,x
            sta VOICEM
            lda Dance,x
            ldx #0
            sta (PTR,x)
            lda #21             ; Starting point is 1/3 second delay
            sec                 ; But it gets a little faster with each 
            sbc LEVEL           ;   level
            bne patt_delay      ; This here code here is just a failsafe,
            lda #10             ;   to prevent a super-long delay
patt_delay: jsr Delay
            ldx #0              ; Return to Ready position after each move,
            lda #CHR_READY      ;   so the game is easier to play without
            sta (PTR,x)         ;   sound
            lda #0
            sta VOICEM
            lda #6
            jsr Delay
            dey
            bne loop
            ; Fall through to PattListen

PattListen: lda #CHR_READY      ; Draw the player in walking state
            ldx #0              ; ,,
            sta (PTR,x)         ; ,,
            lda #1              ; Set color to white during the listen
            jsr SetColor        ; ,,
-debounce:  jsr Joystick        ; Debounce the joystick before responding
            bne debounce        ; ,,
            ldy PATT_LEN
-loop:      jsr Joystick
            beq loop
            cmp PATTERN,y
            beq hit_move
            rts
hit_move:   tax
            lda Note,x
            sta VOICEM
            lda Dance,x
            ldx #0
            sta (PTR,x)
            lda #13             ; Set screen color to indicate successful
            sta SCRCOL          ;   move
-debounce:  jsr Joystick        ; Debounce joystick
            bne debounce        ; ,,
            ldx #0              ; Back to the Ready position after a move
            lda #CHR_READY      ; ,,
            sta (PTR,x)         ; ,,
            lda #0
            sta VOICEM
            lda #15
            sta SCRCOL
            dey
            bne loop
            rts            
    
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAZE GENERATOR ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initialize Maze
MakeMaze:   lda #$0b            ; Push 9,b as the X and Y coordinate as the
            pha                 ;   starting point for the maze
            lda #$09            ;   ,,
            pha                 ;   ,,
            lda #1              ; Set the number of levels on the stack to 1
            sta LEVELS          ; ,,
pop_cell:   pla                 ; Pop a cell's coordinates off the stack
            sta COOR_Y          ;   ,,
            pla                 ;   ,,
            sta COOR_X          ;   ,,
visit:      lda #CHR_OPEN       ; Visit the cell
            jsr PlotChar        ; ,,
CheckAdj:   lda #0              ; Check which directions are still available
            sta SCRPAD          ; ,,
av_west:    dec COOR_X          ; Check West
            dec COOR_X          ;   ,,
            jsr IsAvail         ;   ,,
            rol SCRPAD          ;   ,,
            inc COOR_X          ;   Reset
            inc COOR_X          ;   ,,
av_south:   inc COOR_Y          ; Check South
            inc COOR_Y          ;   ,,
            jsr IsAvail         ;   ,,
            rol SCRPAD          ;   ,,
            dec COOR_Y          ;   Reset
            dec COOR_Y          ;   ,,
av_east:    inc COOR_X          ; Check East
            inc COOR_X          ;   ,,
            jsr IsAvail         ;   ,,
            rol SCRPAD          ;   ,,
            dec COOR_X          ;   Reset
            dec COOR_X          ;   ,,
av_north:   dec COOR_Y          ; Check North
            dec COOR_Y          ;   ,,
            jsr IsAvail         ;   ,,
            rol SCRPAD          ;   ,,
            inc COOR_Y          ;   Reset
            inc COOR_Y          ;   ,,
            lda SCRPAD          ; A is a bitfield of unvisited directions           
            beq deadend            ; If no cells are unvisited, this is a dead end.
            sta SCRPAD          ; Choose a direction from the CheckAdj bitfield
next_rand:  ldx #0              ;   * Get a pseudorandom number between 0 and 3
            lda (P_RAND,x)      ;     ,,
            and #$03            ;     ,,
            tay                 ;     ,,
            inc P_RAND          ;   * Increment the pseudorandom table counter
            bne get_sp          ;     ,,
            inc P_RAND+1        ;     ,,
get_sp:     lda BitNumber,y     ;   * See if the selected bit is one of the
            bit SCRPAD          ;     options for the new direction
            beq next_rand       ;     If it isn't, get a new p-random number
open_wall:  pha                 ; Move to cell based on coordinates and
            jsr MoveCoor        ;   knock out wall
            lda #CHR_OPEN       ;   ,,
            jsr PlotChar        ;   ,,
            pla                 ; Now move from the wall to the actual cell
            jsr MoveCoor        ; ,,
            lda COOR_X          ; Once the coordinates have been moved, push
            pha                 ;   the updated cell onto the stack, increment
            lda COOR_Y          ;   the level counter, and go back to visit
            pha                 ;   the new cell
            inc LEVELS          ;   ,,
            jmp visit           ;   ,,
deadend:    dec LEVELS          ;   Decrement the level counter, get next cell.
            bne pop_cell        ;   Maze is finished when LEVELS is 0
            lda #$cb            ; This is bit esoteric; if the P_RAND counter is
            cmp P_RAND          ;   at exactly $cb, it means the maze probably
            beq rocks           ;   was generated at Welcome, so skip knock-outs
            
            ; Knock out some walls
            ldy #8              ; Knock out some walls to make the maze a little
-loop:      lda #CHR_OPEN       ;   less stuffy
            clc                 ;   (Clear carry will allow open spaces to be
            jsr Position        ;   placed on walls)
            dey                 ;   ,,
            bne loop            ;   ,,
            
            ; Add the dungeon graphics
rocks:      lda #$00            ; Add the dunegon graphics to the maze.
            sta COOR_X          ;   Start at the top left corner of the maze
            sta COOR_Y          ;   check for passageways to neighboring cells
-loop:      jsr Coor2Ptr        ; Get the character at the pointer and check if
            ldx #$00            ;   it's a piece of the wall.
            lda (PTR,x)         ;   ,,    
            cmp #CHR_WALL       ;   ,,
            bne next_cell       ; If it's not a wall, proceed to next character
            stx SCRPAD          ; SCRPAD is the passageway bitfield (0000wsen)
            lda #WEST           ; Check west
            jsr CheckPass       ; ,,
            rol SCRPAD          ; ,,
            lda #SOUTH          ; Check South
            jsr CheckPass       ; ,,
            rol SCRPAD          ; ,,
            lda #EAST           ; Check East
            jsr CheckPass       ; ,,
            rol SCRPAD          ; ,,
            lda #NORTH          ; Check North
            jsr CheckPass       ; ,,
            rol SCRPAD          ; SCRPAD now contains the bitfield of directions
            bne offset_dun      ; If there are any walls open, offset the char
            lda #CHR_FILL       ; Use the fill character if entirely surrounded
            bne convert         ; ,,
offset_dun: lda #$20            ; Convert the bitfield to the character code
            ora SCRPAD          ;   between $20 and $2f
convert:    jsr PlotChar        ; Store the plotted character
next_cell:  inc COOR_Y          ; Continue iterating across all characters in   
            lda COOR_Y          ;   the maze
            cmp #HEIGHT         ;   ,,
            bne loop            ;   ,,
            lda #$00            ;   ,,
            sta COOR_Y          ;   ,,
            inc COOR_X          ;   ,,
            lda COOR_X          ;   ,,
            cmp #WIDTH          ;   ,,
            bne loop            ;   ,,
            rts
            
; Populate Items
; Based on an item and count table, populate goblins, wraiths, armor, food, and
; "healing" potions.                  
Populate:   ldy #$04            ; Five items in the table
next_item:  lda ItemCount,y     ; Get the count for the item at the index
            sta SCRPAD          ; ,,
-loop:      lda ItemChar,y      ; Get the screen code for the item at the index
            sec                 ; Place it on the screen. SEC means to retry if
            jsr Position        ;   the item is placed over another item
            dec SCRPAD          ; Decrement the count and loop if there's more
            bne loop            ;   to do
            dey                 ; Decrement item index and get the next item if
            bpl next_item       ;   there's more to do
            rts
  
; Check Passageways                      
CheckPass:  pha
            jsr MoveCoor
            jsr Coor2Ptr
            pla
            cmp #SOUTH
            bcs sh_r
            asl
            asl
            jmp ch_reset
sh_r:       lsr
            lsr
ch_reset:   ldy COOR_X
            bmi out_range
            cpy #WIDTH
            bcs out_range
            ldy COOR_Y
            bmi out_range
            cpy #HEIGHT
            bcs out_range            
            jsr MoveCoor 
ch_open:    ldx #$00
            lda (PTR,x)
            cmp #CHR_OPEN
            bne is_wall
is_open:    sec
            rts
out_range:  jsr MoveCoor
is_wall:    clc
            rts  
            
; Move Coordinate
; Based on compass point in A
MoveCoor:   cmp #NORTH          ; Check each compass point and, if selected,
            bne ch_east         ;   move the appropriate coordinate
            dec COOR_Y          ;   ,,
ch_east:    cmp #EAST           ;   ,,
            bne ch_south        ;   ,,
            inc COOR_X          ;   ,,
ch_south:   cmp #SOUTH          ;   ,,
            bne ch_west         ;   ,,
            inc COOR_Y          ;   ,,
ch_west:    cmp #WEST           ;   ,,
            bne move_r          ;   ,,
            dec COOR_X          ;   ,,
move_r:     rts            
             
; Write character in A to a random cell.
; If carry is set, Position will try again if the cell is occupied
; A will be set to the index of the successful position
Position:   pha
            sty TMP_IX
            php
            lda #$d6
            sta PTR
            lda #$1e
            sta PTR+1
-loop:      jsr BASRND
            lda RNDNUM
            tay
            plp
            php
            bcc pos_ok
            lda (PTR),y
            cmp #CHR_OPEN
            bne loop
pos_ok:     plp
            pla
            sta (PTR),y
            tya                 ; Put position in A for access outside subroutine
            ldy TMP_IX          ; Restore Y
            rts
                        
; Plot Character
; To coordinates
; Write character in A           
PlotChar:   pha                 ; Save character for later
            jsr Coor2Ptr        ; Put coordinate in pointer
            pla                 ; Get back the PETSCII character
            ldx #0              ; Put the character on the screen
            sta (PTR,x)         ; ,,
            rts

; Set Color
; To color in A, at color memory corresponding to PTR
SetColor:   pha                 ; Set color at pointer to value in A
            lda PTR             ; ,,
            sta COL_PTR         ; ,,
            lda PTR+1           ; ,,
            clc                 ; ,,
            adc #$78            ; ,, $7800 is the difference between color
            sta COL_PTR+1       ; ,, and screen memory on unexpanded VIC-20
            pla                 ; ,,
            ldx #0              ; ,,
            sta (COL_PTR,x)     ; ,,
            rts
    
; Coordinates to Pointer
; Set PTR and PTR+1 with screen memory address referenced by the
; x and y coordinates, plus x and y offset values.
Coor2Ptr:   lda #<SCREEN        ; Start at the upper left corner of the screen
            sta PTR             ; ,,
            lda #>SCREEN        ; ,,
            sta PTR+1           ; ,,
            lda COOR_Y          ; Get y coordinate
            clc                 ; Add the y offset
            adc #Y_OFFSET       ; ,,
            beq no_y            ; If there's no offset, skip multiplication
            tay                 ; Y is the row index
-loop:      lda #22             ; Drop to the next line...
            clc                 ; ,,
            adc PTR             ; ,,
            sta PTR             ; ,,
            bcc next_y          ; ,,
            inc PTR+1           ; ,,
next_y:     dey                 ; ...Y times
            bne loop            ; ,,
no_y:       lda COOR_X          ; Get x coordinate
            clc                 ; ,,
            adc PTR             ; Add it to the pointer
            sta PTR             ; ,,
            bcc t2p_r           ; ,,
            inc PTR+1           ; ,,
t2p_r       rts

; Is Cell Available (a.k.a. Unvisited)
; Set carry if cell is available
IsAvail:    lda COOR_X          ; Range-checking
            bmi unavail         ; ,,
            cmp #WIDTH          ; ,,
            bcs unavail         ; ,,
            lda COOR_Y          ; ,,
            bmi unavail         ; ,,
            cmp #HEIGHT         ; ,,
            bcs unavail         ; ,,
            jsr Coor2Ptr        ; Convert coordinates to pointer
            ldx #0              ; Get character at pointer
            lda (PTR,x)         ; ,,
            cmp #CHR_WALL       ; If it's a wall, it's available
            beq avail           ; ,,
unavail:    clc                 ; Clear carry means unavailable
            rts                 ; ,,
avail:      sec                 ; Set carry means available
            rts                 ; ,,


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wait for Fire
Wait4Fire:  jsr Joystick
            cmp #JS_FIRE        ; ,,
            beq Wait4Fire       ; ,,
wait_fire:  jsr Joystick        ; Wait for the fire button
            cmp #JS_FIRE        ; ,,
            bne wait_fire       ; ,,
            rts
            
; Read the Joystick
; Return the direction in A
; 1=North, 2=East, 3=South, 4=West, 5=Fire, 0=None
Joystick:   lda VIA1PA          ; Read VIA1 port
            and #$3c            ; Keep track of bits 2,3,4,5
            sta JOYREG
            lda VIA2PB          ; Combine with read of bit 7
            and #$80            ;   from VIA2-B
            ora JOYREG
            eor #$bc            ; Flip each joystick bit in the
                                ;   combined read byte, so that
                                ;   on = 1
            sta JOYREG          ; Store temporary direction
            ldx #5              ; Check five directions (5=fire)
-loop:      lda JoyTable,x      ; Is the joystick pointed in the
            bit JOYREG          ;   direction indexed by X?
            bne found_dir       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
found_dir:  txa
            rts

; Get Random Direction
; In A (between 0 and 3)
RandDir:    ldx #0              ; Get the next pseudorandom number
            lda (P_RAND,x)      ; ,,
            inc P_RAND          ; Increment the pseudorandom counter
            bne randdir_r       ; ,,
            inc P_RAND+1        ; ,,
randdir_r:  and #$03            ; Constrain the value to a direction
            rts

Delay2:     lda #2
            ; Fall through to Delay

; Delay A Jiffies
Delay:      clc
            adc TIME_L
-loop:      cmp TIME_L
            bne loop
            rts    

       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up hardware
SetupHW:    lda #15             ; Set background color
            sta SCRCOL          ; ,,
            lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #<Welcome       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>Welcome       ; ,,
            sta NMINV+1         ; ,,
            sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
            rts  

; Initialize Game
InitGame:   lda #15             ; Screen color changes after game, so reset it
            sta SCRCOL          ; ,,
            ldy #0              ; Reset level to 0 (incremented at first level)
            sty LEVEL           ; ,,
            sty XP              ; Starting XP
            sty XP+1            ; ,,
            sty POTIONS         ; ,,
            iny                 ; ,,
            sty ARMOR           ; Starting armor
            ldy #ST_ENERGY      ; Starting energy
            sty ENERGY          ; ,,
            ldy #HP_ARMOR       ; Set starting HP
            sty HP              ; ,,
            ldy #$0a            ; Set volume
            sty VOLUME          ; ,,
            rts

; Initialize Level
InitLevel:  inc LEVEL           ; Increment player level counter
            jsr CLSR            ; Clear the screen
            sec                 ; Set changed flag for ShowScore
            ror CHANGED         ; ,,
            jsr ShowScore       ; Show the score while the maze is drawn
            lda TIME_M          ; Get a pseudorandom maze table
            sta P_RAND          ; ,,
            lda TIME_L          ; ,,
            ora #$c1            ; ,, In high memory, at least $c100
            sta P_RAND+1        ; ,,
            lda #0              ; Obfuscate the play area
            ;lda #6             ; Uncomment for maze diagnostics
            jsr WipeColor       ; ,,
            jsr MakeMaze        ; Make the maze
            jsr Populate        ; Populate it
            lda #0              ; Reset the key status flag
            sta KEYSTAT         ; ,,
            sta LAST_ENC        ; Reset last encounter
            lda #1              ; Set starting position of player
            sta COOR_X          ; ,,
            sta COOR_Y          ; ,,
            lda #CHR_PL_R       ; Set graphic to right-facing player
            sta PLAYER_CH       ; ,,
            lda #CHR_OPEN       ; Set the character under the player
            sta UNDER           ;   to an open space
            jsr DrawPlayer      ; Draw player at starting position
            rts            

; Wipe Color
; Set character color to value specified in A
; Just for the maze area
WipeColor:  ldy #0
-loop:      sta $9684,y
            sta $9700,y
            dey
            bne loop
            rts

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SOUND ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Play Direction Tone
; With A as the direction (0-3)
DirTone:    pha
            tax
            lda Note,x
            sta VOICEM
            lda #$06
            jsr Delay
            lda #0
            sta VOICEM
            pla
            rts
            
; Play Next Sound Effect
; Rotates the 8-bit sound effect register and
; plays the pitch      
FXService:  lda FX_LEN          ; Has the sound been launched?
            beq fx_end          ; If unlaunched, kill voice and return
            lda #$0a            ; Make sure sound effects are not affected by
            sta VOLUME          ;   music player's volume changes
            dec FX_LEN          ; Decrement both length
            dec FX_COUNT        ;   and countdown
            bne fx_r            ; If countdown has elapsed,
            lda FX_SPEED        ;   reset it with the current effect speed
            sta FX_COUNT        ;   ,,
            bit FX_DIR          ; Rotate the register, based on the direction
            bmi fx_right        ;   specified by the direction flag
fx_left:    lda #$00
            asl FX_REG          ; Rotate the register left if flag = $00
            adc FX_REG          ; If carry was set, set bit 0
            jmp fx_update       ; Update and play the new frequency
fx_right:   lsr FX_REG          ; Rotate the register right if flag = $80
            lda FX_REG          ; ,,
            bcc fx_play         ; If carry was set, set bit 7
            lda #%10000000      ; ,,
            ora FX_REG          ; ,,
fx_update:  sta FX_REG          ; ,,
fx_play:    ora #$80            ; Gate the high voice
fx_end:     sta FX_VOICE        ; ,,
fx_r:       rts 

; Wait for End
; Do nothing until the current effect ends
FXWait:     lda FX_LEN
            bne FXWait
            beq fx_end
        
; Launch Sound Effect
; Preparations
;     A - The sound effect index
FXLaunch:   sei                 ; Don't play anything while setting up
            asl                 ; Each effect has two bytes in the table
            tax
            lda FXTable,x       ; Get the register byte
            sta FX_DIR          ; Set the direction (only bit 7 will be used)
            and #$7f            ; Mask away the direction bit to get the 7-bit
            sta FX_REG          ;   frequency
            lda FXTable+1,x     ; Get the length byte
            tax                 ;   and preserve it
            and #$f0            ; Length is in bits 4-7 of the length byte
            sta FX_LEN          ;  ,,
            txa
            and #$0f            ; Speed (jiffies per rotation) is in the low
            sta FX_SPEED        ;   nybble of the length byte
            sta FX_COUNT        ;   ,,
            cli                 ; Go! 
            rts            


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Text tables
Name:       .asc $13,$05,$0d,"   DUNGEON OF DANCE",$00
Intro:      .asc $0d,$0d,$1e,"  2021 JASON JUSTIAN",$0d,$0d
            .asc "      PRESS FIRE",$0d,$00
Labels:     .asc $1e,$0d," LV@        XP@",$0d," HP@        EN@",$00

GameOver:   .asc $13,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
            .asc $1d,$1d,$1d,$1d,$1d," QUEST OVER ",$00

; Direction tables
JoyTable:   .byte 0,$04,$80,$08,$10,$20            ; Corresponding direction bit
Compass:    .byte 0,1,2,4,8

; Value of bit x using lda BitNumber,x (where x=0~3)            
BitNumber:  .byte %00000001, %00000010, %00000100, %00001000

; Configuration for items and monters per level
ItemChar:   .byte CHR_WRAITH,CHR_GOBLIN,CHR_FOOD,CHR_ARMOR,CHR_POTION
ItemCount:  .byte 3,8,6,1,3

; Search pattern for automap, starting from index 7 (PTR minus 23)
Autopatt:   .byte 1,1,20,2,20,1,1,0

; Item color table
ItemColors: .byte COL_POTION,COL_ARMOR,COL_FOOD,COL_GOBLIN,COL_WRAITH,COL_DRAGON

; Note Table
; Selections from chromatic scale (tonic, major 3rd, 5th, 6th)
; 194,197,201,204,207,209,212,214,217,219,221,223,225
Note:       .byte 0,219,207,194,214

; Dance Table
; Dance moves based on direction
Dance:      .byte 0,$1d,$0d,$1e,$0b

; Sound effects for the sound effects player
; Each effect has four parameters (DFFFFFFF LLLLSSSS)
;   (1) Bit 7 (D) of the first byte is the direction
;       * Unset=shift left, or a rising sound 
;       * Set=shift right, or a falling sound
;   (2) Bits 0-6 (F) of the first byte is the frequency register
;   (3) High nybble of the second byte (L) is the length in jiffies x 16
;       * Between approx. 1/4 sec and 4 sec in length
;   (4) Low nybble of second byte (S) is speed in jiffies
FXTable:    .byte $ef,$11       ; 0- Item Pick-up
            .byte $09,$13       ; 1- Danceoff won
            .byte $d0,$13       ; 2- Danceoff lost
            .byte $01,$25       ; 3- Eat food
            .byte $d0,$23       ; 4- Transported
            .byte $0f,$21       ; 5- Healing potion
            .byte $3c,$48       ; 6- Level victory tune
            .byte $55,$34       ; 7- HP to Energy alert
            .byte $08,$3a       ; 8- Has Key
            
; Pad to 3583
Padding:    .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            .byte 0,0,0,0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUSTOM CHARACTER SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The character set must start at $1C00. If you change anything
; anywhere, you must account for this. The easiest way is to use
; padding bytes immediately before this character data.
;
; The easiest way to tell if you've done this right is to
; make sure that the object code is exactly 3583 bytes. This is
; a reliable method as long as you don't add anything AFTER this
; character data.
;
CharSet:    ; Letters (* used,- reassignable)
            ; DUNGEON OF DANCE
            ; 2021 JASON JUSTIAN
            ; PRESS FIRE
            ; LV~ HP~ XP~
            ; QUEST OVER
            .byte $00,$00,$20,$10,$00,$20,$10,$00 ; Colon
            .byte $10,$28,$44,$54,$82,$fe,$82,$00 ; A *
            ;.byte $fc,$42,$62,$9c,$62,$42,$fc,$00 ; B -
            .byte $38,$38,$10,$7c,$92,$38,$44,$44 ; Ready to go!   ($02)
            .byte $3a,$46,$82,$90,$80,$42,$3c,$00 ; C *
            .byte $78,$44,$42,$a2,$42,$44,$78,$00 ; D *
            .byte $7c,$42,$40,$b8,$40,$42,$7c,$00 ; E *
            .byte $7c,$42,$40,$b8,$40,$40,$40,$00 ; F *
            .byte $38,$44,$80,$9c,$8a,$44,$3c,$00 ; G *
            .byte $82,$82,$92,$ee,$92,$82,$82,$00 ; H *
            .byte $10,$10,$28,$28,$28,$10,$10,$00 ; I *
            .byte $04,$0a,$0a,$04,$04,$04,$08,$30 ; J *
            ;.byte $a8,$88,$90,$e0,$90,$88,$84,$03 ; K -
            .byte $18,$38,$08,$78,$08,$14,$24,$14 ; Dance Left     ($0b)
            .byte $40,$40,$a0,$a0,$40,$42,$7c,$00 ; L *
            ;.byte $82,$d6,$ba,$92,$aa,$92,$82,$00 ; M *
            .byte $18,$1c,$10,$1e,$10,$28,$24,$28 ; Dance Right    ($0d)
            .byte $c2,$aa,$a2,$92,$8a,$aa,$86,$00 ; N *
            .byte $38,$44,$92,$ba,$92,$44,$38,$00 ; O *
            .byte $fc,$42,$a2,$a2,$7c,$40,$40,$00 ; P *
            .byte $38,$44,$92,$82,$92,$54,$38,$10 ; Q *
            .byte $fc,$42,$a2,$a2,$7c,$48,$44,$03 ; R *
            .byte $7c,$82,$90,$6c,$12,$82,$7c,$00 ; S *
            .byte $7c,$92,$10,$28,$28,$10,$10,$00 ; T *
            .byte $c6,$92,$82,$82,$82,$82,$7c,$00 ; U *
            .byte $c6,$92,$82,$82,$44,$28,$10,$00 ; V *
            .byte $a2,$aa,$a2,$92,$4c,$28,$10,$00 ; W -
            .byte $82,$82,$54,$28,$54,$82,$82,$00 ; X *
            ;.byte $c6,$82,$54,$28,$28,$10,$10,$00 ; Y -
            ;.byte $7e,$84,$18,$6c,$30,$42,$fc,$00 ; Z -
            .byte $0c,$1c,$04,$0f,$14,$06,$0b,$19 ; Player walking - left
            .byte $30,$38,$20,$f0,$28,$60,$d0,$98 ; Player walking - right
            
            ; Indicators
            .byte $01,$02,$05,$68,$d0,$88,$58,$30 ; Key Indicator  ($1b)
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff ; Dungeon Fill   ($1c)
            .byte $b2,$ba,$92,$7c,$10,$1c,$24,$44 ; Dance Up       ($1d)
            .byte $00,$38,$38,$10,$7c,$92,$38,$c6 ; Dance Down     ($1e)
            .byte $3c,$7e,$7e,$7e,$7a,$7a,$7e,$7e ; Door           ($1f)    
            .byte $00,$00,$00,$00,$00,$00,$00,$00 ; Space          ($20)
            
            ; Dungeon Walls
            .byte $00,$c3,$ef,$ff,$ff,$ff,$ff,$ff ; N    (0001) "
            .byte $fe,$fe,$fe,$f8,$fc,$fc,$fe,$fe ; E    (0010) #
            .byte $00,$24,$e6,$fc,$f8,$fe,$fe,$fc ; NE   (0011) $
            .byte $ff,$ff,$ff,$ff,$ff,$df,$1d,$00 ; S    (0100) %
            .byte $00,$83,$c7,$f7,$ff,$df,$9c,$00 ; NS   (0101) &
            .byte $fe,$fc,$fe,$fa,$f8,$fc,$dc,$00 ; SE   (0110) '
            .byte $00,$38,$f8,$fc,$fe,$fe,$dc,$00 ; NSE  (0111) (
            .byte $7f,$7f,$1f,$3f,$3f,$3f,$7f,$7f ; W    (1000) )
            .byte $00,$70,$79,$5d,$1f,$3f,$7f,$7f ; NW   (1001) *
            .byte $3c,$38,$38,$38,$7c,$1c,$3c,$7c ; EW   (1010) +
            .byte $00,$3c,$3e,$7e,$3c,$78,$7a,$3e ; NSW  (1011) ,
            .byte $7f,$3f,$1f,$1f,$7f,$77,$07,$00 ; EW   (1100) -
            .byte $00,$13,$3f,$7f,$7f,$7d,$31,$00 ; NEW  (1101) .
            .byte $7e,$7e,$3e,$3e,$7c,$7c,$38,$00 ; ESW  (1110) /
            .byte $00,$1c,$3e,$7e,$7e,$7c,$1c,$00 ; All  (1111) !
            
            ; Digits
            .byte $3a,$44,$8a,$92,$a2,$44,$b8,$00 ; 0
            .byte $38,$10,$10,$10,$10,$10,$38,$00 ; 1 
            .byte $3c,$42,$02,$04,$18,$22,$7c,$00 ; 2 
            .byte $3c,$42,$02,$1c,$02,$02,$04,$18 ; 3 
            .byte $80,$48,$68,$4a,$7c,$08,$08,$00 ; 4
            .byte $7c,$42,$40,$7c,$02,$02,$04,$18 ; 5
            .byte $1e,$20,$78,$84,$92,$82,$7c,$00 ; 6
            .byte $7e,$84,$08,$3c,$10,$20,$40,$00 ; 7
            .byte $7c,$82,$92,$44,$38,$44,$7c,$00 ; 8
            .byte $7c,$82,$92,$82,$7e,$02,$7c,$00 ; 9
            
            ; Items
            .byte $00,$1c,$08,$08,$3e,$3e,$3e,$1c ; Potion         ($3a)
            .byte $00,$22,$2a,$3e,$2a,$36,$1c,$00 ; Armor          ($3b)
            .byte $00,$04,$08,$3c,$7e,$7e,$3c,$42 ; Food           ($3c)
            
            ; Monsters
            .byte $01,$3d,$3d,$19,$3e,$58,$18,$24 ; Goblin         ($3d)
            .byte $00,$1c,$3e,$aa,$fe,$76,$1c,$00 ; Wraith         ($3e)
            .byte $2c,$66,$e7,$2f,$3e,$3c,$67,$88 ; Dragon         ($3f)
