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
WIDTH       = 22                ; surrounding it. So a 16x16 maze has 8x8 cells
Y_OFFSET    = 6                 ; Y offset of maze
SEED        = $c100             ; Start of pseudo-random data table
HP_ARMOR    = 4                 ; Max HP per armor
ST_ENERGY   = 200               ; Starting energy (loss of 1 per move)
HP_ENERGY   = 50                ; Energy gained per HP consumed
FOOD_ENERGY = 50                ; Energy gained per food found
POTION_HP   = 8                 ; HP gained per potion used
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

; Colors
COL_DOOR    = 3                 ; Door is cyan
COL_WALL    = 7                 ; Wall is yellow
COL_POTION  = 4                 ; Potion is purple
COL_ARMOR   = 1                 ; Armor is white
COL_FOOD    = 5                 ; Food is green
COL_GOBLIN  = 4                 ; Goblin is purple
COL_WRAITH  = 1                 ; Wraith is white
COL_DRAGON  = 2                 ; Dragon is red

; Constants
NORTH       = 1
EAST        = 2
SOUTH       = 4
WEST        = 8
JS_NORTH    = 1
JS_EAST     = 2
JS_SOUTH    = 3
JS_WEST     = 4
JS_FIRE     = 5

; System Resources
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
-NMINV     = $fffe              ; Development NMI non-vector (uncomment for dev)
SCREEN      = $1e00             ; Screen character memory (unexpanded)
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eb12             ; System ISR return point
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
HAS_KEY     = $06               ; Flag for key (bit 7=has key, bit 6=found door)
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
            lsr HAS_KEY         ; ,,
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
Main:       bit HAS_KEY         ; Check for level completion with bit 6 of
            bvs LevelUp         ;   HAS_KEY
            lda HP              ; Check for player death
            beq QuestOver       ; ,,
read_js:    jsr Joystick
            beq read_js
            cmp #JS_FIRE
            beq UsePotion
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
            tax                 ; Convert joystick code to compass point
            lda Compass,x       ; ,,
            jsr MoveCoor        ; Move coordinate based on compass point
            jsr DecEnergy       ; Use one energy for the movement
            jsr Encounter       ; Process encounter
            jsr DrawPlayer      ; Draw the player
debounce:   jsr Joystick        ; Debounce the joystick
            bne debounce        ; ,,
            beq Main
    
; Use Potion            
UsePotion:  lda POTIONS
            bne potion_dec
            jmp Main
potion_dec: dec POTIONS
            lda #POTION_HP
            jsr IncHP
            jsr ShowScore
            jmp debounce
            
; Quest Over        
QuestOver:  lda #<GameOver      ; Show Game Over notification
            ldy #>GameOver      ; ,,
            jsr PRTSTR          ; ,,
            jmp Start           ; Back to wait for a new game
            
; Level Clear
LevelUp:    jmp NextLevel       
            
; Interrupt Service Routine            
ISR:        inc TIME_L
            bne isr_r
            inc TIME_M
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
            bit HAS_KEY         ; If the dragon has been defeated, do not
            bmi dragdoor_r      ;   display
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
            bit HAS_KEY         ; Show the key if the dragon has been defeated
            bpl lev_num         ; ,,
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
            ldx XP              ; ,,
            lda XP+1            ; ,,
            jsr PRTFIX          ; ,,
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
            lda #$9c            ; Purple
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
            jmp IncEnergy
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
            sta SCRPAD
            lda HP
            sec
            sbc SCRPAD
            sta HP
            bpl dechp_r
            lda #0
            sta HP
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
            bne enc_wall        ; ,,
            jmp Door
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
            lda #FOOD_ENERGY    ; ,,
            jsr IncEnergy       ; ,,
            jmp RemoveItem      ; If food was picked up, remove it
enc_monst:  cmp #CHR_GOBLIN     ; Handle
            bcs DanceOff
enc_r:      rts
                        
; Process Door Encounter
; If the dragon has been defeated on this level, HAS_KEY bit 7 is set. If the
; door is reached, HAS_KEY bit 6 is set, indicating the level is cleared. This
; will be picked up as a "Level Up" signal by Main.
Door:       bit HAS_KEY
            bpl locked
            lsr HAS_KEY
locked:     rts  

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
            jsr PattPlay
            jsr PattListen
            bcc lose
            lda LAST_ENC
            cmp #$04            ; Is this the dragon?
            bne win             ; ,,
            sec                 ; If so, set the HAS_KEY flag when the dragon
            ror HAS_KEY         ;   has been defeated
win:        jsr LevelMult       ; Increase XP based on level
            jsr IncXP           ; ,,
            jsr RemoveItem      ; Remove the vanquished foe from the dungeon
            lda #0              ; Reset last encounter so there's a new pattern
            sta LAST_ENC        ;   for the next monster of this kind
            rts
lose:       lda LAST_ENC        ; Multiply monster strength times level number
            jsr LevelMult       ; ,,
            and TIME_L          ; AND with the jiffy clock to possibly remove
            ora LAST_ENC        ;   bits, then ORA with the monster strength for
            jsr DecHP           ;   a minimum HP loss
            lda #CHR_OPEN       ; And the player will be put back where he or
            sta UNDER           ;   she was
            jmp Revert          ; You shall not pass!  
            
PattMake:   sec                 ; Set encounter to monster level, where
            sbc #CHR_GOBLIN-1   ; 1 = Goblin, 2 = Wraith, 4 = Dragon
            cmp #$03            ; Increment monster level for dragon
            bne ch_last         ; ,,
            lda #$04            ; ,,
ch_last:    cmp LAST_ENC        ; If the same monster was encountered last,
            bne new_patt        ;   then just return and use the same pattern
            rts                 ;   again
new_patt:   sta LAST_ENC        ; Set the last encounter
            clc                 ; Add the level number
            adc LEVEL           ;   ,,
            sta PATT_LEN        ; Monster strength + level is the pattern length
            tay                 ; Y is the index, add notes to pattern backwards
            lda #>SEED
            sta P_RAND+1
-loop:      jsr RandDir
            sta PATTERN,y
            dey
            bne loop            
            rts
            
; Play a Pattern            
PattPlay:   ldy PATT_LEN
-loop:      lda PATTERN,y
            tax
            lda Note,x
            sta VOICEM
            lda #20
            jsr Delay
            lda #0
            sta VOICEM
            lda #6
            jsr Delay
            dey
            bne loop
            rts

PattListen: lda $9003
            rol
            rts
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAZE ROUTINES
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
Populate:   ldy #$04
next_item:  lda ItemCount,y
            sta SCRPAD
-loop:      lda ItemChar,y
            sec
            jsr Position
            dec SCRPAD
            bne loop
            dey
            bpl next_item
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
            ldy TMP_IX
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
            rts
            sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
            rts  

; Initialize Game
InitGame:   ldy #0              ; Reset level to 0 (incremented at first level)
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
            ldy #$8a            ; Set volume
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
            lda #6             ; Uncomment for maze diagnostics
            jsr WipeColor       ; ,,
            jsr MakeMaze        ; Make the maze
            jsr Populate        ; Populate it
            lda #0              ; Reset the key status flag
            sta HAS_KEY         ; ,,
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
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Text tables
Name:       .asc $13,$05,$0d,"   DUNGEON OF DANCE",$00
Intro:      .asc $0d,$0d,$1e,"  2021 JASON JUSTIAN",$0d,$0d
            .asc "      PRESS FIRE",$0d,$00
Labels:     .asc $1e,$0d," LV@        XP@",$0d," HP@        EN@",$00

GameOver:   .asc $13,$05,$0d,"      QUEST OVER   ",$00

; Direction tables
JoyTable:   .byte 0,$04,$80,$08,$10,$20            ; Corresponding direction bit
Compass:    .byte 0,1,2,4,8

; Value of bit x using lda BitNumber,x (where x=0~3)            
BitNumber:  .byte %00000001, %00000010, %00000100, %00001000

; Configuration for items and monters per level
ItemChar:   .byte CHR_WRAITH,CHR_GOBLIN,CHR_FOOD,CHR_ARMOR,CHR_POTION
ItemCount:  .byte 3,6,5,1,3

; Search pattern for automap, starting from index 7 (PTR minus 23)
Autopatt:   .byte 1,1,20,2,20,1,1,0

; Item color table
ItemColors: .byte COL_POTION,COL_ARMOR,COL_FOOD,COL_GOBLIN,COL_WRAITH,COL_DRAGON

; Note Table
; Selections from chromatic scale (tonic, major 3rd, 5th, 6th)
; 194,197,201,204,207,209,212,214,217,219,221,223,225
Note:       .byte 194,207,214,219