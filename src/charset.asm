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
; Color Map
;   * Text    - Cyan
;   * Maze    - White
;   * Player  - Green
;   * Key     - Yellow
;   * Door    - Cyan
;   * Potion  - Purple
;   * Armor   - White
;   * Food    - Green
;   * Goblin  - Cyan
;   * Wraith  - White
;   * Dragon  - Red

* = $1c00
CharSet:    ; Letters (* used,- reassignable)
            ; DUNGEON OF SONIM
            ; 2021 JASON JUSTIAN
            ; PRESS FIRE
            ; HP~ XP~
            ; QUEST OVER
            .byte $00,$00,$20,$10,$00,$20,$10,$00 ; Colon
            .byte $10,$28,$44,$54,$82,$fe,$82,$00 ; A *
            ;.byte $fc,$42,$62,$9c,$62,$42,$fc,$00 ; B -
            ;.byte $3a,$46,$82,$90,$80,$42,$3c,$00 ; C -
            .byte $18,$38,$08,$78,$08,$14,$24,$14 ; Dance Left     ($02)
            .byte $18,$1c,$10,$1e,$10,$28,$24,$28 ; Dance Right    ($03)
            .byte $78,$44,$42,$a2,$42,$44,$78,$00 ; D *
            .byte $7c,$42,$40,$b8,$40,$42,$7c,$00 ; E *
            .byte $7c,$42,$40,$b8,$40,$40,$40,$00 ; F *
            .byte $38,$44,$80,$9c,$8a,$44,$3c,$00 ; G *
            .byte $82,$82,$92,$ee,$92,$82,$82,$00 ; H *
            .byte $10,$10,$28,$28,$28,$10,$10,$00 ; I *
            .byte $04,$0a,$0a,$04,$04,$04,$08,$30 ; J *
            .byte $a8,$88,$90,$e0,$90,$88,$84,$03 ; K -
            .byte $40,$40,$a0,$a0,$40,$42,$7c,$00 ; L -
            .byte $82,$d6,$ba,$92,$aa,$92,$82,$00 ; M *
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
            .byte $3c,$66,$66,$3c,$10,$1c,$10,$18 ; Key Indicator  ($1b)
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
            .byte $00,$00,$1c,$1c,$08,$3e,$3e,$1c ; Potion         ($3a)
            .byte $41,$63,$7f,$77,$63,$36,$1c,$08 ; Armor          ($3b)
            .byte $00,$12,$24,$12,$00,$3e,$3e,$1c ; Food           ($3c)
            
            ; Monsters
            .byte $01,$3d,$3d,$19,$3e,$58,$18,$24 ; Goblin         ($3d)
            .byte $1c,$be,$aa,$be,$ff,$14,$2a,$55 ; Wraith         ($3e)
            .byte $2c,$66,$e7,$2f,$3e,$3c,$67,$88 ; Dragon         ($3f)
            