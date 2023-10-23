       01 SELLER-DETAILS-SCREEN foreground-color 15 AUTO UPPER.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST SELLER DETAILS       |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 LINE 5 COL 2 "SELLER ID        ".
         03 PIC 9999 from SELLER-ID.
         03 LINE 7 COL 2 "SELLER NAME    [".
         03 pic X(20) using SELLER-NAME foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "ADDRESS-LINE 1 [".
         03 PIC X(25) USING SELLER-AL1 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 10 COL 2 "ADDRESS-LINE 2 [".
         03 PIC X(25) USING SELLER-AL2 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 11 COL 2 "ADDRESS-LINE 3 [".
         03 PIC X(25) USING SELLER-AL3 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 12 COL 2 "ADDRESS-LINE 4 [".
         03 PIC X(25) USING SELLER-AL4 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 14 COL 2 "POSTCODE       [".
         03 PIC X(7) USING SELLER-POSTCODE foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 16 COL 2 "CONTACT NUMBER [".
         03 PIC x(11) USING SELLER-PHONE foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 18 COL 2 "NOTES          [".
         03 PIC x(50) USING SELLER-NOTES foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 line 23 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 23 col 41 foreground-color 14 value
            "----------------------------------------".
         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 55 "ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".
