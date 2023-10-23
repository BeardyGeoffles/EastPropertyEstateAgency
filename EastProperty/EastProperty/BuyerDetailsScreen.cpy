       01 BUYER-DETAILS-SCREEN foreground-color 15 AUTO UPPER.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST BUYER DETAILS        |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 LINE 5 COL 2 "BUYER ID        ".
         03 PIC 9999 from BUYER-ID.
         03 LINE 7 COL 2 "BUYER NAME     [".
         03 pic X(20) using BUYER-NAME foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "ADDRESS-LINE 1 [".
         03 PIC X(25) USING BUYER-AL1 foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 10 COL 2 "ADDRESS-LINE 2 [".
         03 PIC X(25) USING BUYER-AL2 foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 11 COL 2 "ADDRESS-LINE 3 [".
         03 PIC X(25) USING BUYER-AL3 foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 12 COL 2 "ADDRESS-LINE 4 [".
         03 PIC X(25) USING BUYER-AL4 foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 14 COL 2 "POSTCODE       [".
         03 PIC X(7) USING BUYER-POSTCODE foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 16 COL 2 "CONTACT NUMBER [".
         03 PIC x(11) USING BUYER-PHONE foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 18 COL 2 "NOTES          [".
         03 PIC x(50) USING BUYER-NOTES foreground-color 10
         HIGHLIGHT PROMPT " ".
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
