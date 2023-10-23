       01 USER-DETAILS-SCREEN foreground-color 15 AUTO UPPER.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST USER DETAILS         |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 LINE 5 COL 2 "USER ID           ".
         03 PIC 9999 from USER-ID.
         03 LINE 7 COL 2 "USER NAME        [".
         03 pic X(10) using USER-NAME foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "USER PASSWORD    [".
         03 PIC X(10) USING ws-password-1 foreground-color 10 no-echo.
         03 VALUE "]".

         03 LINE 11 COL 2 "CONFIRM PASSWORD [".
         03 PIC X(10) USING ws-password-2 foreground-color 10 no-echo.
         03 VALUE "]".

         03 LINE 13 COL 2 "ADMIN ACCESS Y/N [".
         03 PIC X using user-has-admin foreground-colour 10 highlight
            prompt " ".
         03 value "]".

         03 line 21 col 2 from ws-message foreground-color ws-color.

         03 line 23 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 23 col 41 foreground-color 14 value
            "----------------------------------------".
         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 55 "ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".
