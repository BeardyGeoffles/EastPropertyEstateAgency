
       
       01 VIEWING-DETAILS-SCREEN upper AUTO foreground-color 15.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 2 COL 29 foreground-color 14 VALUE
            "|  EAST VIEWING DETAILS      |".
         03 line 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 5 COL 2 "VIEWING ID    ".
         03 PIC 9(4) FROM VIEWING-ID.
         03 LINE 7 COL 2 "PROPERTY ID  [".
         03 PIC z(4) USING VIEWING-PROPERTY-ID foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 7 COL 24 foreground-color 14 pic x(40) from
            ws-PROPERTY-valid-message.

         03 LINE 9 COL 2 "BUYER ID     [".
         03 PIC z(4) USING VIEWING-BUYER-ID foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 9 COL 24 foreground-color 14 pic x(40) from
            ws-BUYER-valid-message.

         03 LINE 11 COL 2 "DATE         [".
         03 PIC 99 USING view-day foreground-color 10 HIGHLIGHT PROMPT
            " " blank when zero.
         03 value "/".
         03 PIC 99 USING view-month foreground-color 10 HIGHLIGHT PROMPT
            " " blank when zero.
         03 value "/".
         03 PIC 9999 USING view-year foreground-color 10 HIGHLIGHT
            PROMPT " " blank when zero.
         03 VALUE "]".
         03 LINE 13 COL 2 "TIME         [".
         03 PIC 99 USING view-hour foreground-color 10 HIGHLIGHT PROMPT
            " " blank when zero.
         03 value ":".
         03 PIC 99 USING view-mins foreground-color 10 HIGHLIGHT PROMPT
            " ".
         03 VALUE "]".
         03 LINE 15 COL 2 "USER ID      [".
         03 PIC z(4) USING VIEWING-USER-ID foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".

         03 LINE 15 COL 24 foreground-color 14 pic x(40) from
            ws-USER-valid-message.

         03 line 23 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 23 col 41 foreground-color 14 value
            "----------------------------------------".
         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 55 "ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".
