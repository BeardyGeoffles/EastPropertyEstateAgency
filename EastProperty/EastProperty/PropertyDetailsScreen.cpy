       01 PROPERTY-DETAILS-SCREEN foreground-color 15 AUTO UPPER.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 2 COL 29 foreground-color 14 VALUE
            "|  EAST PROPERTY DETAILS     |".
         03 line 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 5 COL 2 "PROPERTY ID     ".
         03 PIC 9(4) FROM PROPERTY-ID.
         03 LINE 7 COL 2 "ADDRESS-LINE 1 [".
         03 PIC X(25) USING PROPERTY-AL1 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 8 COL 2 "ADDRESS-LINE 2 [".
         03 PIC X(25) USING PROPERTY-AL2 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "ADDRESS-LINE 3 [".
         03 PIC X(25) USING PROPERTY-AL3 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 10 COL 2 "ADDRESS-LINE 4 [".
         03 PIC X(25) USING PROPERTY-AL4 foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 11 COL 2 "POSTCODE       [".
         03 PIC X(7) USING PROPERTY-POSTCODE foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 12 COL 2 "ASKING PRICE   [".
         03 FOREGROUND-COLOR 10 pic x(1) from ws-pound.
         03 PIC ZZZ,ZZZ.ZZ USING PROPERTY-ASKING foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 14 COL 2 "SELLER ID      [".
         03 pic z(4) using PROP-SELLER-ID foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".

         03 LINE 14 COL 24 foreground-color 14 pic x(40) from
            ws-seller-valid-message.

         03 LINE 16 COL 2 "PROPERTY TYPE  [".
         03 pic X(20) using PROPERTY-TYPE foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 18 COL 2 "NO OF BEDROOMS [".
         03 pic Z(2) using PROPERTY-BEDS foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 18 COL 25 "CENTRAL HEATING (Y/N) [".
         03 PIC X USING CENTRAL-HEATING foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 19 COL 2 "GARDEN (Y/N)   [".
         03 PIC X USING PROPERTY-GARDEN foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 19 COL 25 "PARKING (Y/N)         [".
         03 PIC X USING OFF-RD-PARKING foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".

        03 LINE 21 COL 2
        "STATUS (L)isted (S)old (O)ffer (W)ithdrawn   [".
         03 PIC X USING OFFER-STATUS foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".
         03 LINE 21 COL 10 FOREGROUND-COLOR 14 "L".
         03 LINE 21 COL 19 FOREGROUND-COLOR 14 "S".
         03 LINE 21 COL 26 FOREGROUND-COLOR 14 "O".
         03 LINE 21 COL 34 FOREGROUND-COLOR 14 "W".

         03 LINE 22 COL 2 "OFFER AMOUNT   [".
         03 FOREGROUND-COLOR 10 pic x(1) from ws-pound.
         03 pic ZZZ,ZZZ.ZZ using OFFER-AMOUNT foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 22 COL 32 "BUYER ID  [".
         03 pic z(4) using PROP-BUYER-ID foreground-color 10 HIGHLIGHT
            PROMPT " ".
         03 VALUE "]".

         03 LINE 22 COL 49 foreground-color 14 pic x(40) from
            ws-buyer-valid-message.

         03 line 23 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 23 col 41 foreground-color 14 value
            "----------------------------------------".
         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 55 "ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".
