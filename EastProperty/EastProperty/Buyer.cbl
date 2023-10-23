      *BUYER.cbl
      *Handle menu screens and functionality to add/search a Buyer
      *
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. Buyer is recursive.
       environment division.
       input-output section.
       file-control.

       copy "buyerfile.cpy".
       copy "propertyfile.cpy".
       copy "userfile.cpy".
       copy "sellerfile.cpy".
       copy "viewingfile.cpy".


       configuration section.
       special-names.
           crt status is key-status.

       data division.

       FILE SECTION.
       copy "buyerrecord.cpy".
       copy "propertyrecord.cpy".
       copy "userrecord.cpy".
       copy "sellerrecord.cpy".
       copy "viewingrecord.cpy".

       working-storage section.

       copy "ws-functionkeys.cpy".
       copy "ws-common.cpy".

       01 ws-buyer-search-fields.
         03 ws-buyer-search-id pic 9(4) value 0.
         03 ws-buyer-search-name pic x(20) value spaces.
         03 ws-buyer-search-address pic x(25) value spaces.
         03 ws-buyer-search-postcode pic x(7) value spaces.

       01 ws-message pic x(30) value spaces.
       01 ws-valid-buyer-found pic 9 value 0 comp.
       01 ws-end-of-file pic 9 value 0 comp.
       01 ws-color pic 99 value 15.


       01 ws-total-matches pic 9999 value 0 comp.
       01 ws-current-match pic 9999 value 0 comp.
       01 ws-total-matches-hide pic z(4).
       01 ws-current-match-hide pic z(4).

      *Array to hold all search results
       01 match-array occurs 9999 times.
         03 match-buyer-id pic 9999.
         03 match-buyer-name pic x(20).
         03 match-buyer-address1 pic x(25).
         03 match-buyer-address2 pic x(25).
         03 match-buyer-address3 pic x(25).
         03 match-buyer-address4 pic x(25).
         03 match-buyer-postcode pic x(7).

      *Display fields for the currently selected record
       01 ws-buyer-found-fields.
         03 ws-buyer-found-id pic 9(4) value 0.
         03 ws-buyer-found-name pic x(20) value spaces.
         03 ws-buyer-found-address1 pic x(25) value spaces.
         03 ws-buyer-found-address2 pic x(25) value spaces.
         03 ws-buyer-found-address3 pic x(25) value spaces.
         03 ws-buyer-found-address4 pic x(25) value spaces.
         03 ws-buyer-found-postcode pic x(7) value spaces.

       SCREEN SECTION.

       01 BUYER-MENU-SCREEN background-color 0 foreground-color 15 AUTO.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST BUYER MENU           |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 6 COL 29 "1. ADD BUYER".
         03 LINE 8 COL 29 "2. VIEW ALL BUYERS".
         03 LINE 10 COL 29 "3. SEARCH BUYERS".
         03 LINE 14 COL 29 "5. RETURN TO MENU".
         03 LINE 21 COL 29 value "MENU: [".
         03 pic x USING MENU-IN foreground-color 10 HIGHLIGHT PROMPT
            " ".
         03 value "]".
         03 LINE 6 COL 29 foreground-color 14 "1".
         03 LINE 8 COL 29 foreground-color 14 "2".
         03 LINE 10 COL 29 foreground-color 14 "3".
         03 LINE 14 COL 29 foreground-color 14 "5".

       copy "BuyerDetailsScreen.cpy".
       
       01 BUYER-SEARCH-SCREEN foreground-color 15 AUTO UPPER.
         03 blank screen.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST BUYER DETAILS        |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 line 5 col 2 "Enter details of record to amend.".

         03 LINE 7 COL 2 "BUYER ID       [".
         03 PIC z(4) using ws-BUYER-search-ID foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 8 COL 2 "BUYER NAME     [".
         03 pic X(20) using ws-BUYER-search-NAME foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "ADDRESS        [".
         03 PIC X(25) USING ws-buyer-search-address foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 10 COL 2 "POSTCODE       [".
         03 PIC X(7) USING ws-BUYER-search-POSTCODE foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 line 14 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 14 col 41 foreground-color 14 value
            "----------------------------------------".

         03 LINE 15 COL 2 "BUYER ID         ".
         03 PIC 9(4) from ws-buyer-found-id foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 16 COL 2 "BUYER NAME       ".
         03 PIC x(20) from ws-buyer-found-name foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 17 COL 2 "ADDRESS 1        ".
         03 pic X(25) from ws-buyer-found-address1 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 18 COL 2 "ADDRESS 2        ".
         03 PIC X(25) from ws-buyer-found-address2 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 19 COL 2 "ADDRESS 3        ".
         03 PIC x(25) from ws-buyer-found-address3 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 20 COL 2 "ADDRESS 4        ".
         03 PIC x(25) from ws-buyer-found-address4 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 21 COL 2 "POSTCODE         ".
         03 PIC x(7) from ws-buyer-found-postcode foreground-color 14
            HIGHLIGHT PROMPT " ".
         
         03 line 22 col 2 from ws-message foreground-color ws-color.

         03 line 23 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 23 col 41 foreground-color 14 value
            "----------------------------------------".
         03 line 24 col 2 "F1 - GO TO RECORD   F3 - NEW SEARCH".
         03 line 24 col 40 "F5 - PREV   F7 - NEXT".
         03 line 24 col 63 "ESC - BACK TO MENU".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 22 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 40 FOREGROUND-COLOR 14 "F5".
         03 line 24 col 52 FOREGROUND-COLOR 14 "F7".
         03 line 24 col 63 FOREGROUND-COLOR 14 "ESC".

       procedure division.

       PARA-300-OPEN-BUYER-SUB-SCREEN.

           Perform until false
               MOVE " " TO MENU-IN

               PERFORM UNTIL MENU-IN NOT EQUALS SPACES

                   DISPLAY BUYER-MENU-SCREEN
                   ACCEPT BUYER-MENU-SCREEN
                   EVALUATE MENU-IN
                       WHEN 1
                           PERFORM PARA-310-ADD-BUYER-SCREEN
                       WHEN 2
                           PERFORM PARA-320-OPEN-BUYER-VIEW
                       WHEN 3
                           PERFORM PARA-330-AMEND-BUYER
                       WHEN 5
                           CALL "Main"
                       WHEN OTHER
                           if key-code-1 = 0
                               call "Main"
                           end-if
                           MOVE " " TO MENU-IN
                   END-EVALUATE
               END-PERFORM
           end-perform.

       PARA-310-ADD-BUYER-SCREEN.

           perform PARA-315-GENERATE-BUYER-REFERENCE
           move 0 to ws-done
           perform until done
               DISPLAY BUYER-DETAILS-SCREEN
               ACCEPT BUYER-DETAILS-SCREEN
               if key-code-1 = 0
                   move 1 to ws-done
               end-if
               if key-code-1 = 1
                   OPEN i-O buyer-file
                   WRITE BUYER-RECORD
                   CLOSE BUYER-FILE
                   move 1 to ws-done
               end-if
               if key-code-1 = 3
                   initialize BUYER-RECORD
                   move WS-NEXT-ID to buyer-id
               end-if
           end-perform
           PERFORM PARA-300-OPEN-BUYER-SUB-SCREEN.

       PARA-315-GENERATE-BUYER-REFERENCE.
      *Search for next available index
           move 0 to WS-NEXT-ID
           move 0 to id-counter
           open i-o buyer-file
           perform until WS-NEXT-ID not equals 0
               add 1 to id-counter
               move id-counter to BUYER-ID
               read buyer-file
                   invalid key
                       move id-counter to WS-NEXT-ID
           end-perform
           close buyer-file
           move spaces to BUYER-RECORD
           move WS-NEXT-ID to buyer-id.

       PARA-320-OPEN-BUYER-VIEW.
           CALL "ViewAllBuyers".

       PARA-330-AMEND-BUYER.
           move 0 to ws-done
           move spaces to ws-message
           move 0 to ws-valid-buyer-found

           perform until done
               
               display BUYER-SEARCH-SCREEN
               accept BUYER-SEARCH-SCREEN

               if key-code-1 = 48 or key-code-1 = 49
                   move 0 to ws-valid-buyer-found
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize ws-buyer-search-fields
                   move spaces to ws-message
                   move 0 to ws-valid-buyer-found
               end-if

               if key-code-1 = 5
                   subtract 1 from ws-current-match
                   if ws-current-match = 0
                       move ws-total-matches to ws-current-match
                   end-if
               end-if

               if key-code-1 = 7
                   add 1 to ws-current-match
                   if ws-current-match > ws-total-matches
                       move 1 to ws-current-match
                   end-if
               end-if

               if ws-valid-buyer-found = 0 and not (
                   ws-buyer-search-id = 0 and
                   ws-buyer-search-name = spaces and
                   ws-buyer-search-address = spaces and
                   ws-buyer-search-postcode = spaces)
                   perform PARA-435-SEARCH
               end-if

               perform PARA-437-UPDATE-SEARCH-DISPLAY 
                   
               if key-code-1 = 1 and ws-valid-buyer-found equals 1
                   Call "AmendBuyer" using ws-buyer-found-id
                   initialize ws-buyer-found-fields
                   move 0 to ws-valid-buyer-found
                   perform PARA-435-SEARCH
                   perform PARA-437-UPDATE-SEARCH-DISPLAY

               end-if

           end-perform.

       PARA-435-SEARCH.
           move 0 to ws-end-of-file
           move 0 to ws-total-matches
           open input buyer-file
           perform until ws-end-of-file equals 1
               read buyer-file next record
                   at end
                       move 1 to ws-end-of-file
                   not at end
                       if (ws-buyer-search-id = 0 or
                         ws-buyer-search-id = buyer-id)
                           and
                         (ws-buyer-search-name = spaces or
                         ws-buyer-search-name = buyer-name)
                           and
                         (ws-buyer-search-address = spaces or
                         ws-buyer-search-address = BUYER-AL1 or
                         ws-buyer-search-address = BUYER-AL2 or
                         ws-buyer-search-address = BUYER-AL3 or
                         ws-buyer-search-address = BUYER-AL4)
                         and
                         (ws-buyer-search-postcode = spaces or
                         ws-buyer-search-postcode = buyer-postcode)

                           move 1 to ws-valid-buyer-found
                           add 1 to ws-total-matches
                           move 1 to ws-current-match

                           move buyer-id of buyer-record to 
                           match-buyer-id of match-array(
                           ws-total-matches)
                           move buyer-name of buyer-record to 
                           match-buyer-name of match-array(
                           ws-total-matches)
                           move BUYER-AL1 of BUYER-RECORD to 
                           match-buyer-address1 of match-array(
                           ws-total-matches)
                           move BUYER-AL2 of BUYER-RECORD to
                             match-buyer-address2 of match-array(
                               ws-total-matches)
                           move BUYER-AL3 of BUYER-RECORD to
                             match-buyer-address3 of match-array(
                               ws-total-matches)
                           move BUYER-AL4 of BUYER-RECORD to
                             match-buyer-address4 of match-array(
                               ws-total-matches)
                           move BUYER-POSTCODE of buyer-record to
                             match-buyer-postcode of match-array(
                             ws-total-matches)
                           
                       end-if
           end-perform

           close buyer-file.

       PARA-437-UPDATE-SEARCH-DISPLAY.
           if ws-valid-buyer-found equals 1
               move spaces to ws-message
               move ws-current-match to ws-current-match-hide
               move ws-total-matches to ws-total-matches-hide

               string function trim (ws-current-match-hide), "/",
                 function trim (ws-total-matches-hide),
                 " matching record(s) found." into ws-message

               move match-array(ws-current-match) to
                 ws-buyer-found-fields
               move 10 to ws-color

           else
               move "No record found." to
                 ws-message
               move 04 to ws-color
               initialize ws-buyer-found-fields
           end-if.
                     

       end program Buyer.