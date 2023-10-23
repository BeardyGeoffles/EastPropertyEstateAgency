      *SELLER.cbl
      *Handle menu screens and functionality to add/search a Seller
      *
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. Seller is recursive.
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


       01 ws-seller-search-fields.
         03 ws-seller-search-id pic 9(4) value 0.
         03 ws-seller-search-name pic x(20) value spaces.
         03 ws-seller-search-address pic x(25) value spaces.
         03 ws-seller-search-postcode pic x(7) value spaces.

       01 ws-message pic x(30) value spaces.
       01 ws-valid-seller-found pic 9 value 0 comp.
       01 ws-end-of-file pic 9 value 0 comp.
       01 ws-color pic 99 value 15.


       01 ws-total-matches pic 9999 value 0 comp.
       01 ws-current-match pic 9999 value 0 comp.
       01 ws-total-matches-hide pic z(4).
       01 ws-current-match-hide pic z(4).

      *Array to hold all search results
       01 match-array occurs 9999 times.
         03 match-seller-id pic 9999.
         03 match-seller-name pic x(20).
         03 match-seller-address1 pic x(25).
         03 match-seller-address2 pic x(25).
         03 match-seller-address3 pic x(25).
         03 match-seller-address4 pic x(25).
         03 match-seller-postcode pic x(7).

      *Display fields for the currently selected record
       01 ws-seller-found-fields.
         03 ws-seller-found-id pic 9(4) value 0.
         03 ws-seller-found-name pic x(20) value spaces.
         03 ws-seller-found-address1 pic x(25) value spaces.
         03 ws-seller-found-address2 pic x(25) value spaces.
         03 ws-seller-found-address3 pic x(25) value spaces.
         03 ws-seller-found-address4 pic x(25) value spaces.
         03 ws-seller-found-postcode pic x(7) value spaces.

       SCREEN SECTION.

       01 SELLER-MENU-SCREEN background-color 0 foreground-color 15
                             AUTO.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST SELLER MENU          |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 6 COL 29 "1. ADD SELLER".
         03 LINE 8 COL 29 "2. VIEW ALL SELLERS".
         03 LINE 10 COL 29 "3. SEARCH SELLERS".
         03 LINE 14 COL 29 "5. RETURN TO MENU".
         03 LINE 21 COL 29 value "MENU: [".
         03 pic x USING MENU-IN foreground-color 10 HIGHLIGHT PROMPT
            " ".
         03 value "]".
         03 LINE 6 COL 29 foreground-color 14 "1".
         03 LINE 8 COL 29 foreground-color 14 "2".
         03 LINE 10 COL 29 foreground-color 14 "3".
         03 LINE 14 COL 29 foreground-color 14 "5".

       copy "SellerDetailsScreen.cpy".
     


       01 SELLER-SEARCH-SCREEN foreground-color 15 AUTO UPPER.
         03 blank screen.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST SELLER DETAILS       |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 5 col 2 "Enter details of record to amend.".

         03 LINE 7 COL 2 "SELLER ID      [".
         03 PIC z(4) using ws-seller-search-id foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 8 COL 2 "SELLER NAME    [".
         03 pic X(20) using ws-seller-search-name foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "ADDRESS        [".
         03 PIC X(25) USING ws-seller-search-address foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 10 COL 2 "POSTCODE       [".
         03 PIC X(7) USING ws-seller-search-postcode foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         
         03 line 14 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 14 col 41 foreground-color 14 value
            "----------------------------------------".

         03 LINE 15 COL 2 "SELLER ID        ".
         03 PIC 9(4) from ws-seller-found-id foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 16 COL 2 "SELLER NAME      ".
         03 PIC x(20) from ws-seller-found-name foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 17 COL 2 "ADDRESS 1        ".
         03 pic X(25) from ws-seller-found-address1 foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 18 COL 2 "ADDRESS 2        ".
         03 PIC X(25) from ws-seller-found-address2 foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 19 COL 2 "ADDRESS 3        ".
         03 PIC x(25) from ws-seller-found-address3 foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 20 COL 2 "ADDRESS 4        ".
         03 PIC x(25) from ws-seller-found-address4 foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 21 COL 2 "POSTCODE         ".
         03 PIC x(7) from ws-seller-found-postcode foreground-color 14
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

       PARA-300-OPEN-SELLER-SUB-SCREEN.

           Perform until false
               MOVE " " TO MENU-IN
               PERFORM UNTIL MENU-IN NOT EQUALS SPACES
                   DISPLAY SELLER-MENU-SCREEN
                   ACCEPT SELLER-MENU-SCREEN
                   EVALUATE MENU-IN
                       WHEN 1
                           PERFORM PARA-310-ADD-SELLER-SCREEN
                       WHEN 2
                           PERFORM PARA-320-OPEN-SELLER-VIEW
                       WHEN 3
                           PERFORM PARA-330-AMEND-SELLER
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

       PARA-310-ADD-SELLER-SCREEN.
           perform PARA-315-GENERATE-SELLER-REFERENCE
           move 0 to ws-done
           perform until done
               DISPLAY SELLER-DETAILS-SCREEN
               ACCEPT SELLER-DETAILS-SCREEN
               if key-code-1 = 0
                   move 1 to ws-done
               end-if
               if key-code-1 = 1
                   OPEN i-O seller-file
                   WRITE SELLER-RECORD
                   CLOSE SELLER-FILE
                   move 1 to ws-done
               end-if
               if key-code-1 = 3
                   initialize SELLER-record
                   move WS-NEXT-ID to seller-id
               end-if
           end-perform
           PERFORM PARA-300-OPEN-SELLER-SUB-SCREEN.

       PARA-315-GENERATE-SELLER-REFERENCE.
           move 0 to WS-NEXT-ID
           move 0 to id-counter
           open i-o seller-file
           perform until WS-NEXT-ID not equals 0
               add 1 to id-counter
               move id-counter to SELLER-ID
               read seller-file
                   invalid key
                       move id-counter to WS-NEXT-ID
           end-perform
           close seller-file
           move spaces to SELLER-RECORD
           move WS-NEXT-ID to SELLER-id.

       PARA-320-OPEN-SELLER-VIEW.
           CALL "ViewAllSellers".

       PARA-330-AMEND-SELLER.
           move 0 to ws-done
           move spaces to ws-message
           move 0 to ws-valid-seller-found

           perform until done
               
               display SELLER-SEARCH-SCREEN
               accept SELLER-SEARCH-SCREEN

               if key-code-1 = 48 or key-code-1 = 49
                   move 0 to ws-valid-seller-found
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize ws-seller-search-fields
                   move spaces to ws-message
                   move 0 to ws-valid-seller-found
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

               if ws-valid-seller-found = 0 and not (
                 ws-seller-search-id = 0 and
                 ws-seller-search-name = spaces and
                 ws-seller-search-address = spaces and
                 ws-seller-search-postcode = spaces)
                   perform PARA-435-SEARCH
               end-if

               perform PARA-437-UPDATE-SEARCH-DISPLAY

               if key-code-1 = 1 and ws-valid-seller-found equals 1
                   Call "AmendSeller" using ws-seller-found-id
                   initialize ws-seller-found-fields
                   move 0 to ws-valid-seller-found
                   perform PARA-435-SEARCH
                   perform PARA-437-UPDATE-SEARCH-DISPLAY

               end-if

           end-perform.

       PARA-435-SEARCH.
           move 0 to ws-end-of-file
           move 0 to ws-total-matches
           open input seller-file
           perform until ws-end-of-file equals 1
               read seller-file next record
                   at end
                       move 1 to ws-end-of-file
                   not at end
                       if (ws-seller-search-id = 0 or
                         ws-seller-search-id = seller-id)
                         and
                         (ws-seller-search-name = spaces or
                         ws-seller-search-name = seller-name)
                         and
                         (ws-seller-search-address = spaces or
                         ws-seller-search-address = seller-AL1 or
                         ws-seller-search-address = seller-AL2 or
                         ws-seller-search-address = seller-AL3 or
                         ws-seller-search-address = seller-AL4)
                         and
                         (ws-seller-search-postcode = spaces or
                         ws-seller-search-postcode = seller-postcode)

                           move 1 to ws-valid-seller-found
                           add 1 to ws-total-matches
                           move 1 to ws-current-match

                           move seller-id of seller-record to
                             match-seller-id of match-array(
                               ws-total-matches)
                           move seller-name of seller-record to
                             match-seller-name of match-array(
                               ws-total-matches)
                           move seller-AL1 of seller-RECORD to
                             match-seller-address1 of match-array(
                               ws-total-matches)
                           move seller-AL2 of seller-RECORD to
                             match-seller-address2 of match-array(
                               ws-total-matches)
                           move seller-AL3 of seller-RECORD to
                             match-seller-address3 of match-array(
                               ws-total-matches)
                           move seller-AL4 of seller-RECORD to
                             match-seller-address4 of match-array(
                               ws-total-matches)
                           move seller-POSTCODE of seller-record to
                             match-seller-postcode of match-array(
                               ws-total-matches)

                       end-if
           end-perform

           close seller-file.

       PARA-437-UPDATE-SEARCH-DISPLAY.
           if ws-valid-seller-found equals 1
               move spaces to ws-message
               move ws-current-match to ws-current-match-hide
               move ws-total-matches to ws-total-matches-hide

               string function trim (ws-current-match-hide), "/",
                 function trim (ws-total-matches-hide),
                 " matching record(s) found." into ws-message

               move match-array(ws-current-match) to
                 ws-seller-found-fields
               move 10 to ws-color

           else
               move "No record found." to
                 ws-message
               move 04 to ws-color
               initialize ws-seller-found-fields
           end-if.


       end program Seller.