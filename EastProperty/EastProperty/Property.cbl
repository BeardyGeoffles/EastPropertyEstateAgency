      *PROPERTY.cbl
      *Handle menu screens and functionality to add/search a Property
      *
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. Property is recursive.
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

       01 ws-pound pic x(1) value x'9c'.

       01 ws-property-search-fields.
         03 ws-property-search-id pic 9(4) value 0.
         03 ws-property-search-address pic x(25) value spaces.
         03 ws-property-search-postcode pic x(7) value spaces.
         03 ws-property-search-type pic x(20) value spaces.
         03 ws-property-search-bedrooms pic 99 value 0.
         03 ws-property-search-heating pic x value space.
         03 ws-property-search-garden pic x value space.
         03 ws-property-search-parking pic x value space.
         03 ws-property-search-seller-id pic 9(4) value 0.
         03 ws-property-search-offer-status pic x value space.

       01 ws-message pic x(40) value spaces.
       01 ws-valid-property-found pic 9 value 0 comp.
       01 ws-end-of-file pic 9 value 0 comp.
       01 ws-color pic 99 value 15.

       01 ws-buyer-valid-message pic x(40) value spaces.
       01 ws-seller-valid-message pic x(40) value spaces.
       01 ws-valid-buyer pic 9 value 0 comp.
       01 ws-valid-seller pic 9 value 0 comp.

       01 ws-total-matches pic 9999 value 0 comp.
       01 ws-current-match pic 9999 value 0 comp.
       01 ws-total-matches-hide pic z(4).
       01 ws-current-match-hide pic z(4).

      *Array to hold all search results
       01 match-array occurs 9999 times.
         03 match-property-id pic 9999.
         03 match-property-address1 pic x(25).
         03 match-property-address2 pic x(25).
         03 match-property-address3 pic x(25).
         03 match-property-address4 pic x(25).
         03 match-property-postcode pic x(7).
         03 match-property-seller-id pic 9(4).

      *Display fields for the currently selected record
       01 ws-property-found-fields.
         03 ws-property-found-id pic 9(4) value 0.
         03 ws-property-found-address1 pic x(25) value spaces.
         03 ws-property-found-address2 pic x(25) value spaces.
         03 ws-property-found-address3 pic x(25) value spaces.
         03 ws-property-found-address4 pic x(25) value spaces.
         03 ws-property-found-postcode pic x(7) value spaces.
         03 ws-property-found-seller-id pic 9(4) value 0.

       SCREEN SECTION.

       01 PROPERTY-MENU-SCREEN foreground-color 15 AUTO.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST PROPERTY MENU        |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 6 COL 29 "1. ADD PROPERTY".
         03 LINE 8 COL 29 "2. VIEW ALL PROPERTIES".
         03 LINE 10 COL 29 "3. SEARCH PROPERTIES".
         03 LINE 14 COL 29 "5. RETURN TO MENU".
         03 LINE 21 COL 29 value "MENU: [".
         03 pic x USING MENU-IN FOREGROUND-COLOR 10
            HIGHLIGHT PROMPT " ".
         03 value "]".
         03 LINE 6 COL 29 foreground-color 14 "1".
         03 LINE 8 COL 29 foreground-color 14 "2".
         03 LINE 10 COL 29 foreground-color 14 "3".
         03 LINE 14 COL 29 foreground-color 14 "5".

       copy "PropertyDetailsScreen.cpy".
      


       01 PROPERTY-SEARCH-SCREEN foreground-color 15 AUTO UPPER.
         03 blank screen.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST PROPERTY DETAILS     |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 line 5 col 2 "Enter details of record to search for.".

         03 LINE 7 COL 2 "PROPERTY ID    [".
         03 PIC z(4) using ws-property-search-id foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 8 COL 2 "ADDRESS        [".
         03 PIC X(25) USING ws-property-search-address foreground-color
            10 HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "POSTCODE       [".
         03 PIC X(7) USING ws-property-search-postcode foreground-color
            10 HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 10 COL 2 "PROPERTY TYPE  [".
         03 pic X(20) using ws-property-search-type foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 11 COL 2 "NO OF BEDROOMS [".
         03 pic z(2) using ws-property-search-bedrooms foreground-color
            10 HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 11 COL 25 "CENTRAL HEATING (Y/N) [".
         03 PIC X USING ws-property-search-heating foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 12 COL 2 "GARDEN (Y/N)   [".
         03 PIC X USING ws-property-search-garden foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 12 COL 25 "PARKING (Y/N)         [".
         03 PIC X USING ws-property-search-parking foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 LINE 13 COL 2 "SELLER ID      [".
         03 pic z(4) using ws-property-search-seller-id foreground-color
            10 HIGHLIGHT PROMPT " ".
         03 value "]".
         03 LINE 13 COL 25
            "STATUS (L)isted (S)old (O)ffer (W)ithdrawn [".
         03 PIC X USING ws-property-search-offer-status foreground-color
            10 HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 13 COL 33 FOREGROUND-COLOR 14 "L".
         03 LINE 13 COL 42 FOREGROUND-COLOR 14 "S".
         03 LINE 13 COL 49 FOREGROUND-COLOR 14 "O".
         03 LINE 13 COL 57 FOREGROUND-COLOR 14 "W".

         03 line 14 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 14 col 41 foreground-color 14 value
            "----------------------------------------".

         03 LINE 15 COL 2 "PROPERTY ID      ".
         03 PIC 9(4) from ws-property-found-id foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 16 COL 2 "SELLER ID        ".
         03 PIC 9(4) from ws-property-found-seller-id foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 17 COL 2 "ADDRESS 1        ".
         03 pic X(25) from ws-property-found-address1 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 18 COL 2 "ADDRESS 2        ".
         03 PIC X(25) from ws-property-found-address2 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 19 COL 2 "ADDRESS 3        ".
         03 PIC x(25) from ws-property-found-address3 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 20 COL 2 "ADDRESS 4        ".
         03 PIC x(25) from ws-property-found-address4 foreground-color
            14 HIGHLIGHT PROMPT " ".

         03 LINE 21 COL 2 "POSTCODE         ".
         03 PIC x(7) from ws-property-found-postcode foreground-color 14
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

       PARA-400-OPEN-PROPERTY-SUB-SCREEN.
           Perform until false

               MOVE " " TO MENU-IN
               PERFORM UNTIL MENU-IN NOT EQUALS SPACE
                   DISPLAY PROPERTY-MENU-SCREEN
                   ACCEPT PROPERTY-MENU-SCREEN
                   EVALUATE MENU-IN
                       WHEN 1
                           PERFORM PARA-410-ADD-PROPERTY-SCREEN
                       WHEN 2
                           PERFORM PARA-420-OPEN-PROPERTY-VIEW
                       WHEN 3
                           PERFORM PARA-430-AMEND-PROPERTY
                       WHEN 5
                           call "Main"
                       WHEN OTHER
                           if key-code-1 = 0
                               call "Main"
                           end-if
                           MOVE " " TO MENU-IN
                   END-EVALUATE
               end-perform
           END-PERFORM.

       PARA-410-ADD-PROPERTY-SCREEN.
           perform PARA-415-GENERATE-PROPERTY-REFERENCE
           move 0 to ws-done
           perform until done

               perform PARA-440-VERIFY-SELLER
               perform PARA-450-VERIFY-BUYER

               DISPLAY PROPERTY-DETAILS-SCREEN
               ACCEPT PROPERTY-DETAILS-SCREEN
               if key-code-1 = 0
                   move 1 to ws-done
               end-if
               if key-code-1 = 1
                   OPEN i-O property-file
                   WRITE PROPERTY-RECORD
                   CLOSE PROPERTY-FILE
                   move 1 to ws-done
               end-if
               if key-code-1 = 3
                   initialize PROPERTY-RECORD
                   move WS-NEXT-ID to property-id
               end-if

               if key-code-1 = 7 and ws-valid-seller = 1
                   Call "AmendSeller" using PROP-SELLER-ID
               end-if

               if key-code-1 = 9 and ws-valid-buyer = 1
                   Call "AmendBuyer" using PROP-BUYER-ID
               end-if

           end-perform
           PERFORM PARA-400-OPEN-PROPERTY-SUB-SCREEN.

       PARA-415-GENERATE-PROPERTY-REFERENCE.
           move 0 to WS-NEXT-ID
           move 0 to id-counter
           open i-o property-file
           perform until WS-NEXT-ID not equals 0
               add 1 to id-counter
               move id-counter to property-ID
               read property-file
                   invalid key
                       move id-counter to WS-NEXT-ID
           end-perform
           close property-file
           initialize PROPERTY-RECORD
           move WS-NEXT-ID to property-id.

       PARA-420-OPEN-PROPERTY-VIEW.
           CALL "ViewAllProperties".

       PARA-430-AMEND-PROPERTY.
           move 0 to ws-done
           move spaces to ws-message
           move 0 to ws-valid-property-found

           perform until done

               display property-SEARCH-SCREEN
               accept property-SEARCH-SCREEN

               if key-code-1 = 48 or key-code-1 = 49
                   move 0 to ws-valid-property-found
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize ws-property-search-fields
                   move spaces to ws-message
                   move 0 to ws-valid-property-found
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

               if ws-valid-property-found = 0 and not (
                 ws-property-search-address = spaces and
                 ws-property-search-bedrooms = 0 and
                 ws-property-search-garden = space and
                 ws-property-search-heating = space and
                 ws-property-search-id = 0 and
                 ws-property-search-offer-status = space and
                 ws-property-search-parking = space and
                 ws-property-search-postcode = spaces and
                 ws-property-search-seller-id = 0 and
                 ws-property-search-type = spaces)
                   perform PARA-435-SEARCH
               end-if

               perform PARA-437-UPDATE-SEARCH-DISPLAY

               if key-code-1 = 1 and ws-valid-property-found equals 1
                   Call "AmendProperty" using ws-property-found-id
                   initialize ws-property-found-fields
                   move 0 to ws-valid-property-found
                   perform PARA-435-SEARCH
                   perform PARA-437-UPDATE-SEARCH-DISPLAY

               end-if

           end-perform.

       PARA-435-SEARCH.
           move 0 to ws-end-of-file
           move 0 to ws-total-matches
           open input property-file
           perform until ws-end-of-file equals 1
               read property-file next record
                   at end
                       move 1 to ws-end-of-file
                   not at end
                       if (ws-property-search-id = 0 or
                         ws-property-search-id = property-id)
                           and
                         (ws-property-search-address = spaces or
                         ws-property-search-address = PROPERTY-AL1 or
                         ws-property-search-address = PROPERTY-AL2 or
                         ws-property-search-address = PROPERTY-AL3 or
                         ws-property-search-address = PROPERTY-AL4)
                           and
                         (ws-property-search-postcode = spaces or
                   ws-property-search-postcode = PROPERTY-POSTCODE)
                           and
                         (ws-property-search-type = spaces or
                         ws-property-search-type = PROPERTY-TYPE)
                           and
                         (ws-property-search-bedrooms = 0 or 
                       ws-property-search-bedrooms = PROPERTY-BEDS)
                           and
                        (ws-property-search-garden = spaces or
                         ws-property-search-garden = PROPERTY-GARDEN) 
                         and
                         (ws-property-search-heating = space or
                          ws-property-search-heating = CENTRAL-HEATING) 
                          and
                          (ws-property-search-parking = space or
                           ws-property-search-parking = OFF-RD-PARKING)
                           and
                           (ws-property-search-offer-status = space or
                           ws-property-search-offer-status = 
                           OFFER-STATUS)
                           and
                           (ws-property-search-seller-id = 0 or
                           ws-property-search-seller-id = 
                           PROP-SELLER-ID)


                           move 1 to ws-valid-property-found
                           add 1 to ws-total-matches
                           move 1 to ws-current-match
                           move property-id of property-record
                             to match-property-id of match-array(
                               ws-total-matches)
                           move PROPERTY-AL1 of property-RECORD
                             to match-property-address1
                             of match-array(
                               ws-total-matches)
                           move PROPERTY-AL2 of property-RECORD
                             to match-property-address2 of
                             match-array(ws-total-matches)
                           move PROPERTY-AL3 of property-RECORD
                             to match-property-address3 of
                             match-array(ws-total-matches)
                           move PROPERTY-AL4 of property-RECORD
                             to match-property-address4 of
                             match-array(ws-total-matches)
                           move PROPERTY-POSTCODE of
                             property-RECORD to
                             match-property-postcode of match-array(
                               ws-total-matches)
                           move prop-seller-id of
                             property-record to
                             match-property-seller-id of match-array(
                               ws-total-matches)
                       end-if
           end-perform

           close property-file.

       PARA-437-UPDATE-SEARCH-DISPLAY.
           if ws-valid-property-found equals 1
               move spaces to ws-message
               move ws-current-match to ws-current-match-hide
               move ws-total-matches to ws-total-matches-hide

               string function trim(ws-current-match-hide), "/",
                 function trim(ws-total-matches-hide),
                 " matching record(s) found." into ws-message

               move match-array(ws-current-match) to
                 ws-property-found-fields
               move 10 to ws-color
           else
               move "No record found." to
                 ws-message
               move 04 to ws-color
               initialize ws-property-found-fields
           end-if.

       PARA-440-VERIFY-SELLER.
           move 0 to ws-valid-seller
           move "Enter a valid seller reference." to
             ws-seller-valid-message

           if not PROP-SELLER-ID = 0

               open i-o seller-file
               move PROP-SELLER-ID to seller-id of SELLER-RECORD
               read seller-file
                   invalid key
                       close seller-file
                   not invalid key
                       move 1 to ws-valid-seller
                       move spaces to ws-seller-valid-message
                       string "[F7] ", SELLER-NAME of SELLER-RECORD
                         into
                         ws-seller-valid-message
                       close seller-file

           end-if.

       PARA-450-VERIFY-BUYER.
           move 0 to ws-valid-buyer
           move "Enter a valid buyer reference." to
             ws-buyer-valid-message.

           if not PROP-BUYER-ID = 0

               open i-o buyer-file
               move PROP-BUYER-ID to buyer-id of BUYER-RECORD
               read buyer-file
                   invalid key
                       close buyer-file
                   not invalid key
                       move 1 to ws-valid-buyer
                       move spaces to ws-buyer-valid-message
                       string "[F9] ", BUYER-NAME of BUYER-RECORD into
                         ws-buyer-valid-message
                       close buyer-file

           end-if.

       end program Property.
