      *VIEWING.cbl
      *Handle menu screens and functionality to add/search a Viewing
      *
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. Viewing is recursive.

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


       01 ws-viewing-search-fields.
         03 ws-viewing-search-id pic 9(4) value 0.
         03 ws-viewing-search-date.
           05 ws-viewing-search-day pic 99 value 0.
           05 ws-viewing-search-month pic 99 value 0.
           05 ws-viewing-search-year pic 9999 value 0.
         03 ws-viewing-search-user-id pic 9(4) value 0.
         03 ws-viewing-search-prop-id pic 9(4) value 0.

       01 ws-message pic x(40) value spaces.
       01 ws-valid-VIEWING-found pic 9 value 0 comp.
       01 ws-end-of-file pic 9 value 0 comp.
       01 ws-color pic 99 value 15.


       01 ws-buyer-valid-message pic x(40) value spaces.
       01 ws-user-valid-message pic x(40) value spaces.
       01 ws-property-valid-message pic x(40) value spaces.
       01 ws-valid-buyer pic 9 value 0 comp.
       01 ws-valid-USER pic 9 value 0 comp.
       01 ws-valid-property pic 9 value 0 comp.

       01 ws-total-matches pic 9999 value 0 comp.
       01 ws-current-match pic 9999 value 0 comp.
       01 ws-total-matches-hide pic z(4).
       01 ws-current-match-hide pic z(4).

       01 match-array occurs 9999 times.
         03 match-viewing-id pic 9999.
         03 match-viewing-date pic x(10).
         03 match-viewing-time pic x(5).
         03 match-viewing-user-id pic 9(4).
         03 match-viewing-prop-id pic 9(4).

       01 ws-viewing-found-fields.
         03 ws-VIEWING-found-id pic 9(4) value 0.
         03 ws-VIEWING-found-DATE pic x(10) value spaces.
         03 ws-VIEWING-found-TIME pic x(5) value spaces.
         03 ws-VIEWING-found-USER-ID pic 9(4) value 0.
         03 ws-viewing-found-PROP-ID pic 9(4) value 0.

       
       

       SCREEN SECTION.

       01 VIEWING-MENU-SCREEN UPPER AUTO foreground-color 15.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST VIEWINGS MENU        |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 6 COL 29 "1. ADD VIEWING".
         03 LINE 8 COL 29 "2. VIEW ALL VIEWINGS".
         03 LINE 10 COL 29 "3. SEARCH VIEWINGS".
         03 LINE 14 COL 29 "5. RETURN TO MENU".
         03 LINE 21 COL 29 value "MENU: [".
         03 pic x USING MENU-IN FOREGROUND-COLOR 10   
            HIGHLIGHT PROMPT " ".
         03 value "]".
         03 LINE 6 COL 29 foreground-color 14 "1".
         03 LINE 8 COL 29 foreground-color 14 "2".
         03 LINE 10 COL 29 foreground-color 14 "3".
         03 LINE 14 COL 29 foreground-color 14 "5".

       copy "ViewingDetailsScreen.cpy.".

       


       01 VIEWING-SEARCH-SCREEN UPPER AUTO foreground-color 15.
         03 blank screen.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST VIEWING DETAILS      |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 line 5 col 2 "Enter details of record to search for.".

         03 LINE 7 COL 2 "VIEWING ID      [".
         03 PIC z(4) using ws-VIEWING-search-id foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 8 COL 2 "DATE            [".
         03 pic 99 using ws-VIEWING-search-day foreground-color 10
            HIGHLIGHT PROMPT " " blank when zero.
         03 VALUE "/".
         03 pic 99 using ws-VIEWING-search-month foreground-color 10
            HIGHLIGHT PROMPT " " blank when zero.
         03 VALUE "/".
         03 pic 9999 using ws-VIEWING-search-year foreground-color 10
            HIGHLIGHT PROMPT " " blank when zero.
         03 VALUE "]".

         03 LINE 9 COL 2 "USER ID         [".
         03 PIC z(4) USING ws-VIEWING-search-USER-ID foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 10 COL 2 "PROP ID         [".
         03 PIC z(4) USING ws-VIEWING-search-prop-ID foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 line 13 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 13 col 41 foreground-color 14 value
            "----------------------------------------".

         03 LINE 15 COL 2 "VIEWING ID       ".
         03 PIC 9(4) from ws-VIEWING-found-id foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 16 COL 2 "DATE             ".
         03 pic X(10) from ws-VIEWING-found-DATE foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 17 COL 2 "TIME             ".
         03 PIC X(5) from ws-VIEWING-found-TIME foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 18 COL 2 "USER ID          ".
         03 PIC 9(4) from ws-VIEWING-found-USER-ID foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 19 COL 2 "PROP ID          ".
         03 PIC 9(4) from ws-VIEWING-found-prop-ID foreground-color 14
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

       PARA-100-SETUP.
           display "USER-IS-ADMIN" upon environment-name
           accept ws-user-has-admin from environment-value.

       PARA-400-OPEN-VIEWING-SUB-SCREEN.
           Perform until false

               MOVE " " TO MENU-IN
               PERFORM UNTIL MENU-IN NOT EQUALS SPACE
                   DISPLAY VIEWING-MENU-SCREEN
                   ACCEPT VIEWING-MENU-SCREEN
                   EVALUATE MENU-IN
                       WHEN 1
                           PERFORM PARA-410-ADD-VIEWING-SCREEN
                       WHEN 2
                           PERFORM PARA-420-OPEN-VIEWING-VIEW
                       WHEN 3
                           PERFORM PARA-430-AMEND-VIEWING
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

       PARA-410-ADD-VIEWING-SCREEN.
           perform PARA-415-GENERATE-VIEWING-REFERENCE
           move 0 to ws-done
           perform until done

               perform PARA-440-VERIFY-USER
               perform PARA-450-VERIFY-BUYER
               perform PARA-460-VERIFY-PROPERTY

               DISPLAY VIEWING-DETAILS-SCREEN
               ACCEPT VIEWING-DETAILS-SCREEN
               if key-code-1 = 0
                   move 1 to ws-done
               end-if
               if key-code-1 = 1
                   OPEN i-O VIEWING-file
                   WRITE VIEWING-RECORD
                   CLOSE VIEWING-FILE
                   move 1 to ws-done
               end-if
               if key-code-1 = 3
                   initialize VIEWING-RECORD
                   move WS-NEXT-ID to VIEWING-id
               end-if

               if key-code-1 = 7 and ws-valid-USER = 1 and is-admin
                   Call "AmendUser" using VIEWING-USER-ID
               end-if

               if key-code-1 = 8 and ws-valid-property = 1
                   Call "AmendProperty" using VIEWING-PROPERTY-ID
               end-if

               if key-code-1 = 9 and ws-valid-buyer = 1
                   Call "AmendBuyer" using VIEWING-BUYER-ID
               end-if

           end-perform
           PERFORM PARA-400-OPEN-VIEWING-SUB-SCREEN.

       PARA-415-GENERATE-VIEWING-REFERENCE.
           move 0 to WS-NEXT-ID
           move 0 to id-counter
           open i-o VIEWING-file
           perform until WS-NEXT-ID not equals 0
               add 1 to id-counter
               move id-counter to VIEWING-ID
               read VIEWING-file
                   invalid key
                       move id-counter to WS-NEXT-ID
           end-perform
           close VIEWING-file
           initialize VIEWING-RECORD
           move WS-NEXT-ID to VIEWING-id.

       PARA-420-OPEN-VIEWING-VIEW.
           CALL "ViewAllViewings".

       PARA-430-AMEND-VIEWING.
           move 0 to ws-done
           move spaces to ws-message
           move 0 to ws-valid-VIEWING-found

           perform until done

               display VIEWING-SEARCH-SCREEN
               accept VIEWING-SEARCH-SCREEN

               if key-code-1 = 48 or key-code-1 = 49
                   move 0 to ws-valid-VIEWING-found
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize ws-viewing-search-fields
                   move spaces to ws-message
                   move 0 to ws-valid-VIEWING-found
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

               if ws-valid-viewing-found = 0 and not (
                   ws-VIEWING-search-DAY = 0 and
                   WS-VIEWing-search-month = 0 and
                   ws-viewing-search-year = 0 and
                   ws-viewing-search-id = 0 and
                   ws-viewing-search-PROP-ID = 0 and
                   ws-VIEWING-search-USER-ID = 0)
                       perform PARA-435-SEARCH
               end-if

               perform PARA-437-UPDATE-SEARCH-DISPLAY

               if key-code-1 = 1 and ws-valid-VIEWING-found = 1
                   Call "AmendViewing" using ws-VIEWING-found-id
                   initialize ws-viewing-found-fields
                   move 0 to ws-valid-VIEWING-found
                   perform PARA-435-SEARCH
                   perform PARA-437-UPDATE-SEARCH-DISPLAY
               end-if.

       PARA-435-SEARCH.
           move 0 to ws-end-of-file
           move 0 to ws-total-matches
           open input VIEWING-file
           perform until ws-end-of-file equals 1
               read VIEWING-file next record
                   at end
                       move 1 to ws-end-of-file
                   not at end
                       if (ws-VIEWING-search-id = 0 or
                         ws-VIEWING-search-id = VIEWING-id)
                           and
                         (ws-VIEWING-search-day = 0 or
                         ws-VIEWING-search-day = view-day)
                           and
                           (ws-VIEWING-search-month = 0 or
                         ws-VIEWING-search-month= view-month)
                           and
                           (ws-VIEWING-search-year = 0 or
                         ws-VIEWING-search-year = view-year)
                           and
                         (ws-VIEWING-search-USER-ID = 0 or
                         ws-VIEWING-search-USER-ID = VIEWING-user-id)
                           and                                          
                         (ws-viewing-search-PROP-ID = 0 or
                        ws-viewing-search-PROP-ID = viewing-property-id)

                           move 1 to ws-valid-VIEWING-found
                           add 1 to ws-total-matches
                           move 1 to ws-current-match
                           move viewing-id of viewing-record
                             to match-viewing-id of match-array(
                               ws-total-matches)
                           string view-day, "/", view-month, "/", 
                           view-year into match-viewing-date of 
                           match-array(ws-total-matches)
                           string view-hour, ":", view-mins into 
                           match-viewing-time of match-array(
                           ws-total-matches)
                           move VIEWING-USER-ID of
                             VIEWING-RECORD to
                             match-viewing-user-id of match-array(
                               ws-total-matches)
                           move VIEWING-PROPERTY-ID of
                             viewing-record to
                             match-viewing-prop-id of match-array(
                               ws-total-matches)
                       end-if
           end-perform

           close VIEWING-file.

       PARA-437-UPDATE-SEARCH-DISPLAY.
           if ws-valid-VIEWING-found equals 1
               move spaces to ws-message
               move ws-current-match to ws-current-match-hide
               move ws-total-matches to ws-total-matches-hide

               string function trim(ws-current-match-hide), "/",
                 function trim(ws-total-matches-hide),
                 " matching record(s) found." into ws-message

               move match-array(ws-current-match) to 
                 ws-viewing-found-fields
               move 10 to ws-color
           else
               move "No record found." to
                 ws-message
               move 04 to ws-color
               initialize ws-viewing-found-fields
           end-if.

       PARA-440-VERIFY-USER.
           move 0 to ws-valid-USER
           move "Enter a valid user reference." to
             ws-user-valid-message

           if not VIEWING-USER-ID = 0

               open i-o user-file
               move VIEWING-USER-ID to user-id of user-RECORD
               read user-file
                   invalid key
                       close user-file
                   not invalid key
                       move 1 to ws-valid-USER
                       move spaces to ws-user-valid-message
                       if is-admin
                           string "[F7] ", user-NAME of user-RECORD
                         into
                         ws-user-valid-message
                       else
                           move user-name of user-record to 
                           ws-user-valid-message
                       end-if
                       close user-file

           end-if.

       PARA-450-VERIFY-BUYER.
           move 0 to ws-valid-buyer
           move "Enter a valid buyer reference." to
             ws-buyer-valid-message.

           if not VIEWING-BUYER-ID = 0

               open i-o buyer-file
               move VIEWING-BUYER-ID to buyer-id of BUYER-RECORD
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

       PARA-460-VERIFY-PROPERTY.
           move 0 to ws-valid-property
           move "Enter a valid property reference." to
             ws-property-valid-message.

           if not VIEWING-PROPERTY-ID = 0

               open i-o property-file
               move VIEWING-PROPERTY-ID to property-id of
                 PROPERTY-RECORD
               read property-file
                   invalid key
                       close property-file
                   not invalid key
                       move 1 to ws-valid-property
                       move spaces to ws-property-valid-message
                       string "[F8] ", PROPERTY-AL1 of PROPERTY-RECORD,
                         " ", PROPERTY-POSTCODE OF PROPERTY-RECORD
                         into
                         ws-PROPERTY-valid-message
                       close property-file

           end-if.

       end program Viewing.