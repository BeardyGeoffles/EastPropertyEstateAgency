      *USER.cbl
      *Handle menu screens and functionality to add/search a User
      *
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. User is recursive.

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

       01 ws-user-search-fields.
         03 ws-user-search-id pic 9(4) value 0.
         03 ws-user-search-name pic x(10) value spaces.
         03 ws-user-search-admin pic x value space.

       01 ws-password-1 pic x(10) value spaces.
       01 ws-password-2 pic x(10) value spaces.
       01 ws-secure-password pic x(10) value spaces.

       01 ws-message pic x(40) value spaces.
       01 ws-valid-user-found pic 9 value 0 comp.
       01 ws-end-of-file pic 9 value 0 comp.
       01 ws-color pic 99 value 15.

       01 ws-total-matches pic 9999 value 0 comp.
       01 ws-current-match pic 9999 value 0 comp.
       01 ws-total-matches-hide pic z(4).
       01 ws-current-match-hide pic z(4).

      *Array to hold all search results
       01 match-array occurs 9999 times.
         03 match-user-id pic 9999.
         03 match-user-name pic x(10).
         03 match-user-admin pic x.

      *Display fields for the currently selected record
       01 ws-user-found-fields.
         03 ws-user-found-id pic 9(4) value 0.
         03 ws-user-found-name pic x(10) value spaces.
         03 ws-user-found-admin pic x value space.


       SCREEN SECTION.

       01 USER-MENU-SCREEN background-color 0 foreground-color 15 AUTO.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST USER ADMIN MENU      |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 6 COL 29 "1. ADD USER".
         03 LINE 8 COL 29 "2. VIEW ALL USERS".
         03 LINE 10 COL 29 "3. SEARCH USERS".
         03 LINE 14 COL 29 "5. RETURN TO MENU".
         03 LINE 21 COL 29 value "MENU: [".
         03 pic x USING MENU-IN foreground-color 10 HIGHLIGHT PROMPT
            " ".
         03 value "]".
         03 LINE 6 COL 29 foreground-color 14 "1".
         03 LINE 8 COL 29 foreground-color 14 "2".
         03 LINE 10 COL 29 foreground-color 14 "3".
         03 LINE 14 COL 29 foreground-color 14 "5".

       copy "UserDetailsScreen.cpy".
       


       01 USER-SEARCH-SCREEN foreground-color 15 AUTO UPPER.
         03 blank screen.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST USER DETAILS         |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".

         03 line 5 col 2 "Enter details of record to amend.".

         03 LINE 7 COL 2 "USER ID        [".
         03 PIC z(4) using ws-USER-search-ID foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 8 COL 2 "USER NAME      [".
         03 pic X(10) using ws-USER-search-NAME foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".
         03 LINE 9 COL 2 "HAS ADMIN      [".
         03 pic X using ws-USER-search-admin foreground-color 10
            HIGHLIGHT PROMPT " ".
         03 VALUE "]".

         03 line 14 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 14 col 41 foreground-color 14 value
            "----------------------------------------".

         03 LINE 15 COL 2 "USER ID          ".
         03 PIC 9(4) from ws-user-found-id foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 16 COL 2 "USER NAME        ".
         03 PIC x(10) from ws-user-found-name foreground-color 14
            HIGHLIGHT PROMPT " ".

         03 LINE 17 COL 2 "HAS ADMIN        ".
         03 PIC x from ws-user-found-admin foreground-color 14
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

       PARA-300-OPEN-USER-SUB-SCREEN.

           Perform until false
               MOVE " " TO MENU-IN

               PERFORM UNTIL MENU-IN NOT EQUALS SPACES

                   DISPLAY USER-MENU-SCREEN
                   ACCEPT USER-MENU-SCREEN
                   EVALUATE MENU-IN
                       WHEN 1
                           PERFORM PARA-310-ADD-USER-SCREEN
                       WHEN 2
                           PERFORM PARA-320-OPEN-USER-VIEW
                       WHEN 3
                           PERFORM PARA-330-AMEND-USER
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

       PARA-310-ADD-USER-SCREEN.

           perform PARA-315-GENERATE-USER-REFERENCE

           move spaces to ws-secure-password
           move spaces to ws-password-1
           move spaces to ws-password-2
           move spaces to ws-message

           move 0 to ws-done
           perform until done
               DISPLAY USER-DETAILS-SCREEN
               ACCEPT USER-DETAILS-SCREEN

               if ws-password-1 not equal spaces
                   if ws-password-1 = ws-password-2
                       move ws-password-1 to ws-secure-password
                       move spaces to ws-password-1
                       move spaces to ws-password-2
                       move "Password is valid." to ws-message
                       move 10 to ws-color
                   else
                       move "Passwords do not match. Please re-enter. "
                         to  ws-message
                       move 04 to ws-color
                       move spaces to ws-password-1
                       move spaces to ws-password-2
                       move spaces to ws-secure-password
                   end-if
               else
                   if ws-secure-password equal spaces
                       move "Password cannot be blank." to ws-message
                       move 04 to ws-color
                   end-if
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if
               if key-code-1 = 1 and ws-secure-password not equal spaces
                   move ws-secure-password to USER-PASSWORD
                   OPEN i-O user-file
                   WRITE USER-RECORD
                   CLOSE USER-FILE
                   move 1 to ws-done
               end-if
               if key-code-1 = 3
                   initialize user-record
                   move spaces to ws-password-1
                   move spaces to ws-password-2
                   move spaces to ws-secure-password
                   move spaces to ws-message
                   move WS-NEXT-ID to user-id
               end-if
           end-perform
           PERFORM PARA-300-OPEN-USER-SUB-SCREEN.

       PARA-315-GENERATE-USER-REFERENCE.
           move 0 to WS-NEXT-ID
           move 0 to id-counter
           open i-o user-file
           perform until WS-NEXT-ID not equals 0
               add 1 to id-counter
               move id-counter to user-ID
               read user-file
                   invalid key
                       move id-counter to WS-NEXT-ID
           end-perform
           close user-file
           move spaces to user-RECORD
           move WS-NEXT-ID to user-id.

       PARA-320-OPEN-USER-VIEW.
           CALL "ViewAllUsers".

       PARA-330-AMEND-USER.
           move 0 to ws-done
           move spaces to ws-message
           move 0 to ws-valid-user-found

           perform until done

               display user-SEARCH-SCREEN
               accept user-SEARCH-SCREEN

               if key-code-1 = 48 or key-code-1 = 49
                   move 0 to ws-valid-user-found
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize ws-user-search-fields
                   move spaces to ws-message
                   move 0 to ws-valid-user-found
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

               if ws-valid-user-found = 0 and not (
                  ws-user-search-id = 0 and
                  ws-user-search-name = spaces and
                  ws-user-search-admin = space)
                   perform PARA-435-SEARCH
               end-if

               perform PARA-437-UPDATE-SEARCH-DISPLAY

               if key-code-1 = 1 and ws-valid-user-found equals 1
                   Call "AmendUser" using ws-user-found-id
                   initialize ws-user-found-fields
                   move 0 to ws-valid-user-found
                   perform PARA-435-SEARCH
                   perform PARA-437-UPDATE-SEARCH-DISPLAY

               end-if

           end-perform.

       PARA-435-SEARCH.
           move 0 to ws-end-of-file
           move 0 to ws-total-matches
           open input user-file
           perform until ws-end-of-file equals 1
               read user-file next record
                   at end
                       move 1 to ws-end-of-file
                   not at end
                       if (ws-user-search-id = 0 or
                         ws-user-search-id = user-id)
                           and
                         (ws-user-search-name = spaces or
                         ws-user-search-name = user-name)
                           and
                           (ws-user-search-admin = space or
                            ws-user-search-admin = USER-HAS-ADMIN)

                           move 1 to ws-valid-user-found
                           add 1 to ws-total-matches
                           move 1 to ws-current-match

                           move user-id of user-record to match-user-id
                           of match-array(ws-total-matches)
                           move user-name of user-record to 
                           match-user-name of match-array(
                           ws-total-matches)
                           move user-has-admin of user-record to
                             match-user-admin of match-array(
                             ws-total-matches)

                       end-if
           end-perform

           close user-file.

       PARA-437-UPDATE-SEARCH-DISPLAY.
           if ws-valid-user-found equals 1
               move spaces to ws-message
               move ws-current-match to ws-current-match-hide
               move ws-total-matches to ws-total-matches-hide

               string function trim (ws-current-match-hide), "/",
                 function trim (ws-total-matches-hide),
                 " matching record(s) found." into ws-message

               move match-array(ws-current-match) to
                 ws-user-found-fields
               move 10 to ws-color

           else
               move "No record found." to
                 ws-message
               move 04 to ws-color
               initialize ws-user-found-fields
           end-if.

       end program User.