       identification division.
      *Authored by RADIO GAGO
       program-id. Main is recursive.

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
       
       SCREEN SECTION.

       01 MENU-SCREEN foreground-color 15 AUTO.
         03 BLANK SCREEN.
         03 line 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 line 2 col 29 foreground-color 14 value
            "|  EAST MAIN MENU            |".
         03 LINE COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 6 COL 29 "1. PROPERTIES".
         03 LINE 8 COL 29 "2. BUYERS".
         03 LINE 10 COL 29 "3. SELLERS".
         03 LINE 12 COL 29 "4. VIEWINGS".
         
         03 LINE 17 COL 29 "9. LOG OUT".
         03 LINE 21 COL 29 value "MENU: [".
         03 pic x USING MENU-IN foreground-color 10
         HIGHLIGHT PROMPT " ".
         03 value "]".
         03 LINE 6 COL 29 foreground-color 14 "1".
         03 LINE 8 COL 29 foreground-color 14 "2".
         03 LINE 10 COL 29 foreground-color 14 "3".
         03 LINE 12 COL 29 foreground-color 14 "4".
   
         03 LINE 17 COL 29 foreground-color 14 "9".

       procedure division.

       PARA-000-MAIN.

           perform Function-key-setup
           display "USER-IS-ADMIN" upon environment-name
           accept ws-user-has-admin from environment-value.

       PARA-200-START.

           MOVE " " TO MENU-IN
           PERFORM UNTIL MENU-IN NOT EQUALS SPACES
               DISPLAY MENU-SCREEN

               if is-admin
                   display "5. USER ACCESS" at line 14 col 29
                   display "5" at line 14 col 29 foreground-color 14
               end-if

               ACCEPT MENU-SCREEN
               EVALUATE MENU-IN
                   WHEN 1
                       call "Property"
                   WHEN 2
                       call "Buyer"
                   WHEN 3
                       call "Seller"
                   WHEN 4
                       call "Viewing"
                   WHEN 5
                       if is-admin
                           call "User"
                       else
                           move " " to menu-in
                       end-if
                   WHEN 9
                       call "Login"
                   WHEN OTHER
                       MOVE " " TO MENU-IN
               END-EVALUATE
           END-PERFORM.




       copy "FunctionKeySetup.cpy".



       end program Main.
