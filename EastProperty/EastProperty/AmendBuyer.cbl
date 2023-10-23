      *AMENDBUYER.cbl
      *Handle screen and function keys for amending a buyer
      *Must be passed a valid Buyer ID to amend
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. AmendBuyer.

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

       linkage section.

       01 ws-buyer-id pic z(4) value 0.

       SCREEN SECTION.

       copy "BuyerDetailsScreen.cpy".
       


      *  The following 6 lines are not part of the copybook

         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 55 "ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".

       procedure division using ws-buyer-id.

       SETUP.
           display "USER-IS-ADMIN" upon environment-name
           accept ws-user-has-admin from environment-value.

       MAIN.
           move ws-buyer-id to buyer-id of BUYER-RECORD

           open i-o buyer-file

           read buyer-file

           close buyer-file.

       PARA-100-AMEND-BUYER-SCREEN.

           move 0 to ws-done

           perform until done

               DISPLAY BUYER-DETAILS-SCREEN

               if is-admin 
                   display "F5 - DELETE" at line 24 col 41
                   display "F5" at line 24 col 41 foreground-color 14
               end-if

               ACCEPT BUYER-DETAILS-SCREEN

               if key-code-1 = 0           *> ESC
                   move 1 to ws-done
               end-if

               if key-code-1 = 1           *> F1
                   OPEN i-O buyer-file

                   REWRITE BUYER-RECORD

                   CLOSE BUYER-FILE

                   move 1 to ws-done
               end-if

               if key-code-1 = 3           *> F3
                   initialize buyer-record
                   move WS-buyer-ID to buyer-id
               end-if

               if key-code-1 = 5 and is-admin          *> F5
                   open i-o buyer-file

                   delete buyer-file record

                   CLOSE buyer-file
                   move 1 to ws-done

               end-if

           end-perform.

       end program AmendBuyer.