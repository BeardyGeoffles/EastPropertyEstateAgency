      *AMENDVIEWING. cbl
      *Handle screen and function keys for amending a viewing
      *Must be passed a valid Viewing ID to amend
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. AmendViewing.

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

       01 ws-buyer-valid-message pic x(40) value spaces.
       01 ws-user-valid-message pic x(40) value spaces.
       01 ws-property-valid-message pic x(40) value spaces.
       01 ws-valid-buyer pic 9 value 0 comp.
       01 ws-valid-property pic 9 value 0 comp.
       01 ws-valid-user pic 9 value 0 comp.

       linkage section.

       01 ws-VIEWING-id pic 9999 value 0.

       SCREEN SECTION.

       copy "ViewingDetailsScreen.cpy".

      *  The following 2 lines are not part of the copybook

         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 41 "F5 - DELETE   ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 41 FOREGROUND-COLOR 14 "F5".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".

       procedure division using ws-VIEWING-id.

       SETUP.
           display "USER-IS-ADMIN" upon environment-name
           accept ws-user-has-admin from environment-value.

       MAIN.
           move ws-VIEWING-id to VIEWING-id of VIEWING-RECORD

           open i-o VIEWING-file

           read VIEWING-file

           close VIEWING-file.

       PARA-100-AMEND-VIEWING-SCREEN.

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

                   REWRITE VIEWING-RECORD

                   CLOSE VIEWING-FILE

                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize VIEWING-record
                   move WS-VIEWING-ID to VIEWING-id
               end-if

               if key-code-1 = 5
                   open i-o VIEWING-file

                   delete VIEWING-file record

                   CLOSE VIEWING-file
                   move 1 to ws-done

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

           end-perform.

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

       end program AmendViewing.