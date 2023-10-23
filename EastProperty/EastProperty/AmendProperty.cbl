      *AMENDPROPERTY.cbl
      *Handle screen and function keys for amending a property
      *Must be passed a valid Property ID to amend
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. AmendProperty.

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

       01 ws-buyer-valid-message pic x(40) value spaces.
       01 ws-seller-valid-message pic x(40) value spaces.
       01 ws-valid-buyer pic 9 value 0 comp.
       01 ws-valid-seller pic 9 value 0 comp.

       linkage section.

       01 ws-property-id pic z(4) value 0.

       SCREEN SECTION.

       copy "PropertyDetailsScreen.cpy".

      *  The following 6 lines are not part of the copybook

         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 55 "ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".

       procedure division using ws-property-id.

       SETUP.
           display "USER-IS-ADMIN" upon environment-name
           accept ws-user-has-admin from environment-value.

       MAIN.
           move ws-property-id to property-id of property-RECORD

           open i-o property-file

           read property-file

           close property-file.

       PARA-100-AMEND-property-SCREEN.

           move 0 to ws-done

           perform until done

               perform PARA-440-VERIFY-SELLER
               perform PARA-450-VERIFY-BUYER

               DISPLAY property-DETAILS-SCREEN

               if is-admin
                   display "F5 - DELETE" at line 24 col 41
                   display "F5" at line 24 col 41 foreground-color 14
               end-if

               ACCEPT property-DETAILS-SCREEN

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 1
                   OPEN i-O property-file

                   REWRITE property-RECORD

                   CLOSE property-FILE

                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize property-record
                   move WS-property-ID to property-id
               end-if

               if key-code-1 = 5 and is-admin
                   open i-o property-file

                   delete property-file record

                   CLOSE property-file
                   move 1 to ws-done

               end-if

               if key-code-1 = 7 and ws-valid-seller = 1
                   Call "AmendSeller" using PROP-SELLER-ID
               end-if

               if key-code-1 = 9 and ws-valid-buyer = 1
                   Call "AmendBuyer" using PROP-BUYER-ID
               end-if

           end-perform.

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
                       string "[F7] ", SELLER-NAME of SELLER-RECORD into
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

       end program AmendProperty.