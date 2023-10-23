      *AMENDUSER.cbl
      *Handle screen and function keys for amending a user
      *Must be passed a valid User ID to amend
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. AmendUser.

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

       01 ws-password-fields.
         03 ws-password-1 pic x(10) value spaces.
         03 ws-password-2 pic x(10) value spaces.
         03 ws-secure-password pic x(10) value spaces.

       01 ws-message pic x(40) value spaces.
       01 ws-color pic 99 value 15.

       linkage section.

       01 ws-user-id pic 9999 value 0.

       SCREEN SECTION.

       copy "UserDetailsScreen.cpy".



      *  The following 2 lines are not part of the copybook

         03 line 24 col 2 "F1 - SAVE RECORD   F3 - BLANK FIELDS".
         03 line 24 col 41 "F5 - DELETE   ESC - EXIT WITHOUT SAVING".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 21 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 41 FOREGROUND-COLOR 14 "F5".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".

       procedure division using ws-user-id.

       SETUP.
           display "USER-IS-ADMIN" upon environment-name
           accept ws-user-has-admin from environment-value.

       MAIN.
           move ws-user-id to user-id of user-RECORD

           open i-o user-file

           read user-file

           close user-file.

       PARA-100-AMEND-USER-SCREEN.

           initialize ws-password-fields
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
                       move spaces to ws-message
                   else
                       move "Passwords do not match. Please re-enter. "
                         to
                         ws-message
                       initialize ws-password-fields
                   end-if
               else
                   if ws-secure-password equal spaces
                       move "Password cannot be blank." to ws-message
                   end-if
               end-if

               if key-code-1 = 0
                   move 1 to ws-done
               end-if

               if key-code-1 = 1 and ws-secure-password not equal spaces
                   move ws-secure-password to USER-PASSWORD

                   OPEN i-O user-file

                   REWRITE user-RECORD

                   CLOSE user-FILE

                   move 1 to ws-done
               end-if

               if key-code-1 = 3
                   initialize user-record
                   initialize ws-password-fields
                   move spaces to ws-message
                   move WS-user-ID to user-id
               end-if

               if key-code-1 = 5
                   open i-o user-file

                   delete user-file record

                   CLOSE user-file
                   move 1 to ws-done

               end-if

           end-perform.

       end program AmendUser.