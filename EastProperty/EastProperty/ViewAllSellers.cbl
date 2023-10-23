      *VIEWALLSELLERS.cbl
      *Handle paged display of all Seller records
      *
      *
      *Last updated 03/04/2023 Radio GAGO

       identification division.
       program-id. ViewAllSellers.
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


       01 ws-current-page pic 9999 value 1 comp.
       01 ws-total-records pic 9999 value 0 comp.
       01 ws-records-per-page pic 99 value 15 comp.
       01 ws-total-pages pic 9999 value 0 comp.
       01 ws-record-remainder pic 99 value 0 comp.

       01 file-array occurs 9999 times.
         03 seller-id pic 9999.
         03 seller-name pic x(20).
         03 seller-address1 pic x(25).
         03 seller-postcode pic x(7).

       01 ws-go-to-record pic 9999 value 0.
       01 ws-end-of-file pic 9 value 0 comp.
       01 ws-first-on-page pic 9999 value 1 comp.
       01 ws-index pic 9999 value 1 comp.
       01 ws-display-line pic 99 value 3 comp.

       SCREEN SECTION.

       01 DISPLAY-SCREEN AUTO.
         03 BLANK SCREEN.
         03 LINE 1 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 2 COL 29 foreground-color 14 VALUE
            "|  EAST VIEW ALL SELLERS     |".
         03 LINE 3 COL 29 foreground-color 14 VALUE
            "------------------------------".
         03 LINE 5 COL 2 FOREGROUND-COLOR 15 VALUE "ID".
         03 LINE 5 COL 7 FOREGROUND-COLOR 15 VALUE "NAME".
         03 LINE 5 COL 28 FOREGROUND-COLOR 15 VALUE "ADDRESS".
         03 LINE 5 COL 54 FOREGROUND-COLOR 15 VALUE "POSTCODE".
         03 line 23 col 2 FOREGROUND-COLOR 15 "Go to Record: [".
         03 pic z(4) using ws-go-to-record foreground-color 10 highlight
            prompt " ".
         03 FOREGROUND-COLOR 15 value "]".
         03 line 2 col 2 FOREGROUND-COLOR 15 "Page ".
         03 pic ZZZ9 from ws-current-page.
         03 value " of ".
         03 pic ZZZ9 from ws-total-pages.
         03 line 22 col 1 foreground-color 14 value
            "----------------------------------------".
         03 line 22 col 41 foreground-color 14 value
            "----------------------------------------".
         03 line 24 col 2 foreground-color 15
            "F1 - PREV PAGE   F3 - NEXT PAGE".
         03 line 24 col 55 foreground-color 15 "ESC - BACK TO MENU".
         03 line 24 col 2 FOREGROUND-COLOR 14 "F1".
         03 line 24 col 19 FOREGROUND-COLOR 14 "F3".
         03 line 24 col 55 FOREGROUND-COLOR 14 "ESC".

       procedure division.

       SETUP.

           perform Function-key-setup
           Perform BuildFileArray
           move 0 to ws-done.

       MAIN.

           perform until done

               move 6 to ws-display-line *> First row to display records
               compute ws-first-on-page equals ((ws-current-page - 1) *
                 ws-records-per-page) + 1
               divide ws-total-records by ws-records-per-page
                 giving ws-total-pages rounded remainder
                 ws-record-remainder

               if ws-record-remainder is greater than 0 and
                 ws-record-remainder is less than ws-records-per-page
                 / 2
                   add 1 to ws-total-pages
               end-if

               if ws-total-pages = 0
                   add 1 to ws-total-pages
               end-if

               display DISPLAY-SCREEN

               perform varying ws-index from ws-first-on-page by 1 until
                 ws-index = ws-first-on-page + ws-records-per-page

                   if seller-id of file-array(ws-index) is numeric and
                     seller-id of file-array(ws-index) > 0
                     and ws-index <= ws-total-records
                       display seller-id of file-array(ws-index) at col
                         2
                         line ws-display-line
                       display seller-name of file-array(ws-index) at
                         col 7 line ws-display-line
                       display seller-address1 of file-array(ws-index)
                         at
                         col
                         28 line ws-display-line
                       display seller-postcode of file-array(ws-index)
                         at
                         col
                         54 line ws-display-line

                       add 1 to ws-display-line
                   end-if

               end-perform

               if ws-total-records = 0

                   display "No records to display." at col 28 line 11
                     foreground-colour 12

               end-if

               accept DISPLAY-SCREEN
               if key-code-1 = 0
                   move 1 to ws-done
                   move 0 to ws-go-to-record
               end-if
               if key-code-1 = 1
                   subtract 1 from ws-current-page
                   if ws-current-page equals 0
                       move ws-total-pages to ws-current-page
                   end-if
                   move 0 to ws-go-to-record
               end-if
               if key-code-1 = 3
                   add 1 to ws-current-page
                   if ws-current-page is greater than ws-total-pages
                       move 1 to ws-current-page
                   end-if
                   move 0 to ws-go-to-record
               end-if
               if ws-go-to-record not equals 0
                   open i-o seller-file
                   move ws-go-to-record to seller-id of seller-RECORD
                   read seller-file
                       invalid key
                           close seller-file
                           move 0 to ws-go-to-record
                       not invalid key
                           close seller-file
                           call "AmendSeller" using ws-go-to-record
                           perform BuildFileArray
                           move 0 to ws-go-to-record
               end-if
           end-perform.

           Goback.

       BuildFileArray.

           move 0 to ws-total-records.
           move 0 to ws-end-of-file.

           open input seller-file

           perform until ws-end-of-file equals 1
               read seller-file next record
                   at end
                       move 1 to ws-end-of-file

                   not at end
                       add 1 to ws-total-records
                       move seller-id of seller-record to
                         seller-id of file-array(
                           ws-total-records)
                       move seller-name of seller-record to
                         seller-name of file-array(
                           ws-total-records)
                       move seller-al1 of seller-record to
                         seller-address1
                         of file-array(ws-total-records)
                       move seller-postcode of seller-record to
                         seller-postcode
                         of file-array(ws-total-records)

           end-perform

           close seller-file.

       copy "FunctionKeySetup.cpy".



       end program ViewAllSellers.
