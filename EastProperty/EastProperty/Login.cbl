       identification division.
       program-id. Login is recursive.

*      SCREEN SECTION CREATED BY USING FOREGROUND-COLOR AND LAYERING
*      THE DIFFERENT COLOURS (THE ONE TO BE VIEWED NEEDS TO BE GENERATED
*      LAST IN THE CODE).
*      SO THERE THE WHITE BACKGROUND WITH BLUE ON TOP AND THEN THE
*      RED/WHITE NEEDLE ON TOP OF THOSE. LAYERED COLOURS NEED A DISPLAY
*      FIELD THAT TIGHTLY FITS THE TEXT FIELD

       environment division.

       input-output section.
       file-control.
       copy "userfile.cpy".




       configuration section.
       special-names.
                  crt status is key-status.

       data division.

       FILE SECTION.
       copy "userrecord.cpy".
 


       working-storage section.

       copy "ws-functionkeys.cpy".




       copy "ws-common.cpy".



       01 WS-USERNAME PIC X(10) value spaces.
       01 WS-PASSWORD PIC X(10) value spaces.
       01 WS-FOUND PIC X(1) VALUE 'N'.
       01 EOF-FLAG PIC X(1) VALUE "N".
       01 LOGIN-FAIL-MESSAGE PIC X(43).
       

       SCREEN SECTION.

       01 TITLE-SCREEN background-color 0 foreground-color 15 AUTO 
       UPPER.
         03 BLANK SCREEN.
*      EAST
         03 line 6 col 44 foreground-color 14 value "@@@ @@@ @@@ @@@".
         03 line 7 col 44 foreground-color 14 value "@   @ @ @    @ ".
         03 line 8 col 44 foreground-color 14 value "@@  @@@ @@@  @ ".
         03 line 9 col 44 foreground-color 14 value "@   @ @   @  @ ".
         03 line 10 col 44 foreground-color 14 value "@@@ @ @ @@@  @ ".
*      PROPERTY
         03 line 12 col 36 foreground-color 14 value
            "@@@ @@@ @@@ @@@ @@@ @@@ @@@ @ @".
         03 line 13 col 36 foreground-color 14 value
            "@ @ @ @ @ @ @ @ @   @ @  @  @ @".
         03 line 14 col 36 foreground-color 14 value
            "@@@ @@  @ @ @@@ @@  @@   @   @ ".
         03 line 15 col 36 foreground-color 14 value
            "@   @ @ @ @ @   @   @ @  @   @ ".
         03 line 16 col 36 foreground-color 14 value
            "@   @ @ @@@ @   @@@ @ @  @   @ ".
*      COMPASS ART WHITE
         03 line 4 col 15 VALUE "        N        ".
         03 line 5 col 15 VALUE "     @@@@@@@".
         03 line 6 col 15 VALUE "   @@@@@@@@@@@   ".
         03 line 7 col 15 VALUE "  @@@@@@@@@@@@@  ".
         03 line 8 col 15 VALUE " @@@@@@@@@@@@@@@ ".
         03 line 9 col 15 VALUE " @@@@@@@@@@@@@@@ ".
         03 line 10 col 15 VALUE "@@@@@@@@@@@@@@@@@".
         03 line 11 col 13 VALUE "W @@@@@@@@@@@@@@@@@ E".
         03 line 12 col 15 VALUE "@@@@@@@@@@@@@@@@@".
         03 line 13 col 15 VALUE " @@@@@@@@@@@@@@@ ".
         03 line 14 col 15 VALUE " @@@@@@@@@@@@@@@ ".
         03 line 15 col 15 VALUE "  @@@@@@@@@@@@@  ".
         03 line 16 col 15 VALUE "   @@@@@@@@@@@   ".
         03 line 17 col 15 VALUE "     @@@@@@@     ".
         03 line 18 col 15 VALUE "        S        ".
*      COMPASS ART BLUE
         03 line 6 col 20 FOREGROUND-COLOUR 9 VALUE "@@@@@@@".
         03 line 7 col 18 FOREGROUND-COLOUR 9 VALUE "@@@@@@@@@@@".
         03 line 8 col 17 FOREGROUND-COLOUR 9 VALUE "@@@@@@@@@@@@@".
         03 line 9 col 17 FOREGROUND-COLOUR 9 VALUE "@@@@@@@@@@@@@".
         03 line 10 col 16 FOREGROUND-COLOUR 9 VALUE "@@@@7778888@@@@".
         03 line 11 col 16 FOREGROUND-COLOUR 9 VALUE "@7777778888888@".
         03 line 12 col 16 FOREGROUND-COLOUR 9 VALUE "@@@@7778888@@@@".
         03 line 13 col 17 FOREGROUND-COLOUR 9 VALUE "@@@@@@@@@@@@@".
         03 line 14 col 17 FOREGROUND-COLOUR 9 VALUE "@@@@@@@@@@@@@".
         03 line 15 col 18 FOREGROUND-COLOUR 9 VALUE "@@@@@@@@@@@".
         03 line 16 col 20 FOREGROUND-COLOUR 9 VALUE "@@@@@@@".
*      COMPASS ART NEEDLE
         03 line 10 col 20 FOREGROUND-COLOUR 15 VALUE "@@@".
         03 line 11 col 17 FOREGROUND-COLOUR 15 VALUE "@@@@@@@".
         03 line 12 col 20 FOREGROUND-COLOUR 15 VALUE "@@@".
         03 line 10 col 23 FOREGROUND-COLOUR 12 VALUE "@@@@".
         03 line 11 col 24 FOREGROUND-COLOUR 12 VALUE "@@@@@@".
         03 line 12 col 23 FOREGROUND-COLOUR 12 VALUE "@@@@".

         03 FOREGROUND-COLOR 15 LINE 21 COL 26 "USER NAME: [".
         03 FOREGROUND-COLOR 10 LINE 21 COL 38 PIC X(10) USING
            WS-USERNAME highlight prompt " ".
         03 FOREGROUND-COLOR 15 LINE 21 COL 48 "]".
         03 FOREGROUND-COLOR 15 LINE 23 COL 26 "PASSWORD : [".
         03 FOREGROUND-COLOR 10 LINE 23 COL 38 PIC X(10) USING
            WS-PASSWORD no-echo highlight prompt " ".
         03 FOREGROUND-COLOR 15 LINE 23 COL 48 "]".
         03 FOREGROUND-COLOR 4 LINE 25 COL 20 FROM LOGIN-FAIL-MESSAGE.


       procedure division.

       PARA-000-SETUP.

           perform Function-key-setup
           MOVE SPACES TO LOGIN-FAIL-MESSAGE.
           

       PARA-000-MAIN.

           
           initialize ws-password
           move 'N' to ws-user-has-admin.

           MOVE 'N' TO EOF-FLAG
           move 'N' TO WS-FOUND

           DISPLAY TITLE-SCREEN
           accept TITLE-SCREEN

           if key-code-1 = 0
               stop run
           end-if.

               PERFORM PROCESS-FILE
           IF WS-FOUND = 'Y'
               PERFORM MENU-SCREEN
           ELSE
               move "Login Failed. Invalid Username or Password."
                 to LOGIN-FAIL-MESSAGE
               PERFORM para-000-main
           END-IF
           STOP RUN.
           
       
       MENU-SCREEN.
 
           display "USER-IS-ADMIN" upon environment-name
           display ws-user-has-admin upon environment-value.

           CALL "MAIN".

       PROCESS-FILE.
           OPEN INPUT USER-FILE.
           PERFORM UNTIL WS-FOUND = 'Y' OR EOF-FLAG = 'Y'
               read USER-FILE NEXT RECORD
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF USER-NAME = WS-USERNAME
                         AND USER-PASSWORD = WS-PASSWORD
                           MOVE 'Y' TO WS-FOUND
                           if USER-HAS-ADMIN = 'Y'
                               move 'Y' to ws-user-has-admin
                           else
                               move 'N' to ws-user-has-admin
                           end-if
                           
                       END-IF
               END-READ
           END-PERFORM
           CLOSE USER-FILE.

       

       copy "FunctionKeySetup.cpy".



       end program Login.