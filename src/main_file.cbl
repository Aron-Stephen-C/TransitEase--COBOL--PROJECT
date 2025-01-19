       IDENTIFICATION DIVISION.
       PROGRAM-ID. main_file.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE    PIC X.
       01  WS-CHOICE-P    PIC X.
       01  WS-CHOICE-A    PIC X.
       01  WS-BUFFER    PIC X.

       01  WS-USER-RECORD.
           02  WS-FIRST-NAME    PIC X(50).
           02  WS-LAST-NAME    PIC X(50).
           02  WS-EMAIL    PIC X(100).
           02  WS-PASSWORD    PIC X(100).
           02  WS-PHONE-NUMBER    PIC X(11).
           02  WS-ROLE    PIC X.


       PROCEDURE DIVISION.
      *    PERFORM SAMPLE-TERMINAL
           MOVE 'Aron Stephen' TO WS-FIRST-NAME
           MOVE 'Cordova' TO WS-LAST-NAME
           MOVE 'aronstephenscordova@gmail.com' TO WS-EMAIL
           MOVE 'aron1234' TO WS-PASSWORD
           MOVE '09617036455' TO WS-PHONE-NUMBER
           MOVE 'A' TO WS-ROLE


           CALL 'backend/user_profile_management' USING WS-USER-RECORD
           STOP RUN.

      *AUTHENTICATE.
      *
      *    .

       SAMPLE-TERMINAL.
           PERFORM UNTIL WS-CHOICE = '3'
               PERFORM CLEAR
               DISPLAY 'T R A N S I T  E A S E'
               DISPLAY ' '
               DISPLAY '(1) - PASSENGER'
               DISPLAY '(2) - ADMIN'
               DISPLAY '(3) - EXIT'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN '1'
                       PERFORM PASSENGER-PAGE
                   WHEN '2'
                       PERFORM ADMIN-PAGE
                   WHEN '3'
                       PERFORM CLEAR
                       DISPLAY '[PROGRAM TERMINATED]'

                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Error : Invalid Input'
               END-EVALUATE

           END-PERFORM
           .

       PASSENGER-PAGE.
           PERFORM UNTIL WS-CHOICE-P = '3'
               PERFORM CLEAR
               DISPLAY 'T R A N S I T  E A S E - PASSENGER'
               DISPLAY ' '
               DISPLAY '(1) - LOG IN'
               DISPLAY '(2) - SIGN UP'
               DISPLAY '(3) - BACK TO MAIN PAGE'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-CHOICE-P

               EVALUATE WS-CHOICE-P
                   WHEN '1'
                       ACCEPT WS-BUFFER
                   WHEN '2'
                       ACCEPT WS-BUFFER
                   WHEN '3'
                       DISPLAY ' '
                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Error : Invalid Input'
               END-EVALUATE
           END-PERFORM.



       ADMIN-PAGE.
           PERFORM UNTIL WS-CHOICE-A = '3'
               PERFORM CLEAR
               DISPLAY 'T R A N S I T  E A S E - ADMIN'
               DISPLAY ' '
               DISPLAY '(1) - LOG IN'
               DISPLAY '(2) - SIGN UP'
               DISPLAY '(3) - BACK TO MAIN PAGE'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-CHOICE-A

               EVALUATE WS-CHOICE-A
                   WHEN '1'
                       ACCEPT WS-BUFFER
                   WHEN '2'
                       ACCEPT WS-BUFFER
                   WHEN '3'
                       DISPLAY ' '
                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Error : Invalid Input'
               END-EVALUATE
           END-PERFORM.
       
       CLEAR.
           CALL 'SYSTEM' USING 'CLEAR'.
