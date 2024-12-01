       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_practice1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RIGHT-TRIANGLE ASSIGN TO 'data/Information.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  RIGHT-TRIANGLE.
       01  FS-RIGHT-TRIANGLE    PIC X(15).

       WORKING-STORAGE SECTION.
       01  WS-COUNTER-I    PIC 9.
       01  WS-COUNTER-J    PIC 9.
       01  WS-RIGHT-TRIANGLE PIC X(15).
       01  WS-TEMP    PIC X(15).
       01  WS-BASE    PIC 9.
       
       PROCEDURE DIVISION.

           DISPLAY "Enter base : " WITH NO ADVANCING
           ACCEPT WS-BASE

           OPEN OUTPUT RIGHT-TRIANGLE
               PERFORM VARYING WS-COUNTER-I FROM 1 BY 1 UNTIL 
               WS-COUNTER-I > WS-BASE
                   MOVE SPACES TO WS-TEMP
                   MOVE SPACES TO WS-RIGHT-TRIANGLE
                   PERFORM VARYING WS-COUNTER-J FROM 1 BY 1 UNTIL 
                   WS-COUNTER-J > WS-COUNTER-I
                       STRING WS-RIGHT-TRIANGLE DELIMITED BY SPACE
                              "* " DELIMITED BY SIZE
                              INTO WS-TEMP
                       END-STRING

                       MOVE WS-TEMP TO WS-RIGHT-TRIANGLE
                   END-PERFORM
                   MOVE WS-RIGHT-TRIANGLE TO FS-RIGHT-TRIANGLE

                   WRITE FS-RIGHT-TRIANGLE
                   END-WRITE

                   DISPLAY " "
               END-PERFORM
           CLOSE RIGHT-TRIANGLE
           STOP RUN.
       