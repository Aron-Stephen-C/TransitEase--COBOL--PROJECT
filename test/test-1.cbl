      * This is the module that Manages user records
       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-1.
       

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-PASSENGER-FILE ASSIGN TO 'data/passenger_file.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-P-USER-ID
           FILE STATUS IS WS-FILE-STATUS.
       
           SELECT FS-ADMIN-FILE ASSIGN TO 'data/admin_file.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-A-USER-ID
           FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FS-PASSENGER-FILE.
       01  FS-PASSENGER-RECORD.
           02    FS-P-USER-ID    PIC X(15).
           02    FS-P-FIRST-NAME    PIC X(50).
           02    FS-P-LAST-NAME    PIC X(50).
           02    FS-P-EMAIL    PIC X(100).
           02    FS-P-PASSWORD    PIC X(255).
           02    FS-P-PHONE-NUMBER    PIC X(11).
           02    FS-P-ROLE    PIC X(9).
           02    FS-P-TIME-STAMP.
               03    FS-P-DATE    PIC 99/99/99.
               03    FS-P-FILLER-SPACE    PIC X(3).
               03    FS-P-TIME    PIC 99/99/99.
       FD  FS-ADMIN-FILE.
       01  FS-ADMIN-RECORD.
           02    FS-A-USER-ID    PIC X(15).
           02    FS-A-FIRST-NAME    PIC X(50).
           02    FS-A-LAST-NAME    PIC X(50).
           02    FS-A-EMAIL    PIC X(100).
           02    FS-A-PASSWORD    PIC X(255).
           02    FS-A-PHONE-NUMBER    PIC X(11).
           02    FS-A-ROLE    PIC X(9).
           02    FS-A-TIME-STAMP.
               03    FS-A-DATE    PIC 99/99/99.
               03    FS-A-FILLER-SPACE    PIC X(3).
               03    FS-A-TIME    PIC 99/99/99.
      
       WORKING-STORAGE SECTION.
       01  WS-DATE    PIC 9(6).
       01  WS-TIME    PIC 9(8).
       01  WS-FILE-STATUS    PIC XX.
       01  WS-GENERATED-USER-ID.
           02    WS-GSI-DATE    PIC 9(6).
           02    WS-GSI-TIME    PIC 9(6).
           02    WS-GSI-INCREMENT-VALUE    PIC 9(3).
       01  WS-EOF    PIC X.
       01  WS-LAST-GENERATED-ID.
           02    WS-LGSI-DATE    PIC 9(6).
           02    WS-LGSI-TIME    PIC 9(6).
           02    WS-L-INCREMENT-VALUE    PIC 9(3).
       01  WS-INCREMENT-VALUE PIC 9(3).
      * Temporary use
       01  WS-USER-RECORD.
           02    WS-USER-ID    PIC X(15).
           02    WS-FIRST-NAME    PIC X(50).
           02    WS-LAST-NAME    PIC X(50).
           02    WS-EMAIL    PIC X(100).
           02    WS-PASSWORD    PIC X(255).
           02    WS-PHONE-NUMBER    PIC X(11).
           02    WS-ROLE    PIC X(5).
           02    WS-TIME-STAMP.
               03    WS-TS-DATE    PIC 99/99/99.
               03    WS-TS-FILLER-SPACE    PIC X(3) VALUE SPACES.
               03    WS-TS-TIME    PIC 99/99/99.

       LINKAGE SECTION.


       PROCEDURE DIVISION.
           MOVE 'ARON' TO WS-FIRST-NAME
           MOVE 'CORDOVA' TO WS-LAST-NAME
           MOVE 'aronstephenscordova@gmail.com' TO WS-EMAIL
           MOVE 'AronPogi' TO WS-PASSWORD
           MOVE '09617036455'TO WS-PHONE-NUMBER
           MOVE 'Passenger' TO WS-ROLE
       
           PERFORM CHECK-FILE-STATUS
           PERFORM RECORD-PASSENGER

           STOP RUN.

       RECORD-ADMIN.
      *    Fetch Last Generated ID (Para sa incremententation)
           MOVE SPACES TO WS-EOF
           .

       RECORD-PASSENGER.
      *    Fetch Last Generated ID (Para sa incremententation)
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           OPEN I-O FS-PASSENGER-FILE

           START FS-PASSENGER-FILE KEY IS GREATER THAN FS-P-USER-ID

           READ FS-PASSENGER-FILE NEXT RECORD
               AT END MOVE 1 TO WS-INCREMENT-VALUE    
               NOT AT END 
                   PERFORM UNTIL WS-EOF = 'Y'
                       MOVE FS-P-USER-ID TO WS-LAST-GENERATED-ID
                       READ FS-PASSENGER-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           NOT AT END
                               CONTINUE
                       END-READ
                   END-PERFORM
           END-READ

           IF WS-LAST-GENERATED-ID NOT EQUAL TO SPACES
               MOVE WS-L-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE 
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF
           
           PERFORM GENERATE-ID-SEQUENCE

           MOVE WS-GENERATED-USER-ID TO FS-P-USER-ID

           DISPLAY WS-GENERATED-USER-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-USER-RECORD TO FS-PASSENGER-RECORD

           WRITE FS-PASSENGER-RECORD
           END-WRITE

           CLOSE FS-PASSENGER-FILE
           .

       CHECK-FILE-STATUS.
           OPEN I-O FS-PASSENGER-FILE
           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT FS-PASSENGER-FILE
               IF WS-FILE-STATUS NOT = '00'
                   DISPLAY 'Error : <Unable Create a File>'
               END-IF
           END-IF
           CLOSE FS-PASSENGER-FILE
           MOVE SPACES TO WS-FILE-STATUS
           OPEN I-O FS-ADMIN-FILE

           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT FS-ADMIN-FILE
               IF WS-FILE-STATUS NOT = '00'
                   DISPLAY 'Error : <Unable Create a File>'
               END-IF
           END-IF
           CLOSE FS-ADMIN-FILE
           .
           

       GENERATE-ID-SEQUENCE.
           ACCEPT WS-GSI-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME(1:6) TO WS-GSI-TIME
           MOVE WS-INCREMENT-VALUE TO WS-GSI-INCREMENT-VALUE
           .

       GENERATE-TIME-STAMP.
           ACCEPT WS-TS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME(1:6) TO WS-TS-TIME
           .

           