       IDENTIFICATION DIVISION.
       PROGRAM-ID. booking_engine.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FS-BOOKING-FILE ASSIGN TO 'data/booking.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-BOOKING-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-CURRENT-USER-FILE ASSIGN 
               TO 'data/artifact/current_user.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-PASSENGER-FILE ASSIGN TO 'data/passenger_file.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-P-USER-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-SCHEDULES-FILE ASSIGN TO 'data/schedules.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-SCHEDULE-ID
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT FS-VEHICLES-FILE ASSIGN TO 'data/vehicles.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-VEHICLE-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-ROUTES-FILE ASSIGN TO 'data/routes.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-ROUTE-ID
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FS-CURRENT-USER-FILE.
       01  FS-CURRENT-USER    PIC X(15).

       FD  FS-PASSENGER-FILE.
       01  FS-PASSENGER-RECORD.
           02    FS-P-USER-ID    PIC X(15).
           02    FS-P-FIRST-NAME    PIC X(50).
           02    FS-P-LAST-NAME    PIC X(50).
           02    FS-P-EMAIL    PIC X(100).
           02    FS-P-PASSWORD    PIC X(64).
           02    FS-P-PHONE-NUMBER    PIC X(11).
           02    FS-P-ROLE    PIC X.
           02    FS-P-TIME-STAMP.
               03    FS-P-DATE    PIC 99/99/99.
               03    FS-P-FILLER-SPACE    PIC X(3).
               03    FS-P-TIME.
                   04    FS-P-HOUR    PIC 99.
                   04    FS-P-COLON-1    PIC X.
                   04    FS-P-MINUTES    PIC 99.
                   04    FS-P-COLON-2    PIC X.
                   04    FS-P-SECOND    PIC 99.

       FD  FS-SCHEDULES-FILE.
       01  FS-SCHEDULES-RECORD.
           02    FS-SCHEDULE-ID    PIC X(15).
           02    FS-FK-ROUTE-ID    PIC X(15).
           02    FS-FK-VEHICLE-ID    PIC X(15).
           02    FS-S-DEPARTURE-TIME.
               03    FS-S-D-DATE    PIC 99/99/99.
               03    FS-S-D-FILLER-SPACE-1    PIC X(3).
               03    FS-S-D-TIME.
                   04    FS-S-D-HOUR    PIC 99.
                   04    FS-S-D-COLON-1    PIC X.
                   04    FS-S-D-MINUTES    PIC 99.
               03    FS-S-D-FILLER-SPACE-2    PIC X(3).
               03    FS-S-D-TIME-FORMAT    PIC XX.
           02    FS-S-ARRIVAL-TIME.
               03    FS-S-A-DATE    PIC 99/99/99.
               03    FS-S-A-FILLER-SPACE-1    PIC X(3).
               03    FS-S-A-TIME.
                   04    FS-S-A-HOUR    PIC 99.
                   04    FS-S-A-COLON-1    PIC X.
                   04    FS-S-A-MINUTES    PIC 99.
               03    FS-S-A-FILLER-SPACE-2    PIC X(3).
               03    FS-S-A-TIME-FORMAT    PIC XX.
           02    FS-S-STATUS    PIC X(5).
           02    FS-S-TIME-STAMP.
               03    FS-S-DATE    PIC 99/99/99.
               03    FS-S-FILLER-SPACE    PIC X(3).
               03    FS-S-TIME.
                   04    FS-S-HOUR    PIC 99.
                   04    FS-S-COLON-1    PIC X.
                   04    FS-S-MINUTES    PIC 99.
                   04    FS-S-COLON-2    PIC X.
                   04    FS-S-SECOND    PIC 99.

       FD  FS-ROUTES-FILE.
       01  FS-ROUTES-RECORD.
           02    FS-ROUTE-ID    PIC X(15).
           02    FS-ROUTE-ORIGIN    PIC X(30).
           02    FS-ROUTE-DESTINATION    PIC X(30).
           02    FS-ROUTE-DISTANCE    PIC 9(10)V9(2).
           02    FS-ROUTE-BASE-PRICE    PIC 9(10)V9(2).
           02    FS-ROUTE-TIME-STAMP.
               03    FS-R-DATE    PIC 99/99/99.
               03    FS-R-FILLER-SPACE    PIC X(3).
               03    FS-R-TIME.
                   04    FS-R-HOUR    PIC 99.
                   04    FS-R-COLON-1    PIC X.
                   04    FS-R-MINUTES    PIC 99.
                   04    FS-R-COLON-2    PIC X.
                   04    FS-R-SECOND    PIC 99.
           
       FD  FS-VEHICLES-FILE.
       01  FS-VEHICLES-RECORD.
           02    FS-VEHICLE-ID    PIC X(15).
           02    FS-VEHICLE-SERIAL    PIC X(6).
           02    FS-VEHICLE-CLASS    PIC X.
           02    FS-VEHICLE-CAPACITY    PIC 9(3).
           02    FS-VEHICLE-LICENSE-PLATE    PIC X(20).
           02    FS-VEHICLE-PRICE-FACTOR    PIC 9(10)V9(2).
           02    FS-VEHICLE-TIME-STAMP.
               03    FS-V-DATE    PIC 99/99/99.
               03    FS-V-FILLER-SPACE    PIC X(3).
               03    FS-V-TIME.
                   04    FS-V-HOUR    PIC 99.
                   04    FS-V-COLON-1    PIC X.
                   04    FS-V-MINUTES    PIC 99.
                   04    FS-V-COLON-2    PIC X.
                   04    FS-V-SECOND    PIC 99.

       FD  FS-BOOKING-FILE.
       01  FS-BOOKING-RECORD.
           02    FS-BOOKING-ID    PIC X(15).
           02    FS-FK-USER-ID    PIC X(15).
           02    FS-FK-SCHEDULE-ID    PIC X(15).
           02    FS-SEAT-NUMBER    PIC 9(10).
           02    FS-BOOKING-STATUS    PIC X.
           02    FS-PRICE    PIC 9(9)V99.
           02    FS-TIME-STAMP.
               03    FS-TS-DATE    PIC 99/99/99.
               03    FS-TS-FILLER-SPACE    PIC X(3).
               03    FS-TS-TIME.
                   04    FS-TS-HOUR    PIC 99.
                   04    FS-TS-FILLER-COLON-1    PIC X.
                   04    FS-TS-MINUTES    PIC 99.
                   04    FS-TS-FILLER-COLON-2    PIC X.
                   04    FS-TS-SECONDS    PIC 99.
       
       WORKING-STORAGE SECTION.
       01  WS-DATE     PIC 9(6).
       01  WS-TIME     PIC 9(8).
       01  WS-EOF    PIC X.
       01  WS-BOOKING-RECORD.
           02    WS-BOOKING-ID    PIC X(15).
           02    WS-FK-USER-ID    PIC X(15).
           02    WS-FK-SCHEDULE-ID    PIC X(15).
           02    WS-SEAT-NUMBER    PIC 9(10).
           02    WS-BOOKING-STATUS    PIC X.
           02    WS-PRICE    PIC 9(9)V99.
           02    WS-BOOKING-TIME-STAMP.
               03    WS-B-TS-DATE    PIC 99/99/99.
               03    WS-B-TS-FILLER-SPACE    PIC X(3).
               03    WS-B-TS-TIME.
                   04    WS-B-TS-HOUR    PIC 99.
                   04    WS-B-TS-FILLER-COLON-1    PIC X.
                   04    WS-B-TS-MINUTES    PIC 99.
                   04    WS-B-TS-FILLER-COLON-2    PIC X.
                   04    WS-B-TS-SECONDS    PIC 99.
       01  WS-FILE-STATUS    PIC XX.
       01  WS-GENERATED-ID.
           02    WS-GSI-DATE    PIC X(6).
           02    WS-GSI-TIME    PIC X(6).
           02    WS-GSI-INCREMENT-VALUE    PIC 9(3).
       01  WS-LAST-GENERATED-ID.
           02    WS-LSI-DATE    PIC X(6).
           02    WS-LSI-TIME    PIC X(6).
           02    WS-LSI-INCREMENT-VALUE    PIC 9(3).
       01  WS-INCREMENT-VALUE    PIC 9(3).
       01  WS-TIME-STAMP.
           02    WS-TS-DATE PIC 99/99/99.
           02    WS-B-TS-FILLER-SPACE    PIC X(3) VALUE SPACES.
           02    WS-TS-TIME.
               03    WS-TS-HOUR    PIC 99.
               03    WS-TS-FILLER-COLON-1    PIC X VALUE ':'.
               03    WS-TS-MINUTES    PIC 99.
               03    WS-TS-FILLER-COLON-2    PIC X VALUE ':'.
               03    WS-TS-SECONDS    PIC 99.
       01  WS-PASSENGER-PAGE-CHOICE    PIC X.
       01  WS-BUFFER    PIC X.

       LINKAGE SECTION.
       
       PROCEDURE DIVISION.

           PERFORM FETCH-USER

           PERFORM USER-MAIN-PAGE

           GOBACK
           STOP RUN.

       FETCH-USER.
           OPEN INPUT FS-CURRENT-USER-FILE
               READ FS-CURRENT-USER-FILE INTO FS-CURRENT-USER
               END-READ
               MOVE FS-CURRENT-USER TO FS-P-USER-ID
           CLOSE FS-CURRENT-USER-FILE
            OPEN I-O FS-PASSENGER-FILE
               READ FS-PASSENGER-FILE
                   KEY IS FS-P-USER-ID
               END-READ
           CLOSE FS-PASSENGER-FILE

           .

       USER-MAIN-PAGE.
           PERFORM UNTIL WS-PASSENGER-PAGE-CHOICE = ''

               PERFORM CLEAR
               DISPLAY 'Welcome - ' FS-P-LAST-NAME ', ' FS-P-FIRST-NAME
               DISPLAY ' '
               DISPLAY '1 - Add Booking'
               DISPLAY '2 - Cancel Booking'
               DISPLAY '3 - Back'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-PASSENGER-PAGE-CHOICE

               EVALUATE WS-PASSENGER-PAGE-CHOICE
                   WHEN '1'

                   WHEN '2'

                   WHEN '3'
                       CONTINUE
                   WHEN OTHER
                       DISPLAY ' '
                       DISPLAY 'Invalid Input'
                       ACCEPT WS-BUFFER
               END-EVALUATE

           END-PERFORM
           .

       TRAVERSAL-BOOKING.
           DISPLAY ' M Y  B O O K I N G S'
           OPEN I-O FS-BOOKING-FILE
               
           CLOSE FS-BOOKING-FILE
           .

       RECORD-BOOKING.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUES TO FS-BOOKING-ID

           OPEN I-O FS-BOOKING-FILE
               START FS-BOOKING-FILE KEY IS GREATER THAN FS-BOOKING-ID
               READ FS-BOOKING-FILE NEXT RECORD
                   AT END MOVE 1 TO WS-INCREMENT-VALUE
                   NOT AT END
                       PERFORM UNTIL WS-EOF = 'Y'
                           MOVE FS-BOOKING-ID TO WS-LAST-GENERATED-ID
                           READ FS-BOOKING-FILE NEXT RECORD
                               AT END MOVE 'Y' TO WS-EOF
                               NOT AT END
                                   CONTINUE
                           END-READ
                       END-PERFORM
               END-READ

           IF WS-LAST-GENERATED-ID NOT = SPACES THEN
               MOVE WS-LSI-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE    
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF

           PERFORM GENERATE-ID-SEQUENCE

           MOVE WS-GENERATED-ID TO WS-BOOKING-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-TIME-STAMP TO WS-BOOKING-TIME-STAMP

           MOVE WS-BOOKING-RECORD TO FS-BOOKING-RECORD

           WRITE FS-BOOKING-RECORD
           END-WRITE
               
           CLOSE FS-BOOKING-FILE
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
       
       CHECK-FILE-STATUS.
           OPEN I-O FS-BOOKING-FILE
           IF WS-FILE-STATUS NOT = '00' THEN
               OPEN OUTPUT FS-BOOKING-FILE
               IF WS-FILE-STATUS NOT = '00' THEN
                   DISPLAY 'Error : <Unable to oepn file>'
               END-IF
           END-IF
           CLOSE FS-BOOKING-FILE
           .

       CLEAR.
           CALL 'SYSTEM' USING 'clear'.
