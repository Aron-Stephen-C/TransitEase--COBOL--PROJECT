       IDENTIFICATION DIVISION.
       PROGRAM-ID. booking_engine.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-BOOKING-FILE ASSIGN TO 'data/booking.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-BOOKING-ID
               FILE STATUS IS WS-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
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
       01  WS-STATUS    PIC XX.
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

       LINKAGE SECTION.
       
       PROCEDURE DIVISION.
           PERFORM CHECK-FILE-STATUS
           PERFORM GENERATE-ID-SEQUENCE
           DISPLAY WS-GENERATED-ID
           PERFORM GENERATE-TIME-STAMP
           DISPLAY WS-TIME-STAMP
           STOP RUN.

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
           IF WS-STATUS NOT = '00' THEN
               OPEN OUTPUT FS-BOOKING-FILE
               IF WS-STATUS NOT = '00' THEN
                   DISPLAY 'Error : <Unable to oepn file>'
               END-IF
           END-IF
           CLOSE FS-BOOKING-FILE
           .
