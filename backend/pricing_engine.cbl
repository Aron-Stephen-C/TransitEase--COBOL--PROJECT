       IDENTIFICATION DIVISION.
       PROGRAM-ID. pricing_engine.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-PAYMENT-FILE ASSIGN TO 'data/payments.txt'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-PAYMENT-ID
           FILE STATUS IS WS-STATUS.
           
       DATA DIVISION.
       FILE SECTION.
       FD  FS-PAYMENT-FILE.
       01  FS-PAYMENT-RECORD.
           02    FS-PAYMENT-ID    PIC X(15).
           02    FS-FK-BOOKING-ID    PIC X(15).
           02    FS-PAYMENT-METHOD    PIC X(4).
           02    FS-PAYMENT-AMOUNT    PIC 9(10)V99.
           02    FS-PAYMENT-STATUS    PIC X.
           02    FS-TRANSACTION-TIME-STAMP.
               03    FS-TT-DATE    PIC 99/99/99.
               03    FS-TT-FILLER-SPACE    PIC X(3).
               03    FS-TT-TIME.
                   04    FS-TT-HOUR    PIC 99.
                   04    FS-TT-FILLER-COLON-1    PIC X.
                   04    FS-TT-MINUTES    PIC 99.
                   04    FS-TT-FILLER-COLON-2    PIC X.
                   04    FS-TT-SECONDS    PIC 99.

       WORKING-STORAGE SECTION.
       01  WS-STATUS    PIC XX.
       01  WS-DATE     PIC 9(6).
       01  WS-TIME     PIC 9(8).
       01  WS-EOF    PIC X.
       01  WS-INCREMENT-VALUE    PIC 9(3).
       01  WS-PAYMENT-RECORD.
           02    WS-PAYMENT-ID    PIC X(15).
           02    WS-FK-BOOKING-ID    PIC X(15).
           02    WS-PAYMENT-METHOD    PIC X(4).
           02    WS-PAYMENT-AMOUNT    PIC 9(10)V99.
           02    WS-PAYMENT-STATUS    PIC X.
           02    WS-TRANSACTION-TIME-STAMP.
               03    WS-TT-DATE    PIC 99/99/99.
               03    WS-TT-FILLER-SPACE    PIC X(3).
               03    WS-TT-TIME.
                   04    WS-TT-HOUR    PIC 99.
                   04    WS-TT-FILLER-COLON-1    PIC X.
                   04    WS-TT-MINUTES    PIC 99.
                   04    WS-TT-FILLER-COLON-2    PIC X.
                   04    WS-TT-SECONDS    PIC 99.
       01  WS-TIME-STAMP.
           02    WS-TS-DATE    PIC 99/99/99.
           02    WS-TS-FILLER-SPACE    PIC X(3) VALUE SPACES.
           02    WS-TS-TIME.
               03    WS-TS-HOUR    PIC 99.
               03    WS-TS-FILLER-COLON-1 PIC X VALUE ':'.
               03    WS-TS-MINUTES    PIC 99.
               03    WS-TS-FILLER-COLON-2 PIC X VALUE ':'.
               03    WS-TS-SECONDS    PIC 99.
       01  WS-GENERATED-ID.
           02    WS-GSI-DATE    PIC 9(6).
           02    WS-GSI-TIME    PIC 9(6).
           02    WS-GSI-INCREMENT-VALUE    PIC 9(3).
       01  WS-LAST-GENERATED-ID.
           02    WS-LSI-DATE    PIC 9(6).
           02    WS-LSI-TIME    PIC 9(6).
           02    WS-LSI-INCREMENT-VALUE    PIC 9(3).

       LINKAGE SECTION.
       
       PROCEDURE DIVISION.
           PERFORM CHECK-FILE-STATUS

           STOP RUN.

       RECORD-PAYMENT.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUE TO FS-PAYMENT-ID
           OPEN I-O FS-PAYMENT-FILE
               START FS-PAYMENT-FILE KEY IS GREATER THAN FS-PAYMENT-ID
               READ FS-PAYMENT-FILE NEXT RECORD
                   AT END MOVE 1 TO WS-INCREMENT-VALUE
                   NOT AT END
                       MOVE FS-PAYMENT-ID TO WS-LAST-GENERATED-ID
                       PERFORM UNTIL WS-EOF = 'Y'
                       READ FS-PAYMENT-FILE NEXT RECORD
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

               MOVE WS-GENERATED-ID TO WS-PAYMENT-ID

               PERFORM GENERATE-TIME-STAMP

               MOVE WS-TIME-STAMP TO WS-TRANSACTION-TIME-STAMP

               MOVE WS-PAYMENT-RECORD TO FS-PAYMENT-RECORD

               WRITE FS-PAYMENT-RECORD
               END-WRITE
           CLOSE FS-PAYMENT-FILE
           .

       GENERATE-TIME-STAMP.
           ACCEPT WS-TS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME(1:6) TO WS-TS-TIME
           .
       
       GENERATE-ID-SEQUENCE.
           ACCEPT WS-GSI-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME(1:6) TO WS-GSI-TIME
           MOVE WS-INCREMENT-VALUE TO WS-GSI-INCREMENT-VALUE
           .

       CHECK-FILE-STATUS.
           OPEN I-O FS-PAYMENT-FILE
               IF WS-STATUS NOT = '00' THEN    
                   OPEN OUTPUT FS-PAYMENT-FILE
                   IF WS-STATUS NOT = '00' THEN    
                       DISPLAY 'Error : <Unable to open file>'
                   END-IF
               END-IF
           CLOSE FS-PAYMENT-FILE
           .
