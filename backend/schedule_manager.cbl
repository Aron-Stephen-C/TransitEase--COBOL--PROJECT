      * This module manages Routes, Vehicles, Schedules records.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. schedule_manager.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT FS-ROUTES-FILE ASSIGN TO 'data/routes.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-ROUTE-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-VEHICLES-FILE ASSIGN TO 'data/vehicles.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-VEHICLE-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-SCHEDULES-FILE ASSIGN TO 'data/shedules.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-SCHEDULE-ID
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FS-ROUTES-FILE.
       01  FS-ROUTES-RECORD.
           02    FS-ROUTE-ID    PIC X(15).
           02    FS-ROUTE-ORIGIN    PIC X(100).
           02    FS-ROUTE-DESTINATION    PIC X(100).
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
           02    FS-VEHICLE-TYPE    PIC X.
           02    FS-VEHICLE-CAPACITY    PIC 9(3).
           02    FS-VEHICLE-LICENSE-PLATE    PIC X(20).
           02    FS-VEHICLE-PRICE-FACTOR    PIC 9(19)V9(2).
           02    FS-VEHICLE-TIME-STAMP.
               03    FS-V-DATE    PIC 99/99/99.
               03    FS-V-FILLER-SPACE    PIC X(3).
               03    FS-V-TIME.
                   04    FS-V-HOUR    PIC 99.
                   04    FS-V-COLON-1    PIC X.
                   04    FS-V-MINUTES    PIC 99.
                   04    FS-V-COLON-2    PIC X.
                   04    FS-V-SECOND    PIC 99.

       FD  FS-SCHEDULES-FILE.
       01  FS-SCHEDULES-RECORD.
           02    FS-SCHEDULE-ID    PIC X(15).
           02    FS-S-ROUTE-ID    PIC X(15).
           02    FS-S-VEHICLE-ID    PIC X(15).
           02    FS-S-DEPARTURE-TIME.
               03    FS-S-D-DATE    PIC 99/99/99.
               03    FS-S-D-FILLER-SPACE    PIC X(3).
               03    FS-S-D-TIME.
                   04    FS-S-D-HOUR    PIC 99.
                   04    FS-S-D-COLON-1    PIC X.
                   04    FS-S-D-MINUTES    PIC 99.
                   04    FS-S-D-COLON-2    PIC X.
                   04    FS-S-D-SECOND    PIC 99.
           02    FS-S-ARRIVAL-TIME.
               03    FS-S-A-DATE    PIC 99/99/99.
               03    FS-S-A-FILLER-SPACE    PIC X(3).
               03    FS-S-A-TIME.
                   04    FS-S-A-HOUR    PIC 99.
                   04    FS-S-A-COLON-1    PIC X.
                   04    FS-S-A-MINUTES    PIC 99.
                   04    FS-S-A-COLON-2    PIC X.
                   04    FS-S-A-SECOND    PIC 99.
           02    FS-S-STATUS    PIC X(3).
           02    FS-S-TIME-STAMP.
               03    FS-S-DATE    PIC 99/99/99.
               03    FS-S-FILLER-SPACE    PIC X(3).
               03    FS-S-TIME.
                   04    FS-S-HOUR    PIC 99.
                   04    FS-S-COLON-1    PIC X.
                   04    FS-S-MINUTES    PIC 99.
                   04    FS-S-COLON-2    PIC X.
                   04    FS-S-SECOND    PIC 99.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS    PIC XX.
       01  WS-DATE    PIC    9(6).
       01  WS-TIME    PIC    9(8).
       01  WS-INCREMENT-VALUE    PIC 9(3).
       01  WS-EOF    PIC X.
       01  WS-GENERATED-USER-ID.
           02    WS-GSI-DATE    PIC 9(6).
           02    WS-GSI-TIME    PIC 9(6).
           02    WS-GSI-INCREMENT-VALUE    PIC 9(3).
       01  WS-LAST-GENERATED-ID.
           02    WS-LGSI-DATE    PIC 9(6).
           02    WS-LGSI-TIME    PIC 9(6).
           02    WS-L-INCREMENT-VALUE    PIC 9(3).
       01  WS-TIME-STAMP.
           02    WS-TS-DATE    PIC 99/99/99.
           02    WS-TS-FILLER-SPACE    PIC X(3) VALUE SPACES.
           02    WS-TS-TIME.
               03    WS-TS-HOUR    PIC 99.
               03    WS-TS-COLON-1    PIC X VALUE ':'.
               03    WS-TS-MINUTES    PIC 99.
               03    WS-TS-COLON-2    PIC X VALUE ':'.
               03    WS-TS-SECOND    PIC 99.
       01  WS-ROUTES-RECORD.
           02    WS-ROUTE-ID    PIC X(15).
           02    WS-ROUTE-ORIGIN    PIC X(100).
           02    WS-ROUTE-DESTINATION    PIC X(100).
           02    WS-ROUTE-DISTANCE    PIC 9(10)V9(2).
           02    WS-ROUTE-BASE-PRICE    PIC 9(10)V9(2).
           02    WS-ROUTE-TIME-STAMP.
               03    WS-R-DATE    PIC 99/99/99.
               03    WS-R-FILLER-SPACE    PIC X(3).
               03    WS-R-TIME.
                   04    WS-R-HOUR    PIC 99.
                   04    WS-R-COLON-1    PIC X.
                   04    WS-R-MINUTES    PIC 99.
                   04    WS-R-COLON-2    PIC X.
                   04    WS-R-SECOND    PIC 99.
       01  WS-VEHICLES-RECORD.
           02    WS-VEHICLE-ID    PIC X(15).
           02    WS-VEHICLE-TYPE    PIC X.
           02    WS-VEHICLE-CAPACITY    PIC 9(3).
           02    WS-VEHICLE-LICENSE-PLATE    PIC X(20).
           02    WS-VEHICLE-PRICE-FACTOR    PIC 9(19)V9(2).
           02    WS-VEHICLE-TIME-STAMP.
               03    WS-V-DATE    PIC 99/99/99.
               03    WS-V-FILLER-SPACE    PIC X(3).
               03    WS-V-TIME.
                   04    WS-V-HOUR    PIC 99.
                   04    WS-V-COLON-1    PIC X.
                   04    WS-V-MINUTES    PIC 99.
                   04    WS-V-COLON-2    PIC X.
                   04    WS-V-SECOND    PIC 99.
       01  WS-SCHEDULES-RECORD.
           02    WS-SCHEDULE-ID    PIC X(15).
           02    WS-S-ROUTE-ID    PIC X(15).
           02    WS-S-VEHICLE-ID    PIC X(15).
           02    WS-S-DEPARTURE-TIME.
               03    WS-S-D-DATE    PIC 99/99/99.
               03    WS-S-D-FILLER-SPACE    PIC X(3).
               03    WS-S-D-TIME.
                   04    WS-S-D-HOUR    PIC 99.
                   04    WS-S-D-COLON-1    PIC X.
                   04    WS-S-D-MINUTES    PIC 99.
                   04    WS-S-D-COLON-2    PIC X.
                   04    WS-S-D-SECOND    PIC 99.
           02    WS-S-ARRIVAL-TIME.
               03    WS-S-A-DATE    PIC 99/99/99.
               03    WS-S-A-FILLER-SPACE    PIC X(3).
               03    WS-S-A-TIME.
                   04    WS-S-A-HOUR    PIC 99.
                   04    WS-S-A-COLON-1    PIC X.
                   04    WS-S-A-MINUTES    PIC 99.
                   04    WS-S-A-COLON-2    PIC X.
                   04    WS-S-A-SECOND    PIC 99.
           02    WS-S-STATUS    PIC X(3).
           02    WS-S-TIME-STAMP.
               03    WS-S-DATE    PIC 99/99/99.
               03    WS-S-FILLER-SPACE    PIC X(3).
               03    WS-S-TIME.
                   04    WS-S-HOUR    PIC 99.
                   04    WS-S-COLON-1    PIC X.
                   04    WS-S-MINUTES    PIC 99.
                   04    WS-S-COLON-2    PIC X.
                   04    WS-S-SECOND    PIC 99.

       LINKAGE SECTION.
       
       PROCEDURE DIVISION.
           PERFORM CHECK-FILE-STATUS

           STOP RUN.

       TRAVERSAL-VEHICLE-RECORD.
           MOVE SPACES TO WS-EOF
           OPEN INPUT FS-VEHICLES-FILE    
               READ FS-VEHICLES-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END 
               DISPLAY FS-VEHICLE-ID 
               DISPLAY FS-VEHICLE-TYPE
               DISPLAY FS-VEHICLE-CAPACITY
               DISPLAY FS-VEHICLE-LICENSE-PLATE
               DISPLAY FS-VEHICLE-PRICE-FACTOR
               DISPLAY FS-VEHICLE-TIME-STAMP
               END-READ
           CLOSE FS-VEHICLES-FILE
           .

       INITIALIZE-VEHICLE.
           MOVE 'BUS' TO WS-VEHICLE-TYPE
           MOVE 35 TO WS-VEHICLE-CAPACITY
           MOVE 'hgeuahghihisiuh' TO WS-VEHICLE-LICENSE-PLATE
           MOVE 890 TO WS-VEHICLE-PRICE-FACTOR
           .

       INITIALIZE-RECORD.
           MOVE 'TAGUIG' TO WS-ROUTE-ORIGIN
           MOVE 'MAKATI' TO WS-ROUTE-DESTINATION
           MOVE 15 TO WS-ROUTE-DISTANCE
           MOVE 10 TO WS-ROUTE-BASE-PRICE
           .

       TRAVERSAL-ROUTE-RECORD.
           MOVE SPACES TO WS-EOF
           OPEN INPUT FS-ROUTES-FILE    
               READ FS-ROUTES-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END 
               DISPLAY FS-ROUTE-ID 
               DISPLAY FS-ROUTE-ORIGIN
               DISPLAY FS-ROUTE-DESTINATION
               DISPLAY FS-ROUTE-DISTANCE
               DISPLAY FS-ROUTE-BASE-PRICE
               DISPLAY FS-ROUTE-TIME-STAMP
               END-READ
           CLOSE FS-ROUTES-FILE
           .

       RECORD-ROUTE.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUES TO FS-ROUTE-ID
       
           OPEN I-O FS-ROUTES-FILE
       
           START FS-ROUTES-FILE KEY IS GREATER THAN FS-ROUTE-ID
           READ FS-ROUTES-FILE NEXT RECORD
               AT END MOVE 1 TO WS-INCREMENT-VALUE    
               NOT AT END 
                   PERFORM UNTIL WS-EOF = 'Y'
                       MOVE FS-ROUTE-ID TO WS-LAST-GENERATED-ID
                       READ FS-ROUTES-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           NOT AT END
                               CONTINUE
                       END-READ
                   END-PERFORM
           END-READ
       
           IF WS-LAST-GENERATED-ID NOT EQUAL TO SPACES THEN
               MOVE WS-L-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE 
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF
           
           PERFORM GENERATE-ID-SEQUENCE
           
           MOVE WS-GENERATED-USER-ID TO WS-ROUTE-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-TIME-STAMP TO WS-ROUTE-TIME-STAMP
       
           MOVE WS-ROUTES-RECORD TO FS-ROUTES-RECORD
           
           WRITE FS-ROUTES-RECORD
           END-WRITE
       
           CLOSE FS-ROUTES-FILE
           .

       RECORD-VEHICLE.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUES TO FS-VEHICLE-ID
       
           OPEN I-O FS-VEHICLES-FILE
       
           START FS-VEHICLES-FILE KEY IS GREATER THAN FS-VEHICLE-ID
           READ FS-VEHICLES-FILE NEXT RECORD
               AT END MOVE 1 TO WS-INCREMENT-VALUE    
               NOT AT END 
                   PERFORM UNTIL WS-EOF = 'Y'
                       MOVE FS-VEHICLE-ID TO WS-LAST-GENERATED-ID
                       READ FS-VEHICLES-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           NOT AT END
                               CONTINUE
                       END-READ
                   END-PERFORM
           END-READ
       
           IF WS-LAST-GENERATED-ID NOT EQUAL TO SPACES THEN
               MOVE WS-L-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE 
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF
           
           PERFORM GENERATE-ID-SEQUENCE
           
           MOVE WS-GENERATED-USER-ID TO WS-VEHICLE-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-TIME-STAMP TO WS-VEHICLE-TIME-STAMP
       
           MOVE WS-VEHICLES-RECORD TO FS-VEHICLES-RECORD
       
           WRITE FS-VEHICLES-RECORD
           END-WRITE
       
           CLOSE FS-VEHICLES-FILE
           .

       RECORD-SCHEDULE.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUES TO FS-SCHEDULE-ID
       
           OPEN I-O FS-SCHEDULES-FILE
       
           START FS-SCHEDULES-FILE KEY IS GREATER THAN FS-SCHEDULE-ID
           READ FS-SCHEDULES-FILE NEXT RECORD
               AT END MOVE 1 TO WS-INCREMENT-VALUE    
               NOT AT END 
                   PERFORM UNTIL WS-EOF = 'Y'
                       MOVE FS-SCHEDULE-ID TO WS-LAST-GENERATED-ID
                       READ FS-SCHEDULES-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           NOT AT END
                               CONTINUE
                       END-READ
                   END-PERFORM
           END-READ
       
           IF WS-LAST-GENERATED-ID NOT EQUAL TO SPACES THEN
               MOVE WS-L-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE 
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF
           
           PERFORM GENERATE-ID-SEQUENCE
           
           MOVE WS-GENERATED-USER-ID TO WS-SCHEDULE-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-TIME-STAMP TO FS-S-TIME-STAMP
       
           MOVE WS-SCHEDULES-RECORD TO FS-SCHEDULES-RECORD
       
           WRITE FS-SCHEDULES-RECORD
           END-WRITE
       
           CLOSE FS-SCHEDULES-FILE
           .

       CHECK-FILE-STATUS.
           MOVE SPACES TO WS-FILE-STATUS
           OPEN I-O FS-ROUTES-FILE
               IF WS-FILE-STATUS NOT = '00' THEN
                   OPEN OUTPUT FS-ROUTES-FILE
                   IF WS-FILE-STATUS NOT = '00' THEN    
                       DISPLAY 'Error : <Unable to Open File>'
                   END-IF
               END-IF
           CLOSE FS-ROUTES-FILE

           MOVE SPACES TO WS-FILE-STATUS
           OPEN I-O FS-VEHICLES-FILE
               IF WS-FILE-STATUS NOT = '00' THEN
                   OPEN OUTPUT FS-VEHICLES-FILE
                   IF WS-FILE-STATUS NOT = '00' THEN    
                       DISPLAY 'Error : <Unable to Open File>'
                   END-IF
               END-IF
           CLOSE FS-VEHICLES-FILE

           MOVE SPACES TO WS-FILE-STATUS
           OPEN I-O FS-SCHEDULES-FILE
               IF WS-FILE-STATUS NOT = '00' THEN
                   OPEN OUTPUT FS-SCHEDULES-FILE
                   IF WS-FILE-STATUS NOT = '00' THEN    
                       DISPLAY 'Error : <Unable to Open File>'
                   END-IF
               END-IF
           CLOSE FS-SCHEDULES-FILE
           .

       GENERATE-ID-SEQUENCE.
      *    Generates ID (Using DATE, TIME, and INCREMENT VALUE)
           ACCEPT WS-GSI-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME(1:6) TO WS-GSI-TIME
           MOVE WS-INCREMENT-VALUE TO WS-GSI-INCREMENT-VALUE
           .
       
       GENERATE-TIME-STAMP.
      *    Generates Time Stamp of Creation
           ACCEPT WS-TS-DATE FROM DATE
           ACCEPT WS-TIME FROM TIME
           MOVE WS-TIME(1:2) TO WS-TS-HOUR
           MOVE WS-TIME(3:2) TO WS-TS-MINUTES
           MOVE WS-TIME(5:2) TO WS-TS-SECOND
           .
       