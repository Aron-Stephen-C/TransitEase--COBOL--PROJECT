       IDENTIFICATION DIVISION.
       PROGRAM-ID. ticketing_module.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             
           SELECT FS-PASSENGER-FILE ASSIGN TO 'data/passenger_file.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-P-USER-ID
               FILE STATUS IS WS-FILE-STATUS.


           SELECT FS-ROUTES-FILE ASSIGN TO 'data/routes.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-ROUTE-ID
               FILE STATUS IS WS-FILE-STATUS.


           SELECT FS-VEHICLES-FILE ASSIGN TO 'data/vehicles.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-VEHICLE-ID
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT FS-SCHEDULES-FILE ASSIGN TO 'data/schedules.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-SCHEDULE-ID
               FILE STATUS IS WS-FILE-STATUS.


           SELECT FS-BOOKING-FILE ASSIGN TO 'data/booking.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-BOOKING-ID
               FILE STATUS IS WS-STATUS.
           
           SELECT FS-TICKET-FILE ASSIGN TO 'data/ticket.txt'
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-TICKET-STATUS.

           SELECT FS-CURRENT-BOOKING-FILE ASSIGN 
               TO 'data/artifact/current_booking.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       FD  FS-CURRENT-BOOKING-FILE.
       01  FS-CURRENT-BOOKING-ID    PIC X(15).

       FD  FS-BOOKING-FILE.
       01  FS-BOOKING-RECORD.
           02    FS-BOOKING-ID    PIC X(15).
           02    FS-FK-USER-ID    PIC X(15).
           02    FS-FK-SCHEDULE-ID    PIC X(15).
           02    FS-SEAT-NUMBER    PIC 9(10).
           02    FS-BOOKING-STATUS    PIC X(9).
           02    FS-PRICE    PIC 9(10)V99.
           02    FS-TIME-STAMP.
               03    FS-TS-DATE    PIC 99/99/99.
               03    FS-TS-FILLER-SPACE    PIC X(3).
               03    FS-TS-TIME.
                   04    FS-TS-HOUR    PIC 99.
                   04    FS-TS-FILLER-COLON-1    PIC X.
                   04    FS-TS-MINUTES    PIC 99.
                   04    FS-TS-FILLER-COLON-2    PIC X.
                   04    FS-TS-SECONDS    PIC 99.

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
           02    FS-S-STATUS    PIC X(8).
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
       
       FD  FS-TICKET-FILE.
       01  FS-TICKET-RECORD                PIC X(100).


       WORKING-STORAGE SECTION.
       01  WS-USER-RECORD.
           02    WS-USER-ID                PIC X(15).
           02    WS-FIRST-NAME             PIC X(50).
           02    WS-LAST-NAME              PIC X(50).
           02    WS-EMAIL                  PIC X(100).
       
       
       01  WS-ROUTES-RECORD.
           02    WS-ROUTE-ID               PIC X(15).
           02    WS-ROUTE-ORIGIN           PIC X(30).
           02    WS-ROUTE-DESTINATION      PIC X(30).
       
       
       01  WS-SCHEDULES-RECORD.
           02    WS-SCHEDULE-ID            PIC X(15).
           02    WS-S-DEPARTURE-TIME.
               03    WS-S-D-DATE           PIC 99/99/99.
               03    WS-S-D-FILLER-SPACE-1 PIC X(3) VALUE SPACES.
               03    WS-S-D-TIME.
                   04    WS-S-D-HOUR       PIC 99.
                   04    WS-S-D-COLON-1    PIC X VALUE ':'.
                   04    WS-S-D-MINUTES    PIC 99.
               03    WS-S-D-FILLER-SPACE-2 PIC X(3) VALUE SPACES.
               03    WS-S-D-TIME-FORMAT    PIC XX.
       
       
           02    WS-S-ARRIVAL-TIME.
               03    WS-S-A-DATE           PIC 99/99/99.
               03    WS-S-A-FILLER-SPACE-1 PIC X(3) VALUE SPACES.
               03    WS-S-A-TIME.
                   04    WS-S-A-HOUR       PIC 99.
                   04    WS-S-A-COLON-1    PIC X VALUE ':'.
                   04    WS-S-A-MINUTES    PIC 99.
               03    WS-S-A-FILLER-SPACE-2 PIC X(3) VALUE SPACES.
               03    WS-S-A-TIME-FORMAT    PIC XX.
       
       
       01  WS-BOOKING-RECORD.
           02    WS-BOOKING-ID             PIC X(15).
           02    WS-SEAT-NUMBER            PIC 9(10).
           02    WS-PRICE                  PIC 9(9)V99.


       01  WS-STATUS                       PIC XX.
       01  WS-FILE-STATUS                  PIC XX.
       01  WS-TICKET-STATUS                PIC XX.
       01  WS-LINE                         PIC X(100).


       PROCEDURE DIVISION.

      *    OPEN I-O FS-BOOKING-FILE
      *    IF WS-STATUS NOT = '00'
      *        PERFORM ERROR-OPENING-MESSAGE
      *        STOP RUN
      *    END-IF
      *
      *
      *    OPEN OUTPUT FS-TICKET-FILE
      *    IF WS-TICKET-STATUS NOT = '00'
      *        PERFORM ERROR-OPENING-MESSAGE
      *        STOP RUN
      *    END-IF
      *
      *
      *    PERFORM READ-AND-GENERATE-TICKET
      *
      *
      *    CLOSE FS-BOOKING-FILE
      *    CLOSE FS-TICKET-FILE
      *
      *
      *    PERFORM SUCCESS-TICKET-MESSAGE
      *    
      *    CALL 'system' USING 'python3 txt_to_pdf_and_email.py '
      *    WS-EMAIL

           PERFORM FETCH-BOOKING-FILE
      *    PERFORM DISPLAY-BOOKING-INFORMATION


           STOP RUN.

       FETCH-BOOKING-FILE.
           OPEN INPUT FS-CURRENT-BOOKING-FILE
           OPEN INPUT FS-BOOKING-FILE
           OPEN INPUT FS-SCHEDULES-FILE
           OPEN INPUT FS-ROUTES-FILE
           OPEN INPUT FS-VEHICLES-FILE
           OPEN INPUT FS-PASSENGER-FILE

               READ FS-CURRENT-BOOKING-FILE INTO FS-BOOKING-ID
               END-READ

               READ FS-BOOKING-FILE
               KEY IS FS-BOOKING-ID
               END-READ

               MOVE FS-FK-USER-ID TO FS-P-USER-ID

               MOVE FS-FK-SCHEDULE-ID TO FS-SCHEDULE-ID

               READ FS-PASSENGER-FILE

               END-READ

               READ FS-SCHEDULES-FILE
               KEY IS FS-SCHEDULE-ID
               END-READ

               MOVE FS-FK-ROUTE-ID TO FS-ROUTE-ID
               MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID

               READ FS-ROUTES-FILE 
               KEY IS FS-ROUTE-ID
               END-READ

               READ FS-VEHICLES-FILE
               KEY IS FS-VEHICLE-ID
               END-READ

           CLOSE FS-VEHICLES-FILE
           CLOSE FS-ROUTES-FILE
           CLOSE FS-SCHEDULES-FILE
           CLOSE FS-BOOKING-FILE
           CLOSE FS-CURRENT-BOOKING-FILE
           CLOSE FS-PASSENGER-FILE
           .

       DISPLAY-BOOKING-INFORMATION.
           DISPLAY FS-BOOKING-ID
           DISPLAY FS-P-FIRST-NAME
           DISPLAY FS-P-LAST-NAME
           DISPLAY FS-ROUTE-ORIGIN
           DISPLAY FS-ROUTE-DESTINATION
           DISPLAY FS-S-DEPARTURE-TIME
           DISPLAY FS-S-ARRIVAL-TIME
           DISPLAY FS-SEAT-NUMBER
           DISPLAY FS-PRICE
       .
       
      *READ-AND-GENERATE-TICKET.
      *    READ FS-BOOKING-FILE
      *        AT END
      *            PERFORM NO-BOOKING-MESSAGE
      *            STOP RUN
      *        NOT AT END
      *            PERFORM WRITE-TICKET
      *    END-READ
      *    .
      *
      *
      *WRITE-TICKET.
      *    MOVE "------------------------------------------------------"
      *    TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    MOVE "                  GENERATED TICKET                    "
      *    TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    MOVE "------------------------------------------------------"
      *    TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    MOVE "I. User Information" TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "User ID          : " FS-P-USER-ID
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *    
      *    STRING "First Name       : " FS-P-FIRST-NAME
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Last Name        : " FS-P-LAST-NAME
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    MOVE "------------------------------------------------------"
      *    TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    MOVE "II. Travel Information" TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Route ID         : " FS-ROUTE-ID
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *    
      *    STRING "Origin           : " FS-ROUTE-ORIGIN
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Destination      : " FS-ROUTE-DESTINATION
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Departure Time   : " FS-S-DEPARTURE-TIME
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Arrival Time     : " FS-S-ARRIVAL-TIME
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Seat Number      : " FS-SEAT-NUMBER
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    STRING "Price            : Php" FS-PRICE
      *    DELIMITED BY SIZE INTO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    MOVE "------------------@TransitEase2025--------------------"
      *    TO WS-LINE
      *    WRITE FS-TICKET-RECORD FROM WS-LINE
      *
      *
      *    .
      *
      *
      *ERROR-OPENING-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*         Error opening file. Try Again!          *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."
      *    .
      *
      *
      *SUCCESS-TICKET-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*          Ticket generation completed!           *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."
      *    .
      *
      *
      *NO-BOOKING-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*       No booking records found. Try Again!      *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."
      *    .











