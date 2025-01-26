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

           SELECT FS-CURRENT-BOOKING-FILE ASSIGN 
               TO 'data/artifact/current_booking.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.

           SELECT FS-OUTPUT-FILE ASSIGN TO 'data/booking_details.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.

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

       FD  FS-OUTPUT-FILE.
       01  FS-OUTPUT-RECORD       PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-STATUS                       PIC XX.
       01  WS-FILE-STATUS                  PIC XX.
       01  WS-TICKET-STATUS                PIC XX.
       01  WS-LINE                         PIC X(100).
       01  WS-COMMAND                      PIC X(255).


       PROCEDURE DIVISION.
           
           PERFORM FETCH-BOOKING-FILE
           PERFORM DISPLAY-BOOKING-INFORMATION
           PERFORM WRITE-BOOKING-INFORMATION
           
           DISPLAY " "
           PERFORM SUCCESS-TICKET-MESSAGE
       
           STRING 'python3 backend/txt_to_pdf_and_email.py 'FS-P-EMAIL
           DELIMITED BY SIZE 
           INTO WS-COMMAND
           END-STRING
           CALL 'SYSTEM' USING WS-COMMAND
           END-CALL

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
           DISPLAY "---------------------------------------------------"
           DISPLAY "                 GENERATED TICKET                  "
           DISPLAY "---------------------------------------------------"
           DISPLAY " Booking ID: " FS-BOOKING-ID
           DISPLAY "---------------------------------------------------"
           DISPLAY " "
           DISPLAY " I. User Information"
           DISPLAY "      First Name:        " FS-P-FIRST-NAME
           DISPLAY "      Last Name:         " FS-P-LAST-NAME
           DISPLAY " "
           DISPLAY " II. Travel Information"
           DISPLAY "      Origin:            " FS-ROUTE-ORIGIN
           DISPLAY "      Destination:       " FS-ROUTE-DESTINATION
           DISPLAY "      Departure Time:    " FS-S-DEPARTURE-TIME
           DISPLAY "      Arrival Time:      " FS-S-ARRIVAL-TIME
           DISPLAY "      Seat Number:       " FS-SEAT-NUMBER
           DISPLAY "      Ticket Price:      Php " FS-PRICE
           DISPLAY " "
           DISPLAY "----------------@TransitEase2025-------------------"
           .

       WRITE-BOOKING-INFORMATION.
           OPEN OUTPUT FS-OUTPUT-FILE
                   IF WS-FILE-STATUS NOT = '00'
                        DISPLAY "Error opening output file."
                        STOP RUN
                   END-IF
           
           MOVE "---------------------------------------------------" 
           TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
       
           MOVE "                 GENERATED TICKET                  " 
           TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
       
           MOVE "---------------------------------------------------" 
           TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
       
           STRING "Booking ID: " FS-BOOKING-ID DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING

           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD
       
           MOVE "---------------------------------------------------" 
           TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
       
           MOVE " " TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
       
           MOVE "I. User Information" TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.

           STRING "First Name: " FS-P-FIRST-NAME DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD

           STRING "Last Name: " FS-P-LAST-NAME DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD
       
           MOVE " " TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
       
           MOVE "II. Travel Information" TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.


           STRING "Origin: " FS-ROUTE-ORIGIN DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD

           STRING "Destination: " FS-ROUTE-DESTINATION DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD
       
           STRING "Departure Time: " FS-S-DEPARTURE-TIME 
           DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD

           STRING "Arrival Time: " FS-S-ARRIVAL-TIME 
           DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD
       
           STRING "Seat Number: " FS-SEAT-NUMBER 
           DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           MOVE SPACES TO FS-OUTPUT-RECORD

           STRING "Ticket Price: " FS-PRICE 
           DELIMITED BY SIZE 
           INTO FS-OUTPUT-RECORD
           END-STRING
           WRITE FS-OUTPUT-RECORD.

           
           MOVE " " TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
           
           MOVE "----------------@TransitEase2025-------------------" 
           TO FS-OUTPUT-RECORD
           WRITE FS-OUTPUT-RECORD.
           
           CLOSE FS-OUTPUT-FILE.

      *ERROR-OPENING-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*         Error opening file. Try Again!          *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."
      *    .
      *
       SUCCESS-TICKET-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*          Ticket generation completed!           *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."
           .
       
      *NO-BOOKING-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*       No booking records found. Try Again!      *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."
      *    .











