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

           SELECT FS-PAYMENT-FILE ASSIGN TO 'data/payments.txt'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FS-PAYMENT-ID
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT FS-CURRENT-BOOKING-FILE ASSIGN 
               TO 'data/artifact/current_booking.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FS-CURRENT-BOOKING-FILE.
       01  FS-CURRENT-BOOKING-ID    PIC X(15).

       FD  FS-CURRENT-USER-FILE.
       01  FS-CURRENT-USER    PIC X(15).

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

       FD  FS-PAYMENT-FILE.
       01  FS-PAYMENT-RECORD.
           02    FS-PAYMENT-ID    PIC X(15).
           02    FS-FK-BOOKING-ID    PIC X(15).
           02    FS-PAYMENT-METHOD    PIC X(11).
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
       
       WORKING-STORAGE SECTION.
       01  WS-DATE     PIC 9(6).
       01  WS-TIME     PIC 9(8).
       01  WS-EOF    PIC X.
       01  WS-BOOKING-RECORD.
           02    WS-BOOKING-ID    PIC X(15).
           02    WS-FK-USER-ID    PIC X(15).
           02    WS-FK-SCHEDULE-ID    PIC X(15).
           02    WS-SEAT-NUMBER    PIC 9(10).    
           02    WS-BOOKING-STATUS    PIC X(9).
           02    WS-PRICE    PIC 9(10)V99.
           02    WS-BOOKING-TIME-STAMP.
               03    WS-B-TS-DATE    PIC 99/99/99.
               03    WS-B-TS-FILLER-SPACE    PIC X(3).
               03    WS-B-TS-TIME.
                   04    WS-B-TS-HOUR    PIC 99.
                   04    WS-B-TS-FILLER-COLON-1    PIC X.
                   04    WS-B-TS-MINUTES    PIC 99.
                   04    WS-B-TS-FILLER-COLON-2    PIC X.
                   04    WS-B-TS-SECONDS    PIC 99.
       01  WS-PAYMENT-RECORD.
           02    WS-PAYMENT-ID    PIC X(15).
           02    WS-FK-BOOKING-ID    PIC X(15).
           02    WS-PAYMENT-METHOD    PIC X(11).
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
       01  WS-SCHEDULE-TABLE    PIC X(15) OCCURS 100 TIMES.
       01  WS-AVAILABLE-TABLE    PIC X(15) OCCURS 100 TIMES.
       01  WS-PASSENGER-PAGE-CHOICE    PIC X.
       01  WS-ADD-BOOKING-CHOICE    PIC X.
       01  WS-SCHEDULE-CHOICE    PIC X(3).
       01  WS-SCHEDULE-CHOICE-INT    PIC 9(3).
       01  WS-SCHEDULE-COUNTER    PIC 9(3).
       01  WS-BUFFER    PIC X.
       01  WS-COUNTER-I    PIC 9(4).
       01  WS-SEARCH-QUERIES.
           02    WS-ORIGIN-Q    PIC X(30).
           02    WS-DESTINATION-Q    PIC X(30).
           02    WS-DATE-D-Q.
               03    WS-MONTH-D-Q    PIC 99/.
               03    WS-DAY-D-Q    PIC 99/.
               03    WS-YEAR-D-Q    PIC 99.
           02    WS-DATE-A-Q.
               03    WS-MONTH-A-Q    PIC 99/.
               03    WS-DAY-A-Q    PIC 99/.
               03    WS-YEAR-A-Q    PIC 99.
           02    WS-TIME-D-Q.
               03    WS-HOUR-D-Q    PIC 99.
               03    WS-MINUTES-D-Q    PIC 99.
           02    WS-TIME-A-Q.
               03    WS-HOUR-A-Q    PIC 99.
               03    WS-MINUTES-A-Q    PIC 99.
           02    WS-VEHICLE-CLASS-Q    PIC X(11).
       01  WS-PAYMENT-METHOD-CHOICE    PIC X.
       01  WS-CREDIT-CARD-NUMBER    PIC X(16).
       01  WS-CONFIRM-BOOKING    PIC X(3).
       01  WS-VEHICLE-CLASS-CLEAN PIC X(11).
       01  WS-REPEAT    PIC XXX.

       
       PROCEDURE DIVISION.
           PERFORM CHECK-FILE-STATUS

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
           MOVE FS-P-USER-ID TO WS-FK-USER-ID
           .

       USER-MAIN-PAGE.
           PERFORM UNTIL WS-PASSENGER-PAGE-CHOICE = '3'
               PERFORM CLEAR
               MOVE SPACES TO WS-REPEAT

           DISPLAY "***************************************************"-
           "*************"
           DISPLAY "                     W E L C O M E                 "-
           " "
           DISPLAY "***************************************************"-
           "*************"
           DISPLAY " Welcome - " FS-P-FIRST-NAME
           DISPLAY " "
           
           PERFORM TRAVERSAL-BOOKING

           DISPLAY " "
           DISPLAY " Menu Options:                                     "-
           "   "
           DISPLAY " 1 - Add Booking                                   "-
           "   "
           DISPLAY " 2 - Cancel Booking                                "-
           "   "
           DISPLAY " 3 - Quit                                          "-
           "   "
           DISPLAY " "
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-PASSENGER-PAGE-CHOICE

               EVALUATE WS-PASSENGER-PAGE-CHOICE
                   WHEN '1'
                       PERFORM ADD-BOOKING-PAGE
                   WHEN '2'
                       PERFORM UPDATE-BOOKING
                   WHEN '3'
                       STOP RUN
                   WHEN OTHER
                       PERFORM INVALID-INPUT
               END-EVALUATE

           END-PERFORM
           .

       ADD-BOOKING-PAGE.
           MOVE SPACES TO WS-ADD-BOOKING-CHOICE
           PERFORM UNTIL WS-ADD-BOOKING-CHOICE = '3'
           MOVE SPACES TO WS-REPEAT
               PERFORM CLEAR
           DISPLAY "***************************************************"-
           "****"
           DISPLAY "                     A D D   B O O K I N G         "-
           "  "
           DISPLAY "***************************************************"-
           "****"
           DISPLAY " "
           DISPLAY " Menu Options:                                     "-
           "   "
           DISPLAY " 1 - See Available Schedule                        "-
           "   "
           DISPLAY " 2 - Search Schedule                               "-
           "   "
           DISPLAY " 3 - Back                                          "-
           "   "
           DISPLAY " "
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-ADD-BOOKING-CHOICE


               EVALUATE WS-ADD-BOOKING-CHOICE
                   WHEN '1'
                       PERFORM SEE-AVAILABLE-SCHEDULES
                   WHEN '2'
                       PERFORM SEARCH-SCHEDULE
                   WHEN '3'
                       CONTINUE
                   WHEN OTHER
                       PERFORM INVALID-INPUT
               END-EVALUATE
           END-PERFORM
           .

       UPDATE-BOOKING.
           DISPLAY "***************************************************"-
           "****"
           DISPLAY "              C A N C E L   B O O K I N G          "-
           "  "
           DISPLAY "***************************************************"-
           "****"
           DISPLAY " "
           DISPLAY " [Cancelling 12 hours before the departure date wil"-
           "l be unrefundable]"
           DISPLAY " "
           DISPLAY " Enter Booking ID to Cancel: " WITH NO ADVANCING
           ACCEPT FS-BOOKING-ID.


           OPEN I-O FS-BOOKING-FILE
           OPEN INPUT FS-SCHEDULES-FILE
           OPEN I-O FS-VEHICLES-FILE
               READ FS-BOOKING-FILE
               KEY IS FS-BOOKING-ID
               INVALID KEY PERFORM BOOKING-NOT-FOUND-MESSAGE
               NOT INVALID KEY
                   READ FS-SCHEDULES-FILE
                   KEY IS FS-SCHEDULE-ID
                   END-READ
                   MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID
                   READ FS-VEHICLES-FILE
                   KEY IS FS-VEHICLE-ID
                   END-READ
                   MOVE 'canceled' TO FS-BOOKING-STATUS

                   REWRITE FS-BOOKING-RECORD
                   END-REWRITE

                   ADD FS-SEAT-NUMBER TO FS-VEHICLE-CAPACITY

                   REWRITE FS-VEHICLES-RECORD
                   END-REWRITE
               END-READ    
           CLOSE FS-VEHICLES-FILE
           CLOSE FS-SCHEDULES-FILE
           CLOSE FS-BOOKING-FILE
           ACCEPT WS-BUFFER

           PERFORM UNTIL WS-REPEAT = 'NO'
               DISPLAY ' '
               DISPLAY 'Do you want to try again? [YES/NO] '
               WITH NO ADVANCING
               ACCEPT WS-REPEAT

               MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
               WS-REPEAT

               EVALUATE WS-REPEAT
                   WHEN 'YES'
                       PERFORM UPDATE-BOOKING
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      PERFORM INVALID-INPUT
               END-EVALUATE

               END-PERFORM
           .

       SEE-AVAILABLE-SCHEDULES.
           MOVE SPACES TO WS-REPEAT
           MOVE LOW-VALUES TO WS-SCHEDULE-CHOICE
           PERFORM UNTIL WS-SCHEDULE-CHOICE = '0'
               PERFORM CLEAR
               
               PERFORM CLEAR-SCHEDULE-TABLE
               PERFORM INITILIAZE-SCHEDULE-TABLE
               PERFORM DISPLAY-SCHEDULE-TABLE
      
               DISPLAY 'Enter your choice [0 - to back] : ' 
               WITH NO ADVANCING
               ACCEPT WS-SCHEDULE-CHOICE

               MOVE WS-SCHEDULE-CHOICE TO WS-SCHEDULE-CHOICE-INT
               EVALUATE TRUE
               
                   WHEN WS-SCHEDULE-CHOICE-INT > WS-SCHEDULE-COUNTER
                       PERFORM OUT-OF-RANGE-MESSAGE
                   WHEN WS-SCHEDULE-CHOICE = '0'
                       CONTINUE
                   WHEN WS-SCHEDULE-CHOICE = SPACES
                       PERFORM INVALID-INPUT
                   WHEN OTHER 
                       MOVE WS-SCHEDULE-TABLE(WS-SCHEDULE-CHOICE-INT) TO
                       WS-FK-SCHEDULE-ID
                       PERFORM SEAT-SELECTION
               END-EVALUATE
           END-PERFORM

           PERFORM UNTIL WS-REPEAT = 'NO'
               DISPLAY ' '
               DISPLAY 'Do you want to see available schedules again? ['-
               'YES/NO] '
               WITH NO ADVANCING
               ACCEPT WS-REPEAT

               MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
               WS-REPEAT

               EVALUATE WS-REPEAT
                   WHEN 'YES'
                       PERFORM SEE-AVAILABLE-SCHEDULES
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      PERFORM INVALID-INPUT
               END-EVALUATE

               END-PERFORM
           .

       SEARCH-SCHEDULE.
           MOVE SPACES TO WS-REPEAT
           DISPLAY "***************************************************"-
           "****"
           DISPLAY "                   S E A R C H   S C H E D U L E   "-
           "      "
           DISPLAY "***************************************************"-
           "****"
           DISPLAY " "
           DISPLAY " Enter Details for Your Search:                    "-
           "   "
           
           DISPLAY " Origin: " WITH NO ADVANCING
           ACCEPT WS-ORIGIN-Q
           
           DISPLAY " Destination: " WITH NO ADVANCING
           ACCEPT WS-DESTINATION-Q
           
           DISPLAY " [DEPARTURE]                                       "-
           "   "
           DISPLAY " Month [MM]: " WITH NO ADVANCING
           ACCEPT WS-MONTH-D-Q
           DISPLAY " Day [DD]: " WITH NO ADVANCING
           ACCEPT WS-DAY-D-Q
           DISPLAY " Year [YY]: " WITH NO ADVANCING
           ACCEPT WS-YEAR-D-Q
           DISPLAY " Hour [HH]: " WITH NO ADVANCING
           ACCEPT WS-HOUR-D-Q
           DISPLAY " Minute [MM]: " WITH NO ADVANCING
           ACCEPT WS-MINUTES-D-Q
           
           DISPLAY " [ARRIVAL]                                         "-
           "  "
           DISPLAY " Month [MM]: " WITH NO ADVANCING
           ACCEPT WS-MONTH-A-Q
           DISPLAY " Day [DD]: " WITH NO ADVANCING
           ACCEPT WS-DAY-A-Q
           DISPLAY " Year [YY]: " WITH NO ADVANCING
           ACCEPT WS-YEAR-A-Q
           DISPLAY " Hour [HH]: " WITH NO ADVANCING
           ACCEPT WS-HOUR-A-Q
           DISPLAY " Minute [MM]: " WITH NO ADVANCING
           ACCEPT WS-MINUTES-A-Q
           
           DISPLAY " Class [Standard / Deluxe / First Class]: "
            WITH NO ADVANCING
           ACCEPT WS-VEHICLE-CLASS-Q


           MOVE FUNCTION LOWER-CASE(WS-ORIGIN-Q) TO WS-ORIGIN-Q
           MOVE FUNCTION LOWER-CASE(WS-DESTINATION-Q) TO 
           WS-DESTINATION-Q

           MOVE FUNCTION LOWER-CASE(WS-VEHICLE-CLASS-Q)  
           TO WS-VEHICLE-CLASS-Q

           EVALUATE WS-VEHICLE-CLASS-Q
               WHEN 'standard'
                   MOVE 's' TO WS-VEHICLE-CLASS-Q
               WHEN 'deluxe'
                   MOVE 'd' TO WS-VEHICLE-CLASS-Q
               WHEN 'first class'
                   MOVE 'f' TO WS-VEHICLE-CLASS-Q
           END-EVALUATE

           MOVE LOW-VALUES TO WS-SCHEDULE-CHOICE

           PERFORM UNTIL WS-SCHEDULE-CHOICE = '0'
               PERFORM CLEAR
               PERFORM CLEAR-SCHEDULE-TABLE
               PERFORM INITIALIZE-SCHEDULE-SEARCH

               PERFORM DISPLAY-SCHEDULE-TABLE
               
               DISPLAY 'Enter your choice [0 - to back] : ' 
               WITH NO ADVANCING
               ACCEPT WS-SCHEDULE-CHOICE

               MOVE WS-SCHEDULE-CHOICE TO WS-SCHEDULE-CHOICE-INT

               EVALUATE TRUE
                   WHEN WS-SCHEDULE-CHOICE-INT > WS-SCHEDULE-COUNTER
                       PERFORM OUT-OF-RANGE-MESSAGE
                   WHEN WS-SCHEDULE-CHOICE = '0'
                       CONTINUE
                   WHEN WS-SCHEDULE-CHOICE = SPACES
                       PERFORM INVALID-INPUT
                   WHEN OTHER
                       MOVE WS-SCHEDULE-TABLE(WS-SCHEDULE-CHOICE-INT) TO
                       WS-FK-SCHEDULE-ID
                       PERFORM SEAT-SELECTION
               END-EVALUATE
           END-PERFORM

           PERFORM UNTIL WS-REPEAT = 'NO'
               DISPLAY ' '
               DISPLAY 'Do you want to search again? [YES/NO] '
               WITH NO ADVANCING
               ACCEPT WS-REPEAT

               MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
               WS-REPEAT

               EVALUATE WS-REPEAT
                   WHEN 'YES'
                       PERFORM SEARCH-SCHEDULE    
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      PERFORM INVALID-INPUT
               END-EVALUATE

               END-PERFORM
           .
       
       SEAT-SELECTION.
           MOVE SPACES TO WS-REPEAT
           PERFORM CLEAR
           DISPLAY "***************************************************"-
           "****"
           DISPLAY "                 S E A T   S E L E C T I O N       "-
           "     "
           DISPLAY "***************************************************"-
           "****"
           DISPLAY " "
           DISPLAY " [AVAILABLE SEATS]                                 "-
           "   "
           DISPLAY " "
           
           OPEN INPUT FS-SCHEDULES-FILE
           OPEN I-O FS-VEHICLES-FILE
           OPEN INPUT FS-ROUTES-FILE
           
               MOVE WS-FK-SCHEDULE-ID TO FS-SCHEDULE-ID
               READ FS-SCHEDULES-FILE
               KEY IS FS-SCHEDULE-ID
               END-READ
               
               MOVE FS-FK-ROUTE-ID TO FS-ROUTE-ID
               MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID
               READ FS-VEHICLES-FILE
               KEY IS FS-VEHICLE-ID
               END-READ
               
               DISPLAY " Vehicle Serial : " FS-VEHICLE-SERIAL
               DISPLAY " "
               DISPLAY " Available Seats : " FS-VEHICLE-CAPACITY
               DISPLAY " "
               DISPLAY " Enter Seat Number: " WITH NO ADVANCING
               ACCEPT WS-SEAT-NUMBER

               IF WS-SEAT-NUMBER = SPACES OR WS-SEAT-NUMBER IS NOT 
               NUMERIC OR WS-SEAT-NUMBER = ZEROES THEN
                   DISPLAY ' '
                   PERFORM INVALID-INPUT
               PERFORM UNTIL WS-REPEAT = 'NO'
               DISPLAY ' '
               DISPLAY 'Do you want to try again? [YES/NO] '
               WITH NO ADVANCING
               ACCEPT WS-REPEAT

               MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
               WS-REPEAT

               EVALUATE WS-REPEAT
                   WHEN 'YES'
                       PERFORM SEAT-SELECTION
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      PERFORM INVALID-INPUT
               END-EVALUATE

               END-PERFORM

               ELSE
                   PERFORM PAYMENT-SELECTION
               END-IF



           CLOSE FS-VEHICLES-FILE
           CLOSE FS-ROUTES-FILE
           CLOSE FS-SCHEDULES-FILE
           ACCEPT WS-BUFFER
           .

       PAYMENT-SELECTION.
           MOVE SPACES TO WS-REPEAT
           PERFORM CLEAR
           DISPLAY "***************************************************"-
           "****"
           DISPLAY "                P A Y M E N T   S E C T I O N      "-
           "   "
           DISPLAY "***************************************************"-
           "****"
           DISPLAY " "

           PERFORM PRICING-ENGINE

           DISPLAY " Price : " WS-PRICE
           DISPLAY " "
           
           DISPLAY " Payment Method: 1 - Cash"
           DISPLAY "                 2 - Online payment (Credit Card)"
           DISPLAY " "
           DISPLAY " Confirm payment method: " WITH NO ADVANCING
           ACCEPT WS-PAYMENT-METHOD-CHOICE


           EVALUATE WS-PAYMENT-METHOD-CHOICE
               WHEN '1'
                   MOVE 'cash' TO WS-PAYMENT-METHOD
                   MOVE 'reserved' TO WS-BOOKING-STATUS
                   PERFORM BOOKING-CONFIRMATION
               WHEN '2'
                   DISPLAY ' '
                   DISPLAY ' Credit Card Number : ' WITH NO ADVANCING
                   ACCEPT WS-CREDIT-CARD-NUMBER

                   IF WS-CREDIT-CARD-NUMBER = SPACES THEN
                       DISPLAY ' '
                       PERFORM INVALID-INPUT

                       PERFORM UNTIL WS-REPEAT = 'NO'
                       DISPLAY ' '
                       DISPLAY 'Do you want to try again? [YES/NO] '
                       WITH NO ADVANCING
                       ACCEPT WS-REPEAT
       
                       MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
                       WS-REPEAT
       
                       EVALUATE WS-REPEAT
                           WHEN 'YES'
                               PERFORM PAYMENT-SELECTION
                           WHEN 'NO'
                               CONTINUE
                           WHEN OTHER
                              PERFORM INVALID-INPUT
                       END-EVALUATE
       
                       END-PERFORM
                   ELSE
                       MOVE 'credit-card' TO WS-PAYMENT-METHOD
                       MOVE 'paid' TO WS-BOOKING-STATUS
                       PERFORM BOOKING-CONFIRMATION
                   END-IF
               WHEN OTHER
                  PERFORM INVALID-CHOICE-MESSAGE

                  PERFORM UNTIL WS-REPEAT = 'NO'
               DISPLAY ' '
               DISPLAY 'Do you want to try again? [YES/NO] '
               WITH NO ADVANCING
               ACCEPT WS-REPEAT

               MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
               WS-REPEAT

               EVALUATE WS-REPEAT
                   WHEN 'YES'
                       PERFORM PAYMENT-SELECTION
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      PERFORM INVALID-INPUT
               END-EVALUATE

               END-PERFORM
           END-EVALUATE
           .

       PRICING-ENGINE.
           MOVE ZEROES TO WS-PRICE

           READ FS-ROUTES-FILE
           KEY IS FS-ROUTE-ID
           END-READ
           READ FS-VEHICLES-FILE
           KEY IS FS-VEHICLE-ID
           END-READ

           COMPUTE WS-PRICE = (FS-ROUTE-BASE-PRICE * 
           FS-VEHICLE-PRICE-FACTOR) * WS-SEAT-NUMBER

           MOVE WS-PRICE TO WS-PAYMENT-AMOUNT
           .

       BOOKING-CONFIRMATION.
           MOVE SPACES TO WS-REPEAT
           DISPLAY "***************************************************"-
           "*************"
           DISPLAY "                    BOOKING SUMMARY                "-
           "   "
           DISPLAY "***************************************************"-
           "*************"
           DISPLAY "Booking User        : " FS-P-LAST-NAME ", " 
           FS-P-FIRST-NAME
           DISPLAY "Travel Route        : " FS-ROUTE-ORIGIN " TO " 
                   FS-ROUTE-DESTINATION
           DISPLAY "Departure Date/Time : " FS-S-DEPARTURE-TIME
           DISPLAY "Arrival Date/Time   : " FS-S-ARRIVAL-TIME
           DISPLAY "Vehicle Serial      : " FS-VEHICLE-SERIAL
           DISPLAY "Seat Number         : " WS-SEAT-NUMBER
           DISPLAY "Booking Status      : " WS-BOOKING-STATUS
           DISPLAY "Payment Method      : " WS-PAYMENT-METHOD
           DISPLAY "Price               : " WS-PRICE
           DISPLAY "***************************************************"-
           "*************"


           DISPLAY "***************************************************"-
           "*************"
           DISPLAY "               CONFIRM YOUR BOOKING                "-
           "  "
           DISPLAY "***************************************************"-
           "*************"
           DISPLAY " "
           DISPLAY "Do you confirm?  [YES]  [NO] " WITH NO ADVANCING
           ACCEPT WS-CONFIRM-BOOKING
           DISPLAY "***************************************************"
           "*************"


           MOVE FUNCTION LOWER-CASE(WS-CONFIRM-BOOKING) TO 
           WS-CONFIRM-BOOKING

           EVALUATE WS-CONFIRM-BOOKING
               WHEN 'yes'
                   PERFORM BOOKED-SUCCESSFULY-MESSAGE
                   SUBTRACT WS-SEAT-NUMBER FROM FS-VEHICLE-CAPACITY
                   REWRITE FS-VEHICLES-RECORD
                   END-REWRITE
                   PERFORM RECORD-BOOKING
                   MOVE FS-BOOKING-ID TO WS-FK-BOOKING-ID
                   PERFORM RECORD-PAYMENT
                   MOVE FS-BOOKING-ID TO FS-CURRENT-BOOKING-ID
                   OPEN OUTPUT FS-CURRENT-BOOKING-FILE
                       WRITE FS-CURRENT-BOOKING-ID
                       END-WRITE
                   CLOSE FS-CURRENT-BOOKING-FILE
                   PERFORM TICKETING
                   DISPLAY " Press 'enter' key to continue..."
                   ACCEPT WS-BUFFER
                   GO TO USER-MAIN-PAGE
               WHEN 'no'
                   PERFORM DISCONTINUE-MESSAGE
                   DISPLAY " Press 'enter' key to continue..."
                   ACCEPT WS-BUFFER
                   GO TO USER-MAIN-PAGE
               WHEN OTHER
                  PERFORM INVALID-CHOICE-MESSAGE

                  PERFORM UNTIL WS-REPEAT = 'NO'
                   DISPLAY ' '
                   DISPLAY 'Do you want to try again? [YES/NO] '
                   WITH NO ADVANCING
                   ACCEPT WS-REPEAT
      
                   MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO 
                   WS-REPEAT
      
                   EVALUATE WS-REPEAT
                       WHEN 'YES'
                           PERFORM BOOKING-CONFIRMATION
                       WHEN 'NO'
                           CONTINUE
                       WHEN OTHER
                          PERFORM INVALID-INPUT
                   END-EVALUATE
      
                   END-PERFORM

           END-EVALUATE
           .

       TICKETING.
           CALL 'SYSTEM' USING 'backend/ticketing_module'
           .    

       CLEAR-SCHEDULE-TABLE.
           PERFORM VARYING WS-COUNTER-I FROM 1 BY 1 UNTIL 
           WS-COUNTER-I > 100
               MOVE SPACES TO WS-SCHEDULE-TABLE(WS-COUNTER-I)
           END-PERFORM
           .

       INITIALIZE-SCHEDULE-SEARCH.
           MOVE SPACES TO WS-EOF
           MOVE 1 TO WS-COUNTER-I
           MOVE ZEROES TO WS-SCHEDULE-COUNTER
           OPEN INPUT FS-SCHEDULES-FILE
           OPEN INPUT FS-VEHICLES-FILE
           OPEN INPUT FS-ROUTES-FILE
           PERFORM UNTIL WS-COUNTER-I > 100 OR WS-EOF = 'Y'
               READ FS-SCHEDULES-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END 
                   MOVE FS-FK-ROUTE-ID TO FS-ROUTE-ID
                   MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID
                   READ FS-ROUTES-FILE
                   END-READ
                   READ FS-VEHICLES-FILE
                   END-READ
                   IF WS-ORIGIN-Q = FS-ROUTE-ORIGIN AND WS-DESTINATION-Q
                   = FS-ROUTE-DESTINATION AND WS-DATE-D-Q = 
                   FS-S-D-DATE AND WS-DATE-A-Q = FS-S-A-DATE AND
                   WS-HOUR-D-Q = FS-S-D-HOUR AND WS-HOUR-A-Q = 
                   FS-S-A-HOUR AND WS-MINUTES-D-Q = FS-S-D-MINUTES AND
                   WS-MINUTES-A-Q = FS-S-A-MINUTES AND FS-S-STATUS = 
                   'active' THEN
                       MOVE FS-SCHEDULE-ID TO 
                       WS-SCHEDULE-TABLE(WS-COUNTER-I)
                       ADD 1 TO WS-SCHEDULE-COUNTER
                       ADD 1 TO WS-COUNTER-I    
                   END-IF
               END-READ
           END-PERFORM
           CLOSE FS-SCHEDULES-FILE
           CLOSE FS-VEHICLES-FILE
           CLOSE FS-ROUTES-FILE
           .

       INITILIAZE-SCHEDULE-TABLE.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-SCHEDULE-COUNTER
           OPEN INPUT FS-SCHEDULES-FILE
               PERFORM VARYING WS-COUNTER-I FROM 1 BY 1 UNTIL 
               WS-COUNTER-I > 100 OR WS-EOF = 'Y'
                   READ FS-SCHEDULES-FILE NEXT RECORD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF FS-S-STATUS = 'active' THEN
                       MOVE FS-SCHEDULE-ID TO 
                       WS-SCHEDULE-TABLE(WS-COUNTER-I)
                       ADD 1 TO WS-SCHEDULE-COUNTER
                       END-IF
                   END-READ
               END-PERFORM
           CLOSE FS-SCHEDULES-FILE
           .

       DISPLAY-SCHEDULE-TABLE.
           OPEN INPUT FS-SCHEDULES-FILE
           OPEN INPUT FS-VEHICLES-FILE
           OPEN INPUT FS-ROUTES-FILE
           DISPLAY "***************************************************"-
           "*****************************"
           DISPLAY "                               SCHEDULE SUMMARY    "-
           "                             "
           DISPLAY "***************************************************"-
           "*****************************"
           DISPLAY ' '
           DISPLAY '        SCHEDULE ID   |                            '-
           '  ROUTE                               | VEHICLE SERIAL |   '-
           ' CLASS |    '
           'DEPARTURE TIME    |     ARRIVAL TIME    | STATUS '
           DISPLAY "---------------------------------------------------"-
           "-----------------------------"

           PERFORM VARYING WS-COUNTER-I FROM 1 BY 1 UNTIL WS-COUNTER-I > 
           WS-SCHEDULE-COUNTER
               MOVE WS-SCHEDULE-TABLE(WS-COUNTER-I) TO FS-SCHEDULE-ID

               READ FS-SCHEDULES-FILE 
                   KEY IS FS-SCHEDULE-ID
                   NOT INVALID KEY
                       MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID
                       MOVE FS-FK-ROUTE-ID TO FS-ROUTE-ID

                       READ FS-VEHICLES-FILE
                           KEY IS FS-VEHICLE-ID
                       END-READ

                       READ FS-ROUTES-FILE
                           KEY IS FS-ROUTE-ID
                       END-READ

                       EVALUATE FS-VEHICLE-CLASS
                           WHEN 's'
                               MOVE 'STANDARD' TO WS-VEHICLE-CLASS-CLEAN
                           WHEN 'd'
                               MOVE 'DELUXE' TO WS-VEHICLE-CLASS-CLEAN
                           WHEN 'f'
                               MOVE 'FIRST-CLASS' TO 
                               WS-VEHICLE-CLASS-CLEAN
                           WHEN OTHER
                               MOVE 'UNKNOWN' TO WS-VEHICLE-CLASS-CLEAN
                       END-EVALUATE
                       
                       DISPLAY WS-COUNTER-I '. 'FS-SCHEDULE-ID ' | ' 
                       FS-ROUTE-ORIGIN ' TO ' 
                       FS-ROUTE-DESTINATION ' | ' FS-VEHICLE-SERIAL 
                       ' | ' WS-VEHICLE-CLASS-CLEAN ' | '
                       FS-S-DEPARTURE-TIME ' | ' FS-S-ARRIVAL-TIME ' | ' 
                       FS-S-STATUS  
                       DISPLAY "---------------------------------------"-
                       "-----------------------------------------"
                   END-READ
           END-PERFORM.

           CLOSE FS-SCHEDULES-FILE
           CLOSE FS-ROUTES-FILE
           CLOSE FS-VEHICLES-FILE
           .
       
       TRAVERSAL-BOOKING.
           MOVE SPACES TO WS-EOF
           MOVE 1 TO WS-COUNTER-I
           DISPLAY "***************************************************"-
           "*************"
           DISPLAY "                     M Y   B O O K I N G S         "-
           "  "
           DISPLAY "***************************************************"-
           "*************"
           DISPLAY ' '
           DISPLAY '   BOOKING ID    |         ORIGIN - DESTINATION    '-
           '            |      TRAVEL DATE / TIME        | VEHICLE SERI'-
           'AL |    SEAT    |     PAID      | STATUS |'
           DISPLAY '---------------------------------------------------'-
           '-----------------------------------------------------------'

           OPEN I-O FS-BOOKING-FILE
           OPEN INPUT FS-SCHEDULES-FILE
           OPEN INPUT FS-VEHICLES-FILE
           OPEN INPUT FS-ROUTES-FILE

           PERFORM UNTIL WS-EOF = 'Y'
               READ FS-BOOKING-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF FS-FK-USER-ID = FS-P-USER-ID THEN
                       MOVE FS-FK-SCHEDULE-ID TO FS-SCHEDULE-ID

                       READ FS-SCHEDULES-FILE
                           KEY IS FS-SCHEDULE-ID
                       END-READ
      
                       MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID
                       MOVE FS-FK-ROUTE-ID TO FS-ROUTE-ID
      
                       READ FS-VEHICLES-FILE
                           KEY IS FS-VEHICLE-ID
                       END-READ
      
                       READ FS-ROUTES-FILE
                           KEY IS FS-ROUTE-ID
                       END-READ

                   DISPLAY FS-BOOKING-ID ' | ' FS-ROUTE-ORIGIN ' TO '
                        FS-ROUTE-DESTINATION ' | ' FS-S-DEPARTURE-TIME 
                        ' TO ' FS-S-ARRIVAL-TIME ' | ' FS-VEHICLE-SERIAL 
                        ' | ' FS-SEAT-NUMBER ' | ' FS-PRICE ' | ' 
                        FS-BOOKING-STATUS 
                   DISPLAY "-------------------------------------------"-
                   "--------------------"
                   END-IF
               END-READ
           END-PERFORM

           CLOSE FS-ROUTES-FILE
           CLOSE FS-VEHICLES-FILE
           CLOSE FS-SCHEDULES-FILE
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
           OPEN I-O FS-PAYMENT-FILE
               IF WS-FILE-STATUS NOT = '00' THEN    
                   OPEN OUTPUT FS-PAYMENT-FILE
                   IF WS-FILE-STATUS NOT = '00' THEN    
                       DISPLAY 'Error : <Unable to open file>'
                   END-IF
               END-IF
           CLOSE FS-PAYMENT-FILE
           .

       INVALID-INPUT.
           DISPLAY "***************************************************"
           DISPLAY "*                 Invalid Input                   *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       INVALID-CHOICE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*                Invalid Message                  *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       BOOKED-SUCCESSFULY-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*              Booked Successfuly                 *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       DISCONTINUE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*             Discontinue Booking                 *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       BOOKING-NOT-FOUND-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*               Booking Not Found                 *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       OUT-OF-RANGE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*                 Out of Range                    *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       CLEAR.
           CALL 'SYSTEM' USING 'clear'.
