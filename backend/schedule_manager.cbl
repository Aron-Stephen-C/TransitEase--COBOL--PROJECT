      * This module manages Routes, Vehicles, Schedules records.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. schedule_manager.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
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
       
       DATA DIVISION.
       FILE SECTION.
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
      
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS    PIC XX.
       01  WS-DATE.
           02    WS-YEAR    PIC 99.
           02    WS-MONTH    PIC 99.
           02    WS-DAY    PIC 99.
       01  WS-INPUT-DATE.
           02    WS-I-MONTH    PIC 99/.
           02    WS-I-DAY    PIC 99/.
           02    WS-I-YEAR    PIC 99.
       01  WS-MONTH-CHECKER PIC 99.
           88  WS-MONTHS-31    VALUE 1,3,5,7,8,10,12.
           88  WS-MONTHS-30    VALUE 4,6,9,11.
       01  WS-LIMIT-DAYS PIC 99.
       01  WS-TIME.
           02    WS-HOUR    PIC 99.
           02    WS-MINUTE    PIC 99.
           02    WS-SECOND    PIC 99.
           02    WS-MSECOND    PIC 99.
           02    WS-TIME-FORMAT    PIC XX.
       01  WS-INPUT-TIME.
           02    WS-I-HOUR    PIC 99.
           02    WS-I-MINUTE    PIC 99.
           02    WS-I-SECOND    PIC 99.
           02    WS-I-MSECOND    PIC 99.
           02    WS-I-TIME-FORMAT    PIC XX.
       01  WS-INCREMENT-VALUE    PIC 9(3).
       01  WS-EOF    PIC X.
       01  WS-GENERATED-ID.
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
       01  WS-TIME-STAMP-D-A.
           02    WS-DA-DATE    PIC 99/99/99.
           02    WS-DA-FILLER-SPACE-1    PIC X(3) VALUE SPACES.
           02    WS-DA-TIME.
               03    WS-DA-HOUR    PIC 99.
               03    WS-DA-COLON-1    PIC X VALUE ':'.
               03    WS-DA-MINUTES    PIC 99.
           02    WS-DA-FILLER-SPACE-2    PIC X(3) VALUE SPACES.
           02    WS-DA-TIME-FORMAT     PIC XX.
       01  WS-ROUTES-RECORD.
           02    WS-ROUTE-ID    PIC X(15).
           02    WS-ROUTE-ORIGIN    PIC X(30).
           02    WS-ROUTE-DESTINATION    PIC X(30).
           02    WS-ROUTE-DISTANCE    PIC 9(10)V99.
           02    WS-ROUTE-BASE-PRICE    PIC 9(10)V99.
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
           02    WS-VEHICLE-SERIAL    PIC X(6).
           02    WS-VEHICLE-CLASS    PIC X.
           02    WS-VEHICLE-CAPACITY    PIC 9(3).
           02    WS-VEHICLE-LICENSE-PLATE    PIC X(20).
           02    WS-VEHICLE-PRICE-FACTOR    PIC 9(10)V9(2).
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
           02    WS-FK-ROUTE-ID    PIC X(15).
           02    WS-FK-VEHICLE-ID    PIC X(15).
           02    WS-S-DEPARTURE-TIME.
               03    WS-S-D-DATE    PIC 99/99/99.
               03    WS-S-D-FILLER-SPACE-1    PIC X(3) VALUE SPACES.
               03    WS-S-D-TIME.
                   04    WS-S-D-HOUR    PIC 99.
                   04    WS-S-D-COLON-1    PIC X VALUE ':'.
                   04    WS-S-D-MINUTES    PIC 99.
               03    WS-S-D-FILLER-SPACE-2    PIC X(3) VALUE SPACES.
               03    WS-S-D-TIME-FORMAT    PIC XX.
           02    WS-S-ARRIVAL-TIME.
               03    WS-S-A-DATE    PIC 99/99/99.
               03    WS-S-A-FILLER-SPACE-1    PIC X(3).
               03    WS-S-A-TIME.
                   04    WS-S-A-HOUR    PIC 99.
                   04    WS-S-A-COLON-1    PIC X.
                   04    WS-S-A-MINUTES    PIC 99.
               03    WS-S-A-FILLER-SPACE-2    PIC X(3).
               03    WS-S-A-TIME-FORMAT    PIC XX.
           02    WS-S-STATUS    PIC X(5).
           02    WS-S-TIME-STAMP.
               03    WS-S-DATE    PIC 99/99/99.
               03    WS-S-FILLER-SPACE    PIC X(3).
               03    WS-S-TIME.
                   04    WS-S-HOUR    PIC 99.
                   04    WS-S-COLON-1    PIC X.
                   04    WS-S-MINUTES    PIC 99.
                   04    WS-S-COLON-2    PIC X.
                   04    WS-S-SECOND    PIC 99.
       01  WS-SCHEDULE-MM-CHOICE PIC X.
       01  WS-BUFFER    PIC X.
       01  WS-COUNTER-I PIC 9(3).

       01  WS-VEHICLE-MENU-CHOICE     PIC X.
       01  WS-ROUTE-MENU-CHOICE     PIC X.
       01  WS-SCHEDULE-MENU-CHOICE     PIC X.
       01  WS-BOOL     PIC 9 VALUE 0.
       01  WS-REPEAT PIC X(3).
       01  WS-TIME-FORMAT-CHOICE    PIC X.

       LINKAGE SECTION.
       
       PROCEDURE DIVISION.
           PERFORM CHECK-FILE-STATUS

           PERFORM SCHEDULE-MAIN-MENU
           
           GOBACK
           STOP RUN.

       CLEAR.
           CALL "SYSTEM" USING "clear"
           .

       INVALID-INPUT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*            Invalid Input. Try Again!            *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       SCHEDULE-MAIN-MENU.
           PERFORM UNTIL WS-SCHEDULE-MM-CHOICE = 4
           
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*                 Welcome, Admin!                 *"
           DISPLAY "***************************************************"
           DISPLAY "*                [1] Add Route                    *"
           DISPLAY "*                [2] Add Vehicle                  *"
           DISPLAY "*                [3] Add Schedule                 *"
           DISPLAY "*                [4] Exit                         *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-SCHEDULE-MM-CHOICE

               EVALUATE WS-SCHEDULE-MM-CHOICE
                   WHEN 1 PERFORM ADD-ROUTE-PAGE
                   WHEN 2 PERFORM ADD-VEHICLE-PAGE
                   WHEN 3 PERFORM ADD-SCHEDULE-PAGE
                   WHEN 4
                       STOP RUN
                   WHEN OTHER 
                       PERFORM INVALID-INPUT-MESSAGE
               END-EVALUATE
               

           END-PERFORM
           .


       ADD-ROUTE-PAGE.
           MOVE SPACES TO WS-ROUTES-RECORD
           PERFORM UNTIL WS-ROUTE-MENU-CHOICE = '4'
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*                 Add Route Page                  *"
           DISPLAY "***************************************************"

           PERFORM TRAVERSAL-ROUTE-RECORD

           DISPLAY ' '
           DISPLAY '1 - Add Route'
           DISPLAY '2 - Update Route'
           DISPLAY '3 - Remove Route'
           DISPLAY '4 - back to route page'
           DISPLAY ' '
           DISPLAY 'Enter your choice : ' WITH NO ADVANCING
           ACCEPT WS-ROUTE-MENU-CHOICE

           DISPLAY ' '

           EVALUATE WS-ROUTE-MENU-CHOICE
               WHEN '1'
                   PERFORM ADD-ROUTE
               WHEN '2'
                   PERFORM UPDATE-ROUTE
               WHEN '3'
                   PERFORM REMOVE-ROUTE
               WHEN '4'
                   CONTINUE
               WHEN OTHER
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM ADD-ROUTE-PAGE
           END-EVALUATE

           END-PERFORM
           .

       ADD-ROUTE.
           DISPLAY '[ADD ROUTE]'
           DISPLAY ' '
           DISPLAY " Enter Route Origin: " WITH NO ADVANCING
           ACCEPT WS-ROUTE-ORIGIN
           DISPLAY " Enter Route Destination: " WITH NO ADVANCING
           ACCEPT WS-ROUTE-DESTINATION
           DISPLAY " Enter Route Distance: " WITH NO ADVANCING
           ACCEPT WS-ROUTE-DISTANCE
           DISPLAY " Enter Route Base Price: " WITH NO ADVANCING
           ACCEPT WS-ROUTE-BASE-PRICE

           IF WS-ROUTE-ORIGIN = SPACES OR WS-ROUTE-DESTINATION = SPACES
           OR WS-ROUTE-DISTANCE = ZEROES OR WS-ROUTE-BASE-PRICE = ZEROES
               DISPLAY 'Must fill all of the fields'
               PERFORM ADD-ROUTE-PAGE
           END-IF

           PERFORM RECORD-ROUTE
           PERFORM SUCCESS-ADD-ROUTE-MESSAGE

           ACCEPT WS-BUFFER
           .

       UPDATE-ROUTE.
           DISPLAY '[UPDATE ROUTE]'
           DISPLAY ' '
           DISPLAY 'Search ID : ' WITH NO ADVANCING
           ACCEPT FS-ROUTE-ID

           OPEN I-O FS-ROUTES-FILE
               READ FS-ROUTES-FILE
               KEY IS FS-ROUTE-ID
               INVALID KEY DISPLAY 'Route Record Not Found'
               NOT INVALID KEY
                  DISPLAY ' '
                   DISPLAY " Enter Route Origin: " WITH NO ADVANCING
                   ACCEPT WS-ROUTE-ORIGIN
                   DISPLAY " Enter Route Destination: "
                    WITH NO ADVANCING
                   ACCEPT WS-ROUTE-DESTINATION
                   DISPLAY " Enter Route Distance: " WITH NO ADVANCING
                   ACCEPT WS-ROUTE-DISTANCE
                   DISPLAY " Enter Route Base Price: " WITH NO ADVANCING
                   ACCEPT WS-ROUTE-BASE-PRICE
       
                   IF WS-ROUTE-ORIGIN = SPACES OR 
                   WS-ROUTE-DESTINATION = SPACES
                   OR WS-ROUTE-DISTANCE = ZEROES OR 
                   WS-ROUTE-BASE-PRICE = ZEROES
                       DISPLAY 'Must fill all of the fields'
                       PERFORM ADD-ROUTE-PAGE
                   ELSE
                       MOVE WS-ROUTE-ORIGIN TO FS-ROUTE-ORIGIN
                       MOVE WS-ROUTE-DESTINATION TO FS-ROUTE-DESTINATION
                       MOVE WS-ROUTE-DISTANCE TO FS-ROUTE-DISTANCE
                       MOVE WS-ROUTE-BASE-PRICE TO 
                       FS-ROUTE-BASE-PRICE
                       REWRITE FS-ROUTES-RECORD
                           INVALID KEY 
                               DISPLAY 'ERROR: UPDATE FAILED.'
                           NOT INVALID KEY
                               DISPLAY 'UPDATE SUCCESSFUL!'
                       END-REWRITE
                   END-IF 
               END-READ
           CLOSE FS-ROUTES-FILE
           ACCEPT WS-BUFFER
           .

       REMOVE-ROUTE.
           DISPLAY '[DELETE ROUTE]'
           DISPLAY ' '
           DISPLAY 'Search ID : ' WITH NO ADVANCING
           ACCEPT FS-ROUTE-ID
           
           OPEN I-O FS-ROUTES-FILE
           DELETE FS-ROUTES-FILE
               INVALID KEY DISPLAY 'Route Record Not Found'
               NOT INVALID KEY DISPLAY 'Successfully removed'
           END-DELETE
           CLOSE FS-ROUTES-FILE
           .

       SUCCESS-ADD-ROUTE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*              Success: Route Added!              *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       ADD-VEHICLE-PAGE.

           PERFORM UNTIL WS-VEHICLE-MENU-CHOICE = '4'
               PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*                Add Vehicle Page                 *"
           DISPLAY "***************************************************"
      
               PERFORM TRAVERSAL-VEHICLE-RECORD
      
               DISPLAY ' '
               DISPLAY '1 - Add Vehicle'
               DISPLAY '2 - Update Vehicle'
               DISPLAY '3 - Remove Vehicle'
               DISPLAY '4 - Back to main page'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-VEHICLE-MENU-CHOICE

               DISPLAY ' '

               EVALUATE WS-VEHICLE-MENU-CHOICE
                   WHEN '1'
                       PERFORM ADD-VEHICLE
                   WHEN '2'
                       PERFORM UPDATE-VEHICLE
                   WHEN '3'
                       PERFORM REMOVE-VEHICLE
                   WHEN '4'
                       CONTINUE
                   WHEN OTHER
                       PERFORM INVALID-INPUT-MESSAGE
                       PERFORM ADD-VEHICLE-PAGE
               END-EVALUATE
           END-PERFORM
           .

       ADD-VEHICLE.
           DISPLAY '[ADD VEHICLE]'
           DISPLAY " Enter Vehicle Serial: " WITH NO ADVANCING
           ACCEPT WS-VEHICLE-SERIAL
           DISPLAY " Enter Vehicle Class: " WITH NO ADVANCING
           ACCEPT WS-VEHICLE-CLASS
           DISPLAY " Enter Vehicle Capacity: " WITH NO ADVANCING
           ACCEPT WS-VEHICLE-CAPACITY
           DISPLAY " Enter Vehicle License Plate: " WITH NO ADVANCING
           ACCEPT WS-VEHICLE-LICENSE-PLATE
           DISPLAY " Enter Vehicle Price Factor: " WITH NO ADVANCING
           ACCEPT WS-VEHICLE-PRICE-FACTOR

           IF WS-VEHICLE-CLASS = SPACES OR WS-VEHICLE-CAPACITY = SPACES
           OR WS-VEHICLE-LICENSE-PLATE = SPACES OR 
           WS-VEHICLE-PRICE-FACTOR = ZEROES
               DISPLAY ' '
               DISPLAY 'Error : <Must fill all the fields>'
               PERFORM ADD-VEHICLE-PAGE
           ELSE
               PERFORM RECORD-VEHICLE
               PERFORM SUCCESS-ADD-VEHICLE-MESSAGE
           END-IF

           ACCEPT WS-BUFFER
           .

       UPDATE-VEHICLE.
           DISPLAY '[UPDATE VEHICLE]'
           DISPLAY ' '
           DISPLAY 'Search ID : ' WITH NO ADVANCING
           ACCEPT FS-VEHICLE-ID

           OPEN I-O FS-VEHICLES-FILE
               READ FS-VEHICLES-FILE
               KEY IS FS-VEHICLE-ID
               INVALID KEY 
                   DISPLAY ' '
                   DISPLAY 'Vehicle Not Found.'
               NOT INVALID KEY 
                   DISPLAY " Enter Vehicle Serial: " WITH NO ADVANCING
                   ACCEPT WS-VEHICLE-SERIAL
                   DISPLAY " Enter Vehicle Class: " WITH NO ADVANCING
                   ACCEPT WS-VEHICLE-CLASS
                   DISPLAY " Enter Vehicle Capacity: "
                    WITH NO ADVANCING
                   ACCEPT WS-VEHICLE-CAPACITY
                   DISPLAY " Enter Vehicle License Plate: "
                    WITH NO ADVANCING
                   ACCEPT WS-VEHICLE-LICENSE-PLATE
                   DISPLAY " Enter Vehicle Price Factor: "
                    WITH NO ADVANCING
                   ACCEPT WS-VEHICLE-PRICE-FACTOR
       
                   IF WS-VEHICLE-CLASS = SPACES OR 
                       WS-VEHICLE-CAPACITY = SPACES OR 
                       WS-VEHICLE-LICENSE-PLATE = SPACES OR 
                       WS-VEHICLE-PRICE-FACTOR = SPACES
                           DISPLAY 'Error : <Must fill all the fields>'
                           PERFORM ADD-VEHICLE-PAGE
                   ELSE
                       MOVE WS-VEHICLE-SERIAL TO FS-VEHICLE-SERIAL
                       MOVE WS-VEHICLE-CLASS TO FS-VEHICLE-CLASS
                       MOVE WS-VEHICLE-CAPACITY TO FS-VEHICLE-CAPACITY
                       MOVE WS-VEHICLE-LICENSE-PLATE TO 
                       FS-VEHICLE-LICENSE-PLATE
                       MOVE WS-VEHICLE-PRICE-FACTOR TO 
                       FS-VEHICLE-PRICE-FACTOR
                       REWRITE FS-VEHICLES-RECORD    
                           INVALID KEY
                               DISPLAY 'Error: Update Failed.'
                            NOT INVALID KEY 
                                DISPLAY 'Update Successful.'
                       END-REWRITE
                   END-IF
               END-READ
           CLOSE FS-VEHICLES-FILE
           ACCEPT WS-BUFFER
           .

       REMOVE-VEHICLE.
           DISPLAY '[REMOVE VEHICLE]'
           DISPLAY ' '
           DISPLAY 'Search ID : ' WITH NO ADVANCING
           ACCEPT FS-VEHICLE-ID

           OPEN I-O FS-VEHICLES-FILE
        
           DELETE FS-VEHICLES-FILE
               INVALID KEY DISPLAY 'Vehicle not found.'
               NOT INVALID KEY DISPLAY 'Successfully removed'
           END-DELETE
      *     Pakibago nalang
                           
           CLOSE FS-VEHICLES-FILE
           ACCEPT WS-BUFFER
           .

       SUCCESS-ADD-VEHICLE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*            Success: Vehicle Added!              *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       ADD-SCHEDULE-PAGE. 
           PERFORM UNTIL WS-SCHEDULE-MENU-CHOICE = '5'
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*               Add Schedule Page                 *"
           DISPLAY "***************************************************"

           PERFORM TRAVERSAL-SCHEDULE

           DISPLAY ' '
           DISPLAY '1 - Add Schedule'
           DISPLAY '2 - Update Schedule'
           DISPLAY '3 - Cancel Schedule'
           DISPLAY '4 - Remove Schedule'
           DISPLAY '5 - back to schedule page'
           DISPLAY ' '
           DISPLAY 'Enter your choice : ' WITH NO ADVANCING
           ACCEPT WS-SCHEDULE-MENU-CHOICE
           DISPLAY ' '

               EVALUATE WS-SCHEDULE-MENU-CHOICE
                   WHEN '1'
                       PERFORM ADD-SCHEDULE
                   WHEN '2'
                       PERFORM UPDATE-SCHEDULE
                   WHEN '3'
                       PERFORM CANCEL-SCHEDULE
                   WHEN '4'
                       PERFORM REMOVE-SCHEDULE
                   WHEN '5'
                       CONTINUE
                   WHEN OTHER
                       DISPLAY ' '
                       DISPLAY 'Error : <Invalid Choice>'
                       ACCEPT WS-BUFFER
                       PERFORM ADD-SCHEDULE-PAGE
               END-EVALUATE
      
           
           END-PERFORM 
           ACCEPT WS-BUFFER
           .

       ADD-SCHEDULE.
           PERFORM TRAVERSAL-ROUTE-RECORD
           DISPLAY " Enter Route ID: " WITH NO ADVANCING
           ACCEPT WS-FK-ROUTE-ID
           MOVE WS-FK-ROUTE-ID TO FS-ROUTE-ID
           OPEN INPUT FS-ROUTES-FILE
               READ FS-ROUTES-FILE
               INVALID KEY 
               PERFORM INVALID-INPUT-MESSAGE
               PERFORM ADD-SCHEDULE-PAGE
               END-READ
           CLOSE FS-ROUTES-FILE
       
           PERFORM TRAVERSAL-VEHICLE-RECORD
           DISPLAY " Enter Vehicle ID: " WITH NO ADVANCING
           ACCEPT WS-FK-VEHICLE-ID
           MOVE WS-FK-VEHICLE-ID TO FS-VEHICLE-ID
           OPEN INPUT FS-VEHICLES-FILE
               READ FS-VEHICLES-FILE
               INVALID KEY 
               PERFORM INVALID-INPUT-MESSAGE
               PERFORM ADD-SCHEDULE-PAGE
               END-READ
           CLOSE FS-VEHICLES-FILE

           MOVE 0 TO WS-BOOL

           DISPLAY '[DEPARTURE TIME]'
           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'Enter Month[MM] : ' WITH NO ADVANCING
               ACCEPT WS-I-MONTH
               DISPLAY 'Enter Day[DD] : ' WITH NO ADVANCING
               ACCEPT WS-I-DAY
               DISPLAY 'Enter Year[YY] : ' WITH NO ADVANCING
               ACCEPT WS-I-YEAR

               ACCEPT WS-DATE FROM DATE


               IF WS-I-MONTH < WS-MONTH OR WS-I-MONTH > 12 THEN
                   DISPLAY 'Invalid Month'
               ELSE
                   MOVE WS-I-MONTH TO WS-MONTH-CHECKER

                   EVALUATE TRUE
                       WHEN WS-MONTHS-31
                           MOVE 31 TO WS-LIMIT-DAYS
                       WHEN WS-MONTHS-30
                           MOVE 30 TO WS-LIMIT-DAYS
                       WHEN OTHER
                           MOVE 28 TO WS-LIMIT-DAYS
                   END-EVALUATE

                   IF (WS-I-MONTH = WS-MONTH AND WS-I-DAY < WS-DAY) 
                   OR WS-I-DAY > WS-LIMIT-DAYS THEN
                       DISPLAY 'Invalid Day'
                   ELSE
                       IF WS-I-YEAR NOT = WS-YEAR
                           DISPLAY 'Year'
                       ELSE
                           MOVE WS-INPUT-DATE TO WS-DA-DATE
                            MOVE 1 TO WS-BOOL
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           MOVE 0 TO WS-BOOL

           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'TIME FORMAT'
               DISPLAY '1 - Morning (AM)'
               DISPLAY '2 - Evening / Afternoon (PM)'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-TIME-FORMAT-CHOICE

               EVALUATE WS-TIME-FORMAT-CHOICE
                   WHEN '1'
                       MOVE 'AM' TO WS-I-TIME-FORMAT
                   WHEN '2'
                       MOVE 'PM' TO WS-I-TIME-FORMAT
               END-EVALUATE

               DISPLAY ' '
               DISPLAY 'Enter Hour [HH]: ' WITH NO ADVANCING
               ACCEPT WS-I-HOUR
               DISPLAY 'Enter Minute [MM]: ' WITH NO ADVANCING
               ACCEPT WS-I-MINUTE

               ACCEPT WS-TIME FROM TIME

               EVALUATE TRUE
                   WHEN WS-HOUR = 0
                       MOVE 12 TO WS-HOUR
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR < 12
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR = 12
                       MOVE 'PM' TO WS-TIME-FORMAT
                   WHEN OTHER
                       COMPUTE WS-HOUR = WS-HOUR - 12
                       MOVE 'PM' TO WS-TIME-FORMAT
               END-EVALUATE

               IF WS-I-HOUR < 0 OR WS-I-HOUR > 12 THEN
                   DISPLAY 'Invalid Time: Hour'
               ELSE 
                   IF (WS-I-HOUR = WS-HOUR AND WS-I-MINUTE < 0) OR
                    WS-I-MINUTE > 59 THEN
                   DISPLAY 'Invalid Time: Minute'
                   ELSE
                       MOVE WS-I-HOUR TO WS-DA-HOUR
                       MOVE WS-I-MINUTE TO WS-DA-MINUTES
                       MOVE WS-I-TIME-FORMAT TO WS-DA-TIME-FORMAT
                       MOVE 1 TO WS-BOOL
                   END-IF
               END-IF

           END-PERFORM

           MOVE WS-TIME-STAMP-D-A TO WS-S-DEPARTURE-TIME

           MOVE 0 TO WS-BOOL

           DISPLAY '[ARRIVAL TIME]'
           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'Enter Month[MM] : ' WITH NO ADVANCING
               ACCEPT WS-I-MONTH
               DISPLAY 'Enter Day[DD] : ' WITH NO ADVANCING
               ACCEPT WS-I-DAY
               DISPLAY 'Enter Year[YY] : ' WITH NO ADVANCING
               ACCEPT WS-I-YEAR

               ACCEPT WS-DATE FROM DATE

               IF WS-I-MONTH < WS-MONTH OR WS-I-MONTH > 12 THEN
                   DISPLAY 'Invalid Month'
               ELSE
                   MOVE WS-I-MONTH TO WS-MONTH-CHECKER

                   EVALUATE TRUE
                       WHEN WS-MONTHS-31
                           MOVE 31 TO WS-LIMIT-DAYS
                       WHEN WS-MONTHS-30
                           MOVE 30 TO WS-LIMIT-DAYS
                       WHEN OTHER
                           MOVE 28 TO WS-LIMIT-DAYS
                   END-EVALUATE

                   IF (WS-I-MONTH = WS-MONTH AND WS-I-DAY < WS-DAY) 
                   OR WS-I-DAY > WS-LIMIT-DAYS THEN
                       DISPLAY 'Invalid Day'
                   ELSE
                       IF WS-I-YEAR NOT = WS-YEAR
                           DISPLAY 'Year'
                       ELSE
                           MOVE WS-INPUT-DATE TO WS-DA-DATE
                            MOVE 1 TO WS-BOOL
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           MOVE 0 TO WS-BOOL

           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'TIME FORMAT'
               DISPLAY '1 - Morning (AM)'
               DISPLAY '2 - Evening / Afternoon (PM)'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-TIME-FORMAT-CHOICE
      
               EVALUATE WS-TIME-FORMAT-CHOICE
                   WHEN '1'
                       MOVE 'AM' TO WS-I-TIME-FORMAT
                   WHEN '2'
                       MOVE 'PM' TO WS-I-TIME-FORMAT
               END-EVALUATE
      
               DISPLAY ' '
               DISPLAY 'Enter Hour [HH]: ' WITH NO ADVANCING
               ACCEPT WS-I-HOUR
               DISPLAY 'Enter Minute [MM]: ' WITH NO ADVANCING
               ACCEPT WS-I-MINUTE
      
               ACCEPT WS-TIME FROM TIME
      
               EVALUATE TRUE
                   WHEN WS-HOUR = 0
                       MOVE 12 TO WS-HOUR
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR < 12
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR = 12
                       MOVE 'PM' TO WS-TIME-FORMAT
                   WHEN OTHER
                       COMPUTE WS-HOUR = WS-HOUR - 12
                       MOVE 'PM' TO WS-TIME-FORMAT
               END-EVALUATE
      
               IF WS-I-HOUR < 0 OR WS-I-HOUR > 12  THEN
                   DISPLAY 'Invalid Time: Hour'
               ELSE 
                   IF (WS-I-HOUR = WS-HOUR AND WS-I-MINUTE < 0) OR
                    WS-I-MINUTE > 59 THEN
                   DISPLAY 'Invalid Time: Minute'
                   ELSE
                       MOVE WS-I-HOUR TO WS-DA-HOUR
                       MOVE WS-I-MINUTE TO WS-DA-MINUTES
                       MOVE WS-I-TIME-FORMAT TO WS-DA-TIME-FORMAT
                       MOVE 1 TO WS-BOOL
                   END-IF
               END-IF
      
           END-PERFORM

           MOVE WS-TIME-STAMP-D-A TO WS-S-ARRIVAL-TIME

           MOVE 'sched' TO WS-S-STATUS
           PERFORM RECORD-SCHEDULE
           PERFORM SUCCESS-ADD-SCHEDULE-MESSAGE
           .

       UPDATE-SCHEDULE.
           DISPLAY '[UPDATE SCHEDULE]'
           DISPLAY ' '
           DISPLAY 'Search ID: ' WITH NO ADVANCING
           ACCEPT FS-SCHEDULE-ID


           OPEN I-O FS-SCHEDULES-FILE
               READ FS-SCHEDULES-FILE
               KEY IS FS-SCHEDULE-ID
               INVALID KEY DISPLAY 'Schedule Record Not Found'
               NOT INVALID KEY
                   DISPLAY 'Enter Month[MM] : ' WITH NO ADVANCING
                   ACCEPT WS-I-MONTH
                   DISPLAY 'Enter Day[DD] : ' WITH NO ADVANCING
                   ACCEPT WS-I-DAY
                   DISPLAY 'Enter Year[YY] : ' WITH NO ADVANCING
                   ACCEPT WS-I-YEAR


               ACCEPT WS-DATE FROM DATE




               IF WS-I-MONTH < WS-MONTH OR WS-I-MONTH > 12 THEN
                   DISPLAY 'Invalid Month'
               ELSE
                   MOVE WS-I-MONTH TO WS-MONTH-CHECKER


                   EVALUATE TRUE
                       WHEN WS-MONTHS-31
                           MOVE 31 TO WS-LIMIT-DAYS
                       WHEN WS-MONTHS-30
                           MOVE 30 TO WS-LIMIT-DAYS
                       WHEN OTHER
                           MOVE 28 TO WS-LIMIT-DAYS
                   END-EVALUATE


                   IF (WS-I-MONTH = WS-MONTH AND WS-I-DAY < WS-DAY)
                   OR WS-I-DAY > WS-LIMIT-DAYS THEN
                       DISPLAY 'Invalid Day'
                   ELSE
                       IF WS-I-YEAR NOT = WS-YEAR
                           DISPLAY 'Year'
                       ELSE
                           MOVE WS-INPUT-DATE TO WS-DA-DATE
                           DISPLAY WS-DA-DATE
                            MOVE 1 TO WS-BOOL
                       END-IF
                   END-IF
               END-IF


           MOVE 0 TO WS-BOOL


           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'TIME FORMAT'
               DISPLAY '1 - Morning (AM)'
               DISPLAY '2 - Evening / Afternoon (PM)'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-TIME-FORMAT-CHOICE


               EVALUATE WS-TIME-FORMAT-CHOICE
                   WHEN '1'
                       MOVE 'AM' TO WS-I-TIME-FORMAT
                   WHEN '2'
                       MOVE 'PM' TO WS-I-TIME-FORMAT
               END-EVALUATE


               DISPLAY ' '
               DISPLAY 'Enter Hour [HH]: ' WITH NO ADVANCING
               ACCEPT WS-I-HOUR
               DISPLAY 'Enter Minute [MM]: ' WITH NO ADVANCING
               ACCEPT WS-I-MINUTE


               ACCEPT WS-TIME FROM TIME


               EVALUATE TRUE
                   WHEN WS-HOUR = 0
                       MOVE 12 TO WS-HOUR
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR < 12
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR = 12
                       MOVE 'PM' TO WS-TIME-FORMAT
                   WHEN OTHER
                       COMPUTE WS-HOUR = WS-HOUR - 12
                       MOVE 'PM' TO WS-TIME-FORMAT
               END-EVALUATE


               IF WS-I-HOUR < 0 OR WS-I-HOUR > 23 THEN
                   DISPLAY 'Invalid Time: Hour'
               ELSE
                   IF (WS-I-HOUR = WS-HOUR AND WS-I-MINUTE < 0) OR
                    WS-I-MINUTE > 59 THEN
                   DISPLAY 'Invalid Time: Minute'
                   ELSE
                       MOVE WS-I-HOUR TO WS-DA-HOUR
                       MOVE WS-I-MINUTE TO WS-DA-MINUTES
                       MOVE 1 TO WS-BOOL
                   END-IF
               END-IF


           END-PERFORM
           DISPLAY WS-TIME-STAMP-D-A
           MOVE WS-TIME-STAMP-D-A TO WS-S-DEPARTURE-TIME


           MOVE 0 TO WS-BOOL


           DISPLAY '[ARRIVAL TIME]'
           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'Enter Month[MM] : ' WITH NO ADVANCING
               ACCEPT WS-I-MONTH
               DISPLAY 'Enter Day[DD] : ' WITH NO ADVANCING
               ACCEPT WS-I-DAY
               DISPLAY 'Enter Year[YY] : ' WITH NO ADVANCING
               ACCEPT WS-I-YEAR


               ACCEPT WS-DATE FROM DATE




               IF WS-I-MONTH < WS-MONTH OR WS-I-MONTH > 12 THEN
                   DISPLAY 'Invalid Month'
               ELSE
                   MOVE WS-I-MONTH TO WS-MONTH-CHECKER


                   EVALUATE TRUE
                       WHEN WS-MONTHS-31
                           MOVE 31 TO WS-LIMIT-DAYS
                       WHEN WS-MONTHS-30
                           MOVE 30 TO WS-LIMIT-DAYS
                       WHEN OTHER
                           MOVE 28 TO WS-LIMIT-DAYS
                   END-EVALUATE


                   IF (WS-I-MONTH = WS-MONTH AND WS-I-DAY < WS-DAY)
                   OR WS-I-DAY > WS-LIMIT-DAYS THEN
                       DISPLAY 'Invalid Day'
                   ELSE
                       IF WS-I-YEAR NOT = WS-YEAR
                           DISPLAY 'Year'
                       ELSE
                           MOVE WS-INPUT-DATE TO WS-DA-DATE
                            MOVE 1 TO WS-BOOL
                       END-IF
                   END-IF
               END-IF
           END-PERFORM


           MOVE 0 TO WS-BOOL


           PERFORM UNTIL WS-BOOL = 1
               DISPLAY 'TIME FORMAT'
               DISPLAY '1 - Morning (AM)'
               DISPLAY '2 - Evening / Afternoon (PM)'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-TIME-FORMAT-CHOICE
     
               EVALUATE WS-TIME-FORMAT-CHOICE
                   WHEN '1'
                       MOVE 'AM' TO WS-I-TIME-FORMAT
                   WHEN '2'
                       MOVE 'PM' TO WS-I-TIME-FORMAT
               END-EVALUATE
     
               DISPLAY ' '
               DISPLAY 'Enter Hour [HH]: ' WITH NO ADVANCING
               ACCEPT WS-I-HOUR
               DISPLAY 'Enter Minute [MM]: ' WITH NO ADVANCING
               ACCEPT WS-I-MINUTE
     
               ACCEPT WS-TIME FROM TIME
     
               EVALUATE TRUE
                   WHEN WS-HOUR = 0
                       MOVE 12 TO WS-HOUR
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR < 12
                       MOVE 'AM' TO WS-TIME-FORMAT
                   WHEN WS-HOUR = 12
                       MOVE 'PM' TO WS-TIME-FORMAT
                   WHEN OTHER
                       COMPUTE WS-HOUR = WS-HOUR - 12
                       MOVE 'PM' TO WS-TIME-FORMAT
               END-EVALUATE
     
               IF WS-I-HOUR < 0 OR WS-I-HOUR > 23  THEN
                   DISPLAY 'Invalid Time: Hour'
               ELSE
                   IF (WS-I-HOUR = WS-HOUR AND WS-I-MINUTE < 0) OR
                    WS-I-MINUTE > 59 THEN
                   DISPLAY 'Invalid Time: Minute'
                   ELSE
                       MOVE WS-I-HOUR TO WS-DA-HOUR
                       MOVE WS-I-MINUTE TO WS-DA-MINUTES
                       MOVE 1 TO WS-BOOL
                   END-IF
               END-IF
     
           END-PERFORM


               IF WS-I-MONTH = SPACES OR WS-I-DAY = SPACES OR
               WS-I-YEAR = SPACES OR WS-I-HOUR = SPACES OR
               WS-I-MINUTE = SPACES
                   DISPLAY 'Error : <Must fill all the fields>'
                   PERFORM ADD-VEHICLE-PAGE
               
               ELSE
                   MOVE WS-S-DEPARTURE-TIME TO FS-S-DEPARTURE-TIME
                   MOVE WS-S-ARRIVAL-TIME TO FS-S-ARRIVAL-TIME
                   REWRITE FS-SCHEDULES-RECORD
                       INVALID KEY
                               DISPLAY 'Error: Update Failed.'
                            NOT INVALID KEY
                                DISPLAY 'Update Successful.'
                       END-REWRITE
                   END-IF
               END-READ
           CLOSE FS-SCHEDULES-FILE
           ACCEPT WS-BUFFER
           .


       
       CANCEL-SCHEDULE.
           PERFORM CLEAR
           .
       
       REMOVE-SCHEDULE.
           PERFORM CLEAR
           .

       SUCCESS-ADD-SCHEDULE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*           Success: Schedule Added!              *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       TRAVERSAL-VEHICLE-RECORD.
           MOVE SPACES TO WS-EOF
           MOVE 1 TO WS-COUNTER-I
           OPEN INPUT FS-VEHICLES-FILE
           DISPLAY ' '
           DISPLAY '   VEHICLE ID      | SERIAL | TYPE | CAPACITY |    '-
           'LICENSE PLATE     | PRICE FACTOR |      CREATED      |'
           DISPLAY '---------------------------------------------------'-
           '---------------------------------------------------'
           PERFORM UNTIL WS-EOF = 'Y'    
               READ FS-VEHICLES-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END 
               DISPLAY WS-COUNTER-I '. 'FS-VEHICLE-ID ' | ' 
               FS-VEHICLE-SERIAL' | '
               FS-VEHICLE-CLASS ' | ' FS-VEHICLE-CAPACITY ' | ' 
               FS-VEHICLE-LICENSE-PLATE ' | ' FS-VEHICLE-PRICE-FACTOR 
               ' | ' FS-VEHICLE-TIME-STAMP
               DISPLAY '-----------------------------------------------'-
               '-------------------------------------------------------'
               END-READ
               ADD 1 TO WS-COUNTER-I
            END-PERFORM
           CLOSE FS-VEHICLES-FILE
           .

       TRAVERSAL-ROUTE-RECORD.
           MOVE SPACES TO WS-EOF
           MOVE 1 TO WS-COUNTER-I
           DISPLAY ' '
           DISPLAY '       ROUTE ID     |             ORIGIN           '-
           '  |          DESTINATION           |   DISTANCE   |  BASE P'-
           'RICE  |      CREATED      |'
           DISPLAY '---------------------------------------------------'-
           '-----------------------------------------------------------'-
           '---------------------------'
           OPEN INPUT FS-ROUTES-FILE 
           PERFORM UNTIL WS-EOF = 'Y'   
               READ FS-ROUTES-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END 
               DISPLAY WS-COUNTER-I '. ' FS-ROUTE-ID ' | ' 
               FS-ROUTE-ORIGIN ' | ' 
               FS-ROUTE-DESTINATION ' | ' FS-ROUTE-DISTANCE ' | '
               FS-ROUTE-BASE-PRICE ' | ' FS-ROUTE-TIME-STAMP
               DISPLAY '-----------------------------------------------'-
               '-------------------------------------------------------'-
               '-----------------------------------'
               END-READ
               ADD 1 TO WS-COUNTER-I
            END-PERFORM
           CLOSE FS-ROUTES-FILE
           .

       TRAVERSAL-SCHEDULE.
           MOVE SPACES TO WS-EOF
           MOVE 1 TO WS-COUNTER-I
           DISPLAY ' '
           DISPLAY '        SCHEDULE ID   |                            '-
           '  ROUTE                               | VEHICLE SERIAL |   '-
           'DEPARTURE TIME    |     ARRIVAL TIME    | STATUS |       CR'-
           'EATED       |'
           DISPLAY '---------------------------------------------------'-
           '-----------------------------------------------------------'-
           '-----------------------------------------------------------'-
           '-------------'
           OPEN INPUT FS-SCHEDULES-FILE
           PERFORM UNTIL WS-EOF = 'Y'
               READ FS-SCHEDULES-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
               MOVE FS-FK-ROUTE-ID TO FS-ROUTE-ID
               MOVE FS-FK-VEHICLE-ID TO FS-VEHICLE-ID
               OPEN INPUT FS-ROUTES-FILE
                   READ FS-ROUTES-FILE
                   END-READ
               CLOSE FS-ROUTES-FILE
               OPEN INPUT FS-VEHICLES-FILE
                   READ FS-VEHICLES-FILE
                   END-READ
               CLOSE FS-VEHICLES-FILE
               DISPLAY WS-COUNTER-I '. 'FS-SCHEDULE-ID ' | ' 
               FS-ROUTE-ORIGIN ' TO ' 
               FS-ROUTE-DESTINATION ' | ' FS-VEHICLE-SERIAL ' | ' 
               FS-S-DEPARTURE-TIME ' | ' FS-S-ARRIVAL-TIME ' | ' 
               FS-S-STATUS ' | ' FS-S-TIME-STAMP
               DISPLAY '-----------------------------------------------'-
               '-------------------------------------------------------'-
               '-------------------------------------------------------'-
               '-------------------------'
               ADD 1 TO WS-COUNTER-I
           END-PERFORM
           CLOSE FS-SCHEDULES-FILE
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
           
           MOVE WS-GENERATED-ID TO WS-ROUTE-ID

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
           
           MOVE WS-GENERATED-ID TO WS-VEHICLE-ID

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
           
           MOVE WS-GENERATED-ID TO WS-SCHEDULE-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-TIME-STAMP TO WS-S-TIME-STAMP
       
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
           MOVE WS-HOUR TO WS-TS-HOUR
           MOVE WS-MINUTE TO WS-TS-MINUTES
           MOVE WS-SECOND TO WS-TS-SECOND
           .
       
       REPEAT-STATEMENT.
           DISPLAY "Do you want to repeat? YES/NO: " WITH NO ADVANCING
           ACCEPT WS-REPEAT

           MOVE FUNCTION UPPER-CASE(WS-REPEAT) TO WS-REPEAT


       .

       