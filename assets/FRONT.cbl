       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRONT.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-MAINPAGE-CHOICE PIC 99.
       01  WS-USER-MAIN-PAGE-CHOICE PIC 99.
       01  WS-ADMIN-MAIN-PAGE-CHOICE pic 99.
       01  WS-USERNAME PIC X(30).
       01  WS-PASSWORD PIC X(16).
       01  WS-CONFIRM-PASSWORD PIC X(16).
       01  WS-USERPAGE-CHOICE PIC 99.
       01  WS-ADMINPAGE-CHOICE PIC 99.
       01  WS-ROUTE-KEY PIC 99.
       01  WS-BUS-TYPE-KEY PIC 99.
       01  WS-SCHEDULE-KEY PIC 99.
       01  WS-PASSENGER-TYPE-KEY PIC 99.
       01  WS-SEAT-NUMBER PIC 99.
       01  WS-AVAIL-CHOICE PIC X(3).
       01  WS-NAME PIC X(30).
       01  WS-EMAIL PIC X(30).
       01  WS-CANCEL-CHOICE PIC X(3).
       01  WS-UPDATE-KEY PIC 99.
       01  WS-UPDATE-SCHEDULE PIC X(15).
       01  WS-UPDATE-FARE PIC 99V99.
       01  WS-GENERATE-CHOICE PIC X(3).
       
       PROCEDURE DIVISION.
      
           PERFORM MAIN-PAGE.

       STOP RUN.

       MAIN-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*                 By: Tech4Ward                   *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*                 [1] User                        *"
           DISPLAY "*                 [2] Admin                       *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-MAINPAGE-CHOICE

           EVALUATE WS-MAINPAGE-CHOICE
               
               WHEN 1 PERFORM USER-MAIN-PAGE
               WHEN 2 PERFORM ADMIN-MAIN-PAGE
               WHEN OTHER 
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM MAIN-PAGE
           
           ACCEPT OMITTED.

       USER-MAIN-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*               Welcome, Passenger!               *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*                [1] Login                        *"
           DISPLAY "*                [2] Sign Up                      *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-USER-MAIN-PAGE-CHOICE

           EVALUATE WS-USER-MAIN-PAGE-CHOICE
               
               WHEN 1 PERFORM USER-LOGIN-PAGE
               WHEN 2 PERFORM USER-SIGNUP-PAGE
               WHEN OTHER 
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM USER-MAIN-PAGE
           
           ACCEPT OMITTED.

       ADMIN-MAIN-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*                Welcome, Admin!                  *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*                [1] Login                        *"
           DISPLAY "*                [2] Sign Up                      *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-ADMIN-MAIN-PAGE-CHOICE

           EVALUATE WS-ADMIN-MAIN-PAGE-CHOICE
               
               WHEN 1 PERFORM ADMIN-LOGIN-PAGE
               WHEN 2 PERFORM ADMIN-SIGNUP-PAGE
               WHEN OTHER 
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM ADMIN-MAIN-PAGE
           
           ACCEPT OMITTED.

       USER-LOGIN-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*               Login Page - User                 *"
           DISPLAY "***************************************************"
         
           DISPLAY " Enter your username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY " Enter your password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           
      *    INSERT BACKEND LOGIC HERE FOR INFO CHECKING IN DATA BASE    
      *        PERFORM INVALID-ACCOUNT-MESSAGE
           
           PERFORM SUCCESS-LOGIN-MESSAGE
           PERFORM USER-PAGE
           
           ACCEPT OMITTED.

       USER-SIGNUP-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*              Sign Up Page - User                *"
           DISPLAY "***************************************************"
           
           DISPLAY " Enter your username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME

      *    INSERT BACKEND LOGIC HERE FOR USERNAME CHECKING IN DATA BASE
      *        PERFORM USERNAME-TAKEN-MESSAGE

           DISPLAY " Enter your password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           DISPLAY " Confirm your password: " WITH NO ADVANCING
           ACCEPT WS-CONFIRM-PASSWORD

           IF WS-PASSWORD = WS-CONFIRM-PASSWORD
               PERFORM SUCCESS-ACCOUNT-MESSAGE
               PERFORM MAIN-PAGE
           ELSE
               PERFORM PASSWORD-MISMATCH-MESSAGE
               PERFORM USER-SIGNUP-PAGE
       
           ACCEPT OMITTED.

       ADMIN-LOGIN-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*              Login Page - Admin                 *"
           DISPLAY "***************************************************"
         
           DISPLAY " Enter your username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY " Enter your password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           
      *    INSERT BACKEND LOGIC HERE FOR INFO CHECKING IN DATA BASE    
      *        PERFORM INVALID-ACCOUNT-MESSAGE
           
           PERFORM SUCCESS-LOGIN-MESSAGE
           PERFORM ADMIN-PAGE
           
           ACCEPT OMITTED.

       ADMIN-SIGNUP-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*             Sign Up Page - Admin                *"
           DISPLAY "***************************************************"
           
           DISPLAY " Enter your username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME

      *    INSERT BACKEND LOGIC HERE FOR USERNAME CHECKING IN DATA BASE
      *        PERFORM USERNAME-TAKEN-MESSAGE

           DISPLAY " Enter your password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           DISPLAY " Confirm your password: " WITH NO ADVANCING
           ACCEPT WS-CONFIRM-PASSWORD

           IF WS-PASSWORD = WS-CONFIRM-PASSWORD
               PERFORM SUCCESS-ACCOUNT-MESSAGE
               PERFORM MAIN-PAGE
           ELSE
               PERFORM PASSWORD-MISMATCH-MESSAGE
               PERFORM ADMIN-SIGNUP-PAGE
       
           ACCEPT OMITTED.

       INVALID-INPUT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*            Invalid Input. Try Again!            *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

      *INVALID-ACCOUNT-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*     Invalid Username and Password. Try Again!   *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."

      *    ACCEPT OMITTED.
       
       SUCCESS-LOGIN-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*               Login Successful!                 *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

      *USERNAME-TAKEN-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*      Username is already taken. Try Again!      *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."

      *    ACCEPT OMITTED.

       PASSWORD-MISMATCH-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*        Password do not match. Try Again!        *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

       SUCCESS-ACCOUNT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*          Account Created Successfully!          *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

       USER-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*               Welcome, Passenger!               *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*            [1] Book Ticket                      *"
           DISPLAY "*            [2] Cancel Reservation               *"
           DISPLAY "*            [3] Logout                           *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-USERPAGE-CHOICE

           EVALUATE WS-USERPAGE-CHOICE
               
               WHEN 1 PERFORM BOOK-TICKET-PAGE
               WHEN 2 PERFORM CANCEL-RESERVATION-PAGE
               WHEN 3 PERFORM MAIN-PAGE
               WHEN OTHER PERFORM INVALID-INPUT-MESSAGE

           ACCEPT OMITTED.

       ADMIN-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*                 Welcome, Admin!                 *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*            [1] Update Configuration             *"
           DISPLAY "*            [2] Generate Report                  *"
           DISPLAY "*            [3] Logout                           *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-ADMINPAGE-CHOICE

           EVALUATE WS-ADMINPAGE-CHOICE
               
               WHEN 1 PERFORM UPDATE-CONFIGURATION-PAGE
               WHEN 2 PERFORM GENERATE-REPORT-PAGE
               WHEN 3 PERFORM MAIN-PAGE
               WHEN OTHER PERFORM INVALID-INPUT-MESSAGE

           ACCEPT OMITTED.

       BOOK-TICKET-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*                 Book Ticket Page                *"
           DISPLAY "***************************************************"
           DISPLAY "*       EDSA Bus Carousel southbound bus route    *" 
           DISPLAY "*          (Monumento to PITX) has 6 stops        *"
           DISPLAY "***************************************************"
           DISPLAY "*            ROUTES               |      FARE     *"
           DISPLAY "***************************************************"
           DISPLAY "* [1] MONUMENTO - PITX            |    P 75.50    *"
           DISPLAY "* [2] MONUMENTO - AYALA MALL      |    P 71.75    *"
           DISPLAY "* [3] MONUMENTO - DFA             |    P 68.00    *"
           DISPLAY "* [4] MONUMENTO - MOA             |    P 64.25    *"
           DISPLAY "* [5] MONUMENTO - ROXAS BOULEVARD |    P 62.00    *"
           DISPLAY "* [6] MONUMENTO - TAFT AVENUE     |    P 59.50    *"
           DISPLAY "***************************************************"
           DISPLAY "*                    Bus Type                     *"
           DISPLAY "***************************************************"
           DISPLAY "*      [1] STANDARD     =    FLAT RATE            *"
           DISPLAY "*      [2] FIRST CLASS  =    FLAT RATE + 100      *"
           DISPLAY "*      [3] DELUXE       =    FLAT RATE + 500      *"
           DISPLAY "***************************************************"
           DISPLAY "*                    Discounts                    *"
           DISPLAY "***************************************************"
           DISPLAY "*          STUDENT      =    10% OFF              *"
           DISPLAY "*          SENIOR       =    12% OFF              *"
           DISPLAY "*          PWD          =    15% OFF              *"
           DISPLAY "***************************************************"
           DISPLAY "*                    Schedules                    *"
           DISPLAY "***************************************************"
           DISPLAY "*       [1] MORNING     =    8:00 AM              *"
           DISPLAY "*       [2] AFTERNOON   =    12:30 PM             *"
           DISPLAY "*       [3] NIGHT       =    6:00 PM              *"
           DISPLAY "***************************************************"
           
           DISPLAY " Enter route key/number: " WITH NO ADVANCING
           ACCEPT WS-ROUTE-KEY
           
           IF WS-ROUTE-KEY NOT = 1 AND 2 AND 3 AND 4 AND 5 AND 6 
               PERFORM INVALID-INPUT-MESSAGE
               PERFORM BOOK-TICKET-PAGE
           ELSE
               DISPLAY " Enter bus type: " WITH NO ADVANCING
               ACCEPT WS-BUS-TYPE-KEY
               IF WS-BUS-TYPE-KEY NOT = 1 AND 2 AND 3 
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM BOOK-TICKET-PAGE
               ELSE
                   DISPLAY " Enter prefer schedule key: " 
                   WITH NO ADVANCING
                   ACCEPT WS-SCHEDULE-KEY
                   IF WS-SCHEDULE-KEY NOT = 1 AND 2 AND 3 
                       PERFORM INVALID-INPUT-MESSAGE
                       PERFORM BOOK-TICKET-PAGE
                   ELSE
                       DISPLAY "Checking available seats..."    

      *    INSERT BACKEND LOGIC HERE FOR CHECKING SEATS IN DATA BASE
      *        PERFORM NO-AVAILABLE-SEATS-MESSAGE

                       PERFORM AVAILABLE-SEATS-PAGE

           ACCEPT OMITTED.

      *NOAVAILABLE-SEATS-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*         No available seat/s. Try Again!         *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."

      *    ACCEPT OMITTED.
       
       AVAILABLE-SEATS-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*                  Bus Seats Page                 *"
           DISPLAY "***************************************************"
           DISPLAY "**************                       **************"         
           DISPLAY "*   Driver   *                       *  Entrance  *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  1  *   2  *                       *  3   *  4  *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  5  *   6  *                       *  7   *  8  *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  9  *   10 *                       *  11  *  12 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  13 *   14 *                       *  15  *  16 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  17 *   18 *                       *  19  *  20 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  21 *   22 *                       *  23  *  24 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  25 *   26 *                       *  27  *  28 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  29 *   30 *                       *  31  *  32 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**************                       **************"         
           DISPLAY "*  33 *   34 *                       *  35  *  36 *"    
           DISPLAY "**************                       **************"
           DISPLAY "**********************       **********************"         
           DISPLAY "*  37 *   38 *   39  *       *   40  *  41  *  42 *"    
           DISPLAY "**********************       **********************"               
           DISPLAY "***************************************************"
           DISPLAY "*         Passenger Type (For Discounts)          *"
           DISPLAY "***************************************************"
           DISPLAY "*        [1] REGULAR    =    FLAT RATE            *"
           DISPLAY "*        [2] STUDENT    =    10% OFF              *"
           DISPLAY "*        [3] SENIOR     =    12% OFF              *"
           DISPLAY "*        [4] PWD        =    15% OFF              *"
           DISPLAY "***************************************************"
           
           DISPLAY " SHOW AVAILABLE SEATS: - "
      *    INSERT BACKEND LOGIC HERE FOR DISPLAYING AVAILABLE SEATS

           DISPLAY " Enter passenger type key: " WITH NO ADVANCING
           ACCEPT WS-PASSENGER-TYPE-KEY
           
           IF WS-PASSENGER-TYPE-KEY NOT = 1 AND 2 AND 3 AND 4
               PERFORM INVALID-INPUT-MESSAGE
               PERFORM AVAILABLE-SEATS-PAGE
           ELSE
               DISPLAY " Enter your seat number: " WITH NO ADVANCING
               ACCEPT WS-SEAT-NUMBER

               IF WS-SEAT-NUMBER < 1 OR WS-SEAT-NUMBER > 42
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM AVAILABLE-SEATS-PAGE    
               ELSE

      *    INSERT BACKEND LOGIC HERE FOR CHECKING SEATS IN DATA BASE
      *        PERFORM SEAT-TAKEN-MESSAGE

                   PERFORM SUCCESS-SEAT-MESSAGE
                   PERFORM AVAIL-SEAT-QUESTION

           ACCEPT OMITTED.

       AVAIL-SEAT-QUESTION.
           CALL "SYSTEM" USING "clear"
           DISPLAY "Do you want to avail another seat? (YES/NO): "
           WITH NO ADVANCING
           ACCEPT WS-AVAIL-CHOICE

           IF FUNCTION UPPER-CASE(WS-AVAIL-CHOICE) = "YES"
               PERFORM AVAILABLE-SEATS-PAGE
           ELSE
               IF FUNCTION UPPER-CASE(WS-AVAIL-CHOICE) = "NO"
                   PERFORM CONFIRMATION-PAGE
               ELSE
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM AVAIL-SEAT-QUESTION
           
           ACCEPT OMITTED.

      *SEAT-TAKEN-MESSAGE.
      *    DISPLAY "***************************************************"
      *    DISPLAY "*     Seat number is already taken. Try Again!    *"
      *    DISPLAY "***************************************************"
      *    DISPLAY " Press 'enter' key to continue..."

      *    ACCEPT OMITTED.

       SUCCESS-SEAT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*            Seat Added Successfully!             *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

       CONFIRMATION-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*                Confirmation Page                *"
           DISPLAY "***************************************************"
           
      *    INSERT BACKEND LOGIC HERE FOR DISPLAY FROM DATA BASE
      
           DISPLAY " Enter your name for identification: "
           WITH NO ADVANCING
           ACCEPT WS-NAME

           DISPLAY " Enter your email for confirmation: "
           WITH NO ADVANCING
           ACCEPT WS-EMAIL

           PERFORM SUCCESS-BOOKING-MESSAGE
           PERFORM USER-PAGE

           ACCEPT OMITTED.

       SUCCESS-BOOKING-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*               Booking Successful!               *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.
       
       CANCEL-RESERVATION-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*              Cancel Reservation Page            *"
           DISPLAY "***************************************************"
           
      *    INSERT BACKEND LOGIC HERE FOR DISPLAY FROM DATA BASE
      
           DISPLAY " Do you want to cancel your booking? (YES/NO): "
           WITH NO ADVANCING
           ACCEPT WS-CANCEL-CHOICE

           IF FUNCTION UPPER-CASE(WS-CANCEL-CHOICE) = "YES"
               DISPLAY " Enter your email for confirmation: "
               WITH NO ADVANCING
               ACCEPT WS-EMAIL

               DISPLAY " Terminating the reservation..."
               PERFORM CANCEL-RESERVATION-MESSAGE
               PERFORM USER-PAGE
           ELSE
               IF FUNCTION UPPER-CASE(WS-CANCEL-CHOICE) = "NO"
                   PERFORM USER-PAGE
               ELSE
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM CANCEL-RESERVATION-PAGE

           ACCEPT OMITTED.

       CANCEL-RESERVATION-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*       Your reservation has been canceled!       *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

       UPDATE-CONFIGURATION-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Update Configuration Page            *"
           DISPLAY "***************************************************"
           
      *    INSERT BACKEND LOGIC HERE FOR DISPLAY FROM DATA BASE
      
           DISPLAY " Enter key to change: " WITH NO ADVANCING
           ACCEPT WS-UPDATE-KEY

           DISPLAY " Enter new schedule: " WITH NO ADVANCING
           ACCEPT WS-UPDATE-SCHEDULE

           DISPLAY " Enter new fare: " WITH NO ADVANCING
           ACCEPT WS-UPDATE-FARE

      *    INSERT BACKEND LOGIC HERE TO SAVE UPDATES IN THE DATABASE

           PERFORM SUCCESS-UPDATE-MESSAGE
           PERFORM ADMIN-PAGE

           ACCEPT OMITTED.

       SUCCESS-UPDATE-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*               Updated Successfully!             *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.

       GENERATE-REPORT-PAGE.
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*              Generate Report Page               *"
           DISPLAY "***************************************************"
           
      *    INSERT BACKEND LOGIC HERE FOR DISPLAY FROM DATA BASE
      
           DISPLAY " Do you want a copy of the report? (YES/NO): "
           WITH NO ADVANCING
           ACCEPT WS-GENERATE-CHOICE

           IF FUNCTION UPPER-CASE(WS-GENERATE-CHOICE) = "YES"
               DISPLAY " Enter your email: "
               WITH NO ADVANCING
               ACCEPT WS-EMAIL

               PERFORM SUCCESS-REPORT-MESSAGE
               PERFORM ADMIN-PAGE
           ELSE
               IF FUNCTION UPPER-CASE(WS-GENERATE-CHOICE) = "NO"
                   PERFORM ADMIN-PAGE
               ELSE
                   PERFORM INVALID-INPUT-MESSAGE
                   PERFORM CANCEL-RESERVATION-PAGE

           ACCEPT OMITTED.

       SUCCESS-REPORT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*        The report was successfully sent!        *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT OMITTED.
           
 

       

       