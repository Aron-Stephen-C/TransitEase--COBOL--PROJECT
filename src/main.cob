       IDENTIFICATION DIVISION.
       PROGRAM-ID. TICKET-SYSTEM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SCHEDULE-FILE ASSIGN TO DISK-SCHEDULE.
           SELECT RESERVATION-FILE ASSIGN TO DISK-RESERVATION.

       DATA DIVISION.
       FILE SECTION.
       FD SCHEDULE-FILE.
       01 SCHEDULE-RECORD.
           05 ROUTE-NUMBER      PIC X(10).
           05 ROUTE-NAME        PIC X(50).
           05 DEPARTURE-TIME    PIC X(5).
           05 ARRIVAL-TIME      PIC X(5).
           05 FARE              PIC 9(5)V99.

       FD RESERVATION-FILE.
       01 RESERVATION-RECORD.
           05 TICKET-ID         PIC X(10).
           05 PASSENGER-NAME    PIC X(50).
           05 ROUTE-NUMBER      PIC X(10).
           05 SEAT-NUMBER       PIC 9(3).
           05 TICKET-STATUS     PIC X(10).

       WORKING-STORAGE SECTION.
       01 USER-CHOICE          PIC 9(1).
       01 RETURN-CODE          PIC 9.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM DISPLAY-MENU UNTIL USER-CHOICE = 9.
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "1. Manage Schedules".
           DISPLAY "2. Make a Reservation".
           DISPLAY "3. Cancel a Reservation".
           DISPLAY "4. Generate Reports".
           DISPLAY "9. Exit".
           ACCEPT USER-CHOICE.
           EVALUATE USER-CHOICE
               WHEN 1 PERFORM MANAGE-SCHEDULES
               WHEN 2 PERFORM RESERVATION-MODULE
               WHEN 3 PERFORM CANCEL-RESERVATION
               WHEN 4 PERFORM GENERATE-REPORTS
               WHEN 9 DISPLAY "Exiting System..."
               WHEN OTHER DISPLAY "Invalid Option. Try Again."
           END-EVALUATE.

       MANAGE-SCHEDULES.
           DISPLAY "Enter Route Number: ".
           ACCEPT ROUTE-NUMBER.
           DISPLAY "Enter Route Name: ".
           ACCEPT ROUTE-NAME.
           DISPLAY "Enter Departure Time (HH:MM): ".
           ACCEPT DEPARTURE-TIME.
           DISPLAY "Enter Arrival Time (HH:MM): ".
           ACCEPT ARRIVAL-TIME.
           DISPLAY "Enter Fare: ".
           ACCEPT FARE.
           WRITE SCHEDULE-RECORD.
               DISPLAY "Schedule Added Successfully!".

       RESERVATION-MODULE.
           DISPLAY "Enter Passenger Name: ".
           ACCEPT PASSENGER-NAME.
           DISPLAY "Enter Route Number: ".
           ACCEPT ROUTE-NUMBER.
           DISPLAY "Enter Seat Number: ".
           ACCEPT SEAT-NUMBER.
           MOVE "CONFIRMED" TO TICKET-STATUS.
           GENERATE-TICKET-ID.
           WRITE RESERVATION-RECORD.
           DISPLAY "Ticket Reserved Successfully!".
       
       GENERATE-REPORTS.
           OPEN INPUT RESERVATION-FILE.
           DISPLAY 
           "TICKET-ID PASSENGER-NAME ROUTE-NUMBER SEAT-NUMBER STATUS".
           PERFORM UNTIL EOF
               READ RESERVATION-FILE INTO RESERVATION-RECORD
                    AT END MOVE 1 TO EOF
                    NOT AT END DISPLAY 
                    TICKET-ID PASSENGER-NAME ROUTE-NUMBER SEAT-NUMBER TICKET-STATUS
           END-PERFORM.
           CLOSE RESERVATION-FILE.

