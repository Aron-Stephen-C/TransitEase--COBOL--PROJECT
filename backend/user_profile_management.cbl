      * This is the module that Manages User records
       IDENTIFICATION DIVISION.
       PROGRAM-ID. user_profile_management.
       
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FS-CURRENT-USER-FILE ASSIGN 
           TO 'data/artifact/current_user.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.


           SELECT FS-PASSENGER-FILE ASSIGN TO 'data/passenger_file.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-P-USER-ID
           FILE STATUS IS WS-FILE-STATUS.
       
           SELECT FS-ADMIN-FILE ASSIGN TO 'data/admin_file.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-A-USER-ID
           FILE STATUS IS WS-FILE-STATUS.

           SELECT FS-HASHED-PASSWORD-FILE ASSIGN TO 
           'data/hashpassword.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FS-OTP-FILE ASSIGN TO 'data/otp.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FS-CURRENT-USER-FILE.
       01  FS-CURRENT-USER    PIC X(15).

       FD  FS-HASHED-PASSWORD-FILE.
       01  FS-HASHED-PASSWORD    PIC X(64).

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
       FD  FS-ADMIN-FILE.
       01  FS-ADMIN-RECORD.
           02    FS-A-USER-ID    PIC X(15).
           02    FS-A-FIRST-NAME    PIC X(50).
           02    FS-A-LAST-NAME    PIC X(50).
           02    FS-A-EMAIL    PIC X(100).
           02    FS-A-PASSWORD    PIC X(64).
           02    FS-A-PHONE-NUMBER    PIC X(11).
           02    FS-A-ROLE    PIC X.
           02    FS-A-TIME-STAMP.
               03    FS-A-DATE    PIC 99/99/99.
               03    FS-A-FILLER-SPACE    PIC X(3).
               03    FS-A-TIME.
                   04    FS-A-HOUR    PIC 99.
                   04    FS-A-COLON-1    PIC X.
                   04    FS-A-MINUTES    PIC 99.
                   04    FS-A-COLON-2    PIC X.
                   04    FS-A-SECOND    PIC 99.

       FD  FS-OTP-FILE.
       01  FS-OTP    PIC X(6).
       
       WORKING-STORAGE SECTION.
       01  WS-DATE    PIC 9(6).
       01  WS-TIME    PIC 9(8).
       01  WS-FILE-STATUS    PIC XX.
       01  WS-GENERATED-USER-ID.
           02    WS-GSI-DATE    PIC 9(6).
           02    WS-GSI-TIME    PIC 9(6).
           02    WS-GSI-INCREMENT-VALUE    PIC 9(3).
       01  WS-EOF    PIC X.
       01  WS-LAST-GENERATED-ID.
           02    WS-LGSI-DATE    PIC 9(6).
           02    WS-LGSI-TIME    PIC 9(6).
           02    WS-L-INCREMENT-VALUE    PIC 9(3).
           
       01  WS-INCREMENT-VALUE PIC 9(3).
       01  WS-COMMAND             PIC X(255).
       01  WS-RETURN-CODE         PIC 9(4).
       01  WS-HASHED-PASSWORD    PIC X(64).

       01  WS-USER-RECORD.
           02    WS-USER-ID    PIC X(15).
           02    WS-FIRST-NAME    PIC X(50).
           02    WS-LAST-NAME    PIC X(50).
           02    WS-EMAIL    PIC X(100).
           02    WS-PASSWORD    PIC X(64).
           02    WS-PHONE-NUMBER    PIC X(11).
           02    WS-ROLE    PIC X.
           02    WS-TIME-STAMP.
               03    WS-TS-DATE    PIC 99/99/99.
               03    WS-TS-FILLER-SPACE    PIC X(3) VALUE SPACES.
               03    WS-TS-TIME.
                   04    WS-TS-HOUR    PIC 99.
                   04    WS-TS-COLON-1    PIC X VALUE ':'.
                   04    WS-TS-MINUTES    PIC 99.
                   04    WS-TS-COLON-2    PIC X VALUE ':'.
                   04    WS-TS-SECOND    PIC 99.

       01  WS-MAIN-CHOICE PIC X.
       01  WS-USER-MAIN-PAGE-CHOICE PIC X.
       01  WS-ADMIN-MAIN-PAGE-CHOICE PIC X.
       01  WS-USER-CHOICE PIC X.
       01  WS-ADMIN-CHOICE PIC X.
       01  WS-BUFFER    PIC X.
       01  WS-BOOL    PIC 9.
       01  WS-OTP    PIC X(6).
       01  WS-RETURN-MAINPAGE PIC X(3).
       01  WS-CONFIRM-PASSWORD    PIC X(64).
       01  WS-REENTER-CHOICE     PIC X(3) VALUE 'N'.
       
       PROCEDURE DIVISION.
           PERFORM CHECK-FILE-STATUS
           
           PERFORM MAIN-PAGE

           GOBACK
           STOP RUN.

       MAIN-PAGE.
           MOVE SPACES TO WS-MAIN-CHOICE
           PERFORM UNTIL WS-MAIN-CHOICE = '3'
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*                 By: Tech4Ward                   *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*                 [1] Passenger                   *"
           DISPLAY "*                 [2] Admin                       *"
           DISPLAY "*                 [3] Quit                        *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-MAIN-CHOICE

           EVALUATE WS-MAIN-CHOICE              
               WHEN '1' 
                   PERFORM PASSENGER-MAIN-PAGE
               WHEN '2' 
                   PERFORM ADMIN-MAIN-PAGE
               WHEN '3' 
                   STOP RUN
               WHEN OTHER 
                   PERFORM CLEAR
                   PERFORM INVALID-INPUT-MESSAGE
           END-EVALUATE
           END-PERFORM
           .

       PASSENGER-MAIN-PAGE.
           MOVE SPACES TO WS-USER-CHOICE 
           PERFORM UNTIL WS-USER-CHOICE = '3'
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*               Welcome, Passenger!               *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*                [1] Login                        *"
           DISPLAY "*                [2] Sign Up                      *"
           DISPLAY "*                [3] Go Back                      *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-USER-MAIN-PAGE-CHOICE

           EVALUATE WS-USER-MAIN-PAGE-CHOICE
               
               WHEN '1' 
                   PERFORM PASSENGER-LOGIN-PAGE
               WHEN '2' 
                   PERFORM PASSENGER-SIGNUP-PAGE
               WHEN '3' 
                   PERFORM MAIN-PAGE
               WHEN OTHER 
                   PERFORM CLEAR
                   PERFORM INVALID-INPUT-MESSAGE
           END-EVALUATE
           END-PERFORM
           ACCEPT WS-BUFFER
           .

       PASSENGER-LOGIN-PAGE.
           PERFORM CLEAR
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-BOOL
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*            Login Page - Passenger               *"
           DISPLAY "***************************************************"
         
           DISPLAY " Enter your email: " WITH NO ADVANCING
           ACCEPT WS-EMAIL
           DISPLAY " Enter your password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD

           MOVE FUNCTION LOWER-CASE(WS-EMAIL) TO WS-EMAIL

           PERFORM HASH-PASSWORD

           OPEN INPUT FS-PASSENGER-FILE
               READ FS-PASSENGER-FILE NEXT RECORD
                   AT END CONTINUE
                   NOT AT END 
                       PERFORM UNTIL WS-EOF = 'Y'
                           IF FS-P-EMAIL = WS-EMAIL AND FS-P-PASSWORD 
                               = WS-HASHED-PASSWORD THEN    
                               MOVE 1 TO WS-BOOL
                               MOVE 'Y' TO WS-EOF
                           END-IF
                           READ FS-PASSENGER-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           END-READ
                       END-PERFORM
               END-READ
               
           CLOSE FS-PASSENGER-FILE

           MOVE FS-P-USER-ID TO FS-CURRENT-USER

           OPEN OUTPUT FS-CURRENT-USER-FILE
               WRITE FS-CURRENT-USER
               END-WRITE
           CLOSE FS-CURRENT-USER-FILE

           IF WS-BOOL = 1 THEN
               PERFORM SUCCESS-LOGIN-MESSAGE
           ELSE 
               PERFORM CLEAR
               PERFORM INVALID-ACCOUNT-MESSAGE
               DISPLAY'Do you want to login again? [YES/NO]: ' WITH NO 
               ADVANCING
               ACCEPT WS-REENTER-CHOICE

               MOVE FUNCTION UPPER-CASE(WS-REENTER-CHOICE) TO 
               WS-REENTER-CHOICE

               EVALUATE WS-REENTER-CHOICE
                   WHEN 'YES'
                       PERFORM PASSENGER-LOGIN-PAGE
                   WHEN 'NO'
                       PERFORM MAIN-PAGE
                   WHEN OTHER
                       DISPLAY'Your choice is invalid'
                   END-EVALUATE
           END-IF.

       PASSENGER-SIGNUP-PAGE.
           PERFORM CLEAR
           MOVE 1 TO WS-BOOL
           MOVE 'p' TO WS-ROLE
           MOVE SPACES TO WS-EOF    
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*            Sign Up Page - Passenger             *"
           DISPLAY "***************************************************"
           
           DISPLAY " Enter first name: " WITH NO ADVANCING
           ACCEPT WS-FIRST-NAME
           DISPLAY " Enter last name: " WITH NO ADVANCING
           ACCEPT WS-LAST-NAME
           DISPLAY " Enter your phone number: " WITH NO ADVANCING
           ACCEPT WS-PHONE-NUMBER
           DISPLAY " Enter your email: " WITH NO ADVANCING
           ACCEPT WS-EMAIL


           MOVE FUNCTION LOWER-CASE(WS-EMAIL) TO WS-EMAIL
           
           OPEN INPUT FS-PASSENGER-FILE
               READ FS-PASSENGER-FILE NEXT RECORD
                   AT END CONTINUE
                   NOT AT END
                   PERFORM UNTIL WS-EOF = 'Y'
                       IF WS-EMAIL = FS-P-EMAIL
                           MOVE 0 TO WS-BOOL
                       END-IF
                       READ FS-PASSENGER-FILE NEXT RECORD 
                       AT END MOVE 'Y' TO WS-EOF
                       END-READ
                   END-PERFORM
               END-READ
           CLOSE FS-PASSENGER-FILE

           IF WS-BOOL = 0 THEN
               PERFORM EMAIL-TAKEN-MESSAGE
               DISPLAY'Do you want to sign up again? [YES/NO]: ' WITH NO 
               ADVANCING
               ACCEPT WS-REENTER-CHOICE

               MOVE FUNCTION UPPER-CASE(WS-REENTER-CHOICE) TO 
               WS-REENTER-CHOICE

               EVALUATE WS-REENTER-CHOICE
                   WHEN 'YES'
                       PERFORM PASSENGER-SIGNUP-PAGE
                   WHEN 'NO'
                       PERFORM PASSENGER-MAIN-PAGE
                   WHEN OTHER
                       DISPLAY'Your choice is invalid'
                   END-EVALUATE
           END-IF

           STRING "python3 backend/python_script_for_email.py " WS-EMAIL
           DELIMITED BY SIZE INTO WS-COMMAND

           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE

           IF WS-RETURN-CODE = 0 
               PERFORM CLEAR
               PERFORM USER-SUCCESS-OTP-MESSAGE
               OPEN INPUT FS-OTP-FILE
                   READ FS-OTP-FILE INTO FS-OTP
                   END-READ
               CLOSE FS-OTP-FILE
               DISPLAY " Enter OTP: " WITH NO ADVANCING
               ACCEPT WS-OTP

               IF WS-OTP = FS-OTP
               PERFORM CLEAR
               PERFORM CORRECT-OTP-MESSAGE
               DISPLAY " Enter your password: " WITH NO ADVANCING
               ACCEPT WS-PASSWORD
               DISPLAY " Confirm your password: " WITH NO ADVANCING
               ACCEPT WS-CONFIRM-PASSWORD

                   IF WS-PASSWORD = WS-CONFIRM-PASSWORD
                       PERFORM SUCCESS-ACCOUNT-MESSAGE
                       PERFORM RECORD-PASSENGER
                       PERFORM MAIN-PAGE
                   ELSE
                       PERFORM PASSWORD-MISMATCH-MESSAGE
           
                        PERFORM UNTIL WS-RETURN-MAINPAGE = 'NO'
                            PERFORM RETURN-TO-MAINPAGE

                            EVALUATE WS-RETURN-MAINPAGE
                                WHEN 'YES'
                                PERFORM PASSENGER-MAIN-PAGE
                            WHEN 'NO'
                                    PERFORM PASSENGER-SIGNUP-PAGE
                            WHEN OTHER
                                    DISPLAY ' '
                                    DISPLAY 'Invalid Input'
                            END-EVALUATE
                        END-PERFORM
               END-IF
               ELSE
                   PERFORM INCORRECT-OTP-MESSAGE
           END-IF
               
           ELSE
               PERFORM FAILED-OTP-MESSAGE

           END-IF.

       ADMIN-MAIN-PAGE.
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*                Welcome, Admin!                  *"
           DISPLAY "***************************************************"
           DISPLAY "*                                                 *"
           DISPLAY "*                [1] Login                        *"
           DISPLAY "*                [2] Sign Up                      *"
           DISPLAY "*                [3] Go Back                      *"
           DISPLAY "*                                                 *"
           DISPLAY "***************************************************"
           DISPLAY " Enter your choice: " WITH NO ADVANCING
           ACCEPT WS-ADMIN-MAIN-PAGE-CHOICE

           EVALUATE WS-ADMIN-MAIN-PAGE-CHOICE
               
               WHEN '1' PERFORM ADMIN-LOGIN-PAGE
               WHEN '2' PERFORM ADMIN-SIGNUP-PAGE
               WHEN '3' PERFORM MAIN-PAGE
               WHEN OTHER 
                   PERFORM CLEAR
                   PERFORM INVALID-INPUT-MESSAGE.

       ADMIN-LOGIN-PAGE.
           PERFORM CLEAR
           MOVE ZEROES TO WS-BOOL
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*              Login Page - Admin                 *"
           DISPLAY "***************************************************"
         
           DISPLAY " Enter your email: " WITH NO ADVANCING
           ACCEPT WS-EMAIL
           DISPLAY " Enter your password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD
           
           
           MOVE FUNCTION LOWER-CASE(WS-EMAIL) TO WS-EMAIL

           PERFORM HASH-PASSWORD

           OPEN INPUT FS-ADMIN-FILE
               READ FS-ADMIN-FILE NEXT RECORD
                   AT END CONTINUE
                   NOT AT END 
                       PERFORM UNTIL WS-EOF = 'Y'
                           IF FS-A-EMAIL = WS-EMAIL AND FS-A-PASSWORD 
                               = WS-HASHED-PASSWORD THEN    
                               MOVE 1 TO WS-BOOL
                           END-IF
                           READ FS-ADMIN-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           END-READ
                       END-PERFORM
               END-READ
               
           CLOSE FS-PASSENGER-FILE

           IF WS-BOOL = 1 THEN
               PERFORM SUCCESS-LOGIN-MESSAGE
               PERFORM MAIN-PAGE
           ELSE 
               PERFORM INVALID-ACCOUNT-MESSAGE
               DISPLAY'Do you want to sign up again? [YES/NO]: ' WITH NO 
               ADVANCING
               ACCEPT WS-REENTER-CHOICE

               MOVE FUNCTION UPPER-CASE(WS-REENTER-CHOICE) TO 
               WS-REENTER-CHOICE

               EVALUATE WS-REENTER-CHOICE
                   WHEN 'YES'
                       PERFORM ADMIN-LOGIN-PAGE
                   WHEN 'NO'
                       PERFORM ADMIN-MAIN-PAGE
                   WHEN OTHER
                       DISPLAY'Your choice is invalid'
                   END-EVALUATE
           END-IF.

       ADMIN-SIGNUP-PAGE.
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-BOOL
           MOVE 'a' TO WS-ROLE
           PERFORM CLEAR
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*             Sign Up Page - Admin                *"
           DISPLAY "***************************************************"
           
           MOVE 'TransitEase2025@gmail.com' TO WS-EMAIL
           
           STRING "python3 backend/python_script_for_email.py " WS-EMAIL
           DELIMITED BY SIZE INTO WS-COMMAND
       
           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE
       
           IF WS-RETURN-CODE = 0 
               OPEN INPUT FS-OTP-FILE
                   READ FS-OTP-FILE INTO FS-OTP
                   END-READ
               CLOSE FS-OTP-FILE
               PERFORM CLEAR
               PERFORM USER-SUCCESS-OTP-MESSAGE
               DISPLAY " Enter OTP: " WITH NO ADVANCING
               ACCEPT WS-OTP
       
               IF WS-OTP = FS-OTP
                   PERFORM CLEAR
                   PERFORM CORRECT-OTP-MESSAGE
                   MOVE 1 TO WS-BOOL
               ELSE
                   PERFORM INCORRECT-OTP-MESSAGE
           END-IF    
           MOVE 1 TO WS-BOOL
           
           PERFORM CLEAR
           IF WS-BOOL = 1 THEN
               DISPLAY " Enter first name: " WITH NO ADVANCING
               ACCEPT WS-FIRST-NAME
               DISPLAY " Enter last name: " WITH NO ADVANCING
               ACCEPT WS-LAST-NAME
               DISPLAY " Enter your phone number: " WITH NO ADVANCING
               ACCEPT WS-PHONE-NUMBER
               DISPLAY " Enter your email: " WITH NO ADVANCING
               ACCEPT WS-EMAIL
      
               MOVE FUNCTION LOWER-CASE(WS-EMAIL) TO WS-EMAIL

               OPEN INPUT FS-ADMIN-FILE
                   READ FS-ADMIN-FILE NEXT RECORD
                       AT END CONTINUE
                       NOT AT END
                           PERFORM UNTIL WS-EOF = 'Y'
                           DISPLAY FS-ADMIN-RECORD
                               IF FS-A-EMAIL = WS-EMAIL THEN
                                   MOVE 0 TO WS-BOOL
                               END-IF
                               READ FS-ADMIN-FILE NEXT RECORD
                               AT END MOVE 'Y' TO WS-EOF
                               END-READ
                           END-PERFORM
                   END-READ
               CLOSE FS-ADMIN-FILE

            IF WS-BOOL = 0 THEN
               PERFORM EMAIL-TAKEN-MESSAGE
               DISPLAY'Do you want to sign up again? [YES/NO]: ' WITH NO 
               ADVANCING
               ACCEPT WS-REENTER-CHOICE

               MOVE FUNCTION UPPER-CASE(WS-REENTER-CHOICE) TO 
               WS-REENTER-CHOICE

               EVALUATE WS-REENTER-CHOICE
                   WHEN 'YES'
                       PERFORM ADMIN-SIGNUP-PAGE
                   WHEN 'NO'
                       PERFORM ADMIN-MAIN-PAGE
                   WHEN OTHER
                       DISPLAY'Your choice is invalid'
                   END-EVALUATE
           END-IF

           STRING "python3 backend/python_script_for_email.py "WS-EMAIL
           DELIMITED BY SIZE INTO WS-COMMAND

           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE

           IF WS-RETURN-CODE = 0 
               OPEN INPUT FS-OTP-FILE
                   READ FS-OTP-FILE INTO FS-OTP
                   END-READ
               CLOSE FS-OTP-FILE
               PERFORM CLEAR
               PERFORM ADMIN-SUCCESS-OTP-MESSAGE
               DISPLAY " Enter OTP: " WITH NO ADVANCING
               ACCEPT WS-OTP

               IF WS-OTP = FS-OTP
                   PERFORM CLEAR
                   PERFORM CORRECT-OTP-MESSAGE

                   DISPLAY " Enter your password: " WITH NO ADVANCING
                   ACCEPT WS-PASSWORD
                   DISPLAY " Confirm your password: " WITH NO ADVANCING
                   ACCEPT WS-CONFIRM-PASSWORD

                       IF WS-PASSWORD = WS-CONFIRM-PASSWORD THEN
                           PERFORM SUCCESS-ACCOUNT-MESSAGE
                           PERFORM RECORD-ADMIN
                           
                           PERFORM ADMIN-MAIN-PAGE
                   ELSE
                       PERFORM INVALID-ACCOUNT-MESSAGE
                       DISPLAY'Do you want to sign up again? [YES/NO]: ' 
                       WITH NO ADVANCING
                       ACCEPT WS-REENTER-CHOICE
       
                       MOVE FUNCTION UPPER-CASE(WS-REENTER-CHOICE) TO 
                       WS-REENTER-CHOICE
       
                       EVALUATE WS-REENTER-CHOICE
                           WHEN 'YES'
                               PERFORM ADMIN-LOGIN-PAGE
                           WHEN 'NO'
                               PERFORM ADMIN-MAIN-PAGE
                           WHEN OTHER
                               DISPLAY'Your choice is invalid'
                           END-EVALUATE
                   END-IF
               ELSE
                   PERFORM INCORRECT-OTP-MESSAGE
           END-IF.

      *INITIALIZE-RECORDS.
      *    MOVE LS-FIRST-NAME TO WS-FIRST-NAME
      *    MOVE LS-LAST-NAME TO WS-LAST-NAME
      *    MOVE LS-EMAIL TO WS-EMAIL
      *    PERFORM HASH-PASSWORD
      *    MOVE WS-HASHED-PASSWORD TO WS-PASSWORD
      *    MOVE WS-HASHED-PASSWORD TO LS-PASSWORD
      *    MOVE LS-PHONE-NUMBER TO WS-PHONE-NUMBER
      *    MOVE LS-ROLE TO WS-ROLE
      *    .
       
       RECORD-ADMIN.
      *    Fetch Last Generated ID (Para sa incremententation)
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUES TO FS-A-USER-ID

           OPEN I-O  FS-ADMIN-FILE

           START  FS-ADMIN-FILE KEY IS GREATER THAN FS-A-USER-ID
           READ FS-ADMIN-FILE NEXT RECORD 
           AT END MOVE 1 TO WS-INCREMENT-VALUE
           NOT AT END
               PERFORM UNTIL WS-EOF = 'Y'
                   MOVE FS-A-USER-ID TO WS-LAST-GENERATED-ID
                   READ FS-ADMIN-FILE NEXT RECORD
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END
                           CONTINUE
               END-PERFORM
           END-READ
           
           PERFORM HASH-PASSWORD
           
           MOVE FS-HASHED-PASSWORD TO WS-PASSWORD

           IF WS-LAST-GENERATED-ID NOT EQUAL TO SPACES THEN
               MOVE WS-L-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE 
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF

           PERFORM GENERATE-ID-SEQUENCE

           MOVE WS-GENERATED-USER-ID TO WS-USER-ID

           PERFORM GENERATE-TIME-STAMP

           MOVE WS-USER-RECORD TO FS-ADMIN-RECORD

           WRITE FS-ADMIN-RECORD
           END-WRITE

           CLOSE FS-ADMIN-FILE
           .
       
       RECORD-PASSENGER.
      *    Fetch Last Generated ID (Para sa incremententation)
           MOVE SPACES TO WS-EOF
           MOVE ZEROES TO WS-INCREMENT-VALUE
           MOVE LOW-VALUES TO FS-P-USER-ID
       
           OPEN I-O FS-PASSENGER-FILE
       
           START FS-PASSENGER-FILE KEY IS GREATER THAN FS-P-USER-ID
           READ FS-PASSENGER-FILE NEXT RECORD
               AT END MOVE 1 TO WS-INCREMENT-VALUE    
               NOT AT END 
                   PERFORM UNTIL WS-EOF = 'Y'
                       MOVE FS-P-USER-ID TO WS-LAST-GENERATED-ID
                       READ FS-PASSENGER-FILE NEXT RECORD
                           AT END MOVE 'Y' TO WS-EOF
                           NOT AT END
                               CONTINUE
                       END-READ
                   END-PERFORM
           END-READ

           PERFORM HASH-PASSWORD

           MOVE WS-HASHED-PASSWORD TO WS-PASSWORD
       
           IF WS-LAST-GENERATED-ID NOT EQUAL TO SPACES THEN
               MOVE WS-L-INCREMENT-VALUE TO WS-INCREMENT-VALUE
               ADD 1 TO WS-INCREMENT-VALUE
           ELSE 
               MOVE 1 TO WS-INCREMENT-VALUE
           END-IF
           
           PERFORM GENERATE-ID-SEQUENCE
           
           MOVE WS-GENERATED-USER-ID TO WS-USER-ID

           PERFORM GENERATE-TIME-STAMP
       
           MOVE WS-USER-RECORD TO FS-PASSENGER-RECORD
       
           WRITE FS-PASSENGER-RECORD
           END-WRITE
       
           CLOSE FS-PASSENGER-FILE
           .
       
       CHECK-FILE-STATUS.
           MOVE SPACES TO WS-FILE-STATUS
      *    Check if File exist, and if it doesn't it will create one
           OPEN I-O FS-PASSENGER-FILE
           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT FS-PASSENGER-FILE
               IF WS-FILE-STATUS NOT = '00'
                   DISPLAY 'Error : <Unable Create a File>'
               END-IF
           END-IF
           CLOSE FS-PASSENGER-FILE
           MOVE SPACES TO WS-FILE-STATUS
           OPEN I-O FS-ADMIN-FILE
       
           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT FS-ADMIN-FILE
               IF WS-FILE-STATUS NOT = '00'
                   DISPLAY 'Error : <Unable Create a File>'
               END-IF
           END-IF
           CLOSE FS-ADMIN-FILE

           MOVE SPACES TO WS-FILE-STATUS
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

       HASH-PASSWORD.
      *    Hashes password for security using python
           STRING "python3 backend/hash_password.py " 
               WS-PASSWORD DELIMITED BY SIZE INTO WS-COMMAND.
           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               OPEN INPUT FS-HASHED-PASSWORD-FILE
               READ FS-HASHED-PASSWORD-FILE INTO FS-HASHED-PASSWORD
               END-READ
               CLOSE FS-HASHED-PASSWORD-FILE
               MOVE FS-HASHED-PASSWORD TO WS-HASHED-PASSWORD
           ELSE
               PERFORM INVALID-HASH-PASSWORD
           END-IF
           OPEN OUTPUT FS-HASHED-PASSWORD-FILE
           CLOSE FS-HASHED-PASSWORD-FILE
           .

       INVALID-HASH-PASSWORD.
           DISPLAY "***************************************************"
           DISPLAY "*           Failed to hash the password.          *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       INVALID-INPUT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*            Invalid Input. Try Again!            *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to go back..."

           ACCEPT WS-BUFFER.

       SUCCESS-LOGIN-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*               Login Successful!                 *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       SUCCESS-ACCOUNT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*          Account Created Successfully!          *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       INVALID-ACCOUNT-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*     Invalid email and/or Password. Try Again!   *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."
       
           ACCEPT WS-BUFFER.

       USER-SUCCESS-OTP-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*      An OTP has already been sent to your       *"
           DISPLAY "*              email address.                     *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       ADMIN-SUCCESS-OTP-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*      An OTP has already been sent to your       *"
           DISPLAY "*              company email address.             *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       CORRECT-OTP-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*    OTP verification successfull. You may now    *"
           DISPLAY "*              complete your sign up!             *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       INCORRECT-OTP-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*           Incorrect OTP. Try Again!             *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       FAILED-OTP-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*           ERROR: OTP failed to send!            *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       EMAIL-TAKEN-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*        Email is already taken. Try Again!       *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       PASSWORD-MISMATCH-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*        Password do not match. Try Again!        *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       PASSWORD-EXCEED-MESSAGE.
           DISPLAY "***************************************************"
           DISPLAY "*        Password exceeds the allowed length!     *"
           DISPLAY "***************************************************"
           DISPLAY " Press 'enter' key to continue..."

           ACCEPT WS-BUFFER.

       RETURN-TO-MAINPAGE.
           DISPLAY "Do you want to go back? [YES] [NO]: " 
           WITH NO ADVANCING
           ACCEPT WS-RETURN-MAINPAGE

           MOVE FUNCTION UPPER-CASE(WS-RETURN-MAINPAGE) TO 
           WS-RETURN-MAINPAGE
           .       
           
       CLEAR.
           CALL 'SYSTEM' USING 'clear'.

