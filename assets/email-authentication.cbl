       IDENTIFICATION DIVISION.
       PROGRAM-ID. email-authentication.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-OTP-FILE ASSIGN TO "data/otp.txt"
           ORGANIZATION IS LINE sequential.

       DATA DIVISION.
       FILE SECTION.
       FD  FS-OTP-FILE.
       01  FS-OTP PIC X(6).
       
       WORKING-STORAGE SECTION.
       01  WS-COMMAND PIC X(300). 
       01  WS-EMAIL PIC X(100).
       01  WS-RETURN-CODE PIC 9(2).
       01  WS-OTP PIC X(6).

       PROCEDURE DIVISION.
           
           CALL "SYSTEM" USING "clear"
           DISPLAY "***************************************************"
           DISPLAY "*            Welcome to TransitEase!              *"
           DISPLAY "*              Sign Up Page - User                *"
           DISPLAY "***************************************************"
           
           DISPLAY " Enter your email: " WITH NO ADVANCING
           ACCEPT WS-EMAIL

           MOVE FUNCTION LOWER-CASE(WS-EMAIL) TO WS-EMAIL

           STRING "python3 backend/python_script_for_email.py " WS-EMAIL
           DELIMITED BY SIZE INTO WS-COMMAND

           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE

           IF WS-RETURN-CODE = 0 
               OPEN INPUT FS-OTP-FILE
                   READ FS-OTP-FILE INTO FS-OTP
                   END-READ
               CLOSE FS-OTP-FILE
               DISPLAY " OTP SENT TO YOUR EMAIL!"
               DISPLAY " Enter OTP: " WITH NO ADVANCING
               ACCEPT WS-OTP

               IF WS-OTP = FS-OTP
                   DISPLAY " YEHEY!"
               ELSE
                   DISPLAY " SAD"
           END-IF
               
           ELSE
               DISPLAY "ERROR: <FAILED TO SEND OTP>"

           END-IF

           
           STOP RUN.
