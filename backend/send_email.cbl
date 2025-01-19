       IDENTIFICATION DIVISION.
       PROGRAM-ID. send_email.
       

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OTP-FILE ASSIGN TO "data/otp.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OTP-FILE.
       01  OTP-RECORD PIC X(6).

       WORKING-STORAGE SECTION.
       01  WS-EMAIL PIC X(100).
       01  WS-COMMAND PIC X(300).
       01  WS-RETURN-CODE PIC 9(2).
       01  WS-OTP    PIC X(6).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 'aronstephenscordova@gmail.com' TO WS-EMAIL

           STRING "python3 backend/python_script_for_email.py " WS-EMAIL
                  DELIMITED BY SIZE INTO WS-COMMAND.

           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               OPEN INPUT OTP-FILE
               READ OTP-FILE INTO OTP-RECORD
               CLOSE OTP-FILE

               DISPLAY 'Input OTP : ' WITH NO ADVANCING
               ACCEPT WS-OTP

               IF WS-OTP = OTP-RECORD THEN
                   DISPLAY 'Success'
               ELSE
                   DISPLAY 'Womp womp'
               END-IF
               

           ELSE
               DISPLAY "Failed to send OTP."
           END-IF.

           STOP RUN.
