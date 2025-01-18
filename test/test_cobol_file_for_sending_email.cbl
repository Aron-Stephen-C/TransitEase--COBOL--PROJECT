       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_cobol_file_for_sending_email.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OTP-FILE ASSIGN TO "otp.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OTP-FILE.
       01  OTP-RECORD PIC X(6).

       WORKING-STORAGE SECTION.
       01 WS-EMAIL PIC X(100).
       01 WS-COMMAND PIC X(200).
       01 WS-RETURN-CODE PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      *    DISPLAY "Enter recipient email: " WITH NO ADVANCING.
      *    ACCEPT WS-EMAIL.
           MOVE 'aronstephenscordova@gmail.com' TO WS-EMAIL

           STRING "python3 python_script_for_email.py " WS-EMAIL
                  DELIMITED BY SIZE INTO WS-COMMAND.

           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               OPEN INPUT OTP-FILE
               READ OTP-FILE INTO OTP-RECORD
               CLOSE OTP-FILE
               DISPLAY "OTP sent successfully. OTP: " OTP-RECORD
           ELSE
               DISPLAY "Failed to send OTP."
           END-IF.

           STOP RUN.
