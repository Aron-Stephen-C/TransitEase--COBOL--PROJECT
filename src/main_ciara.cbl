       IDENTIFICATION DIVISION.
       PROGRAM-ID. main_ciara.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RETURNING-CODE    PIC 99.
       
       PROCEDURE DIVISION.
           CALL 'SYSTEM' USING 'backend/user_profile_management' 
           RETURNING WS-RETURNING-CODE
           IF WS-RETURNING-CODE = 0 THEN
               DISPLAY 'YEH'
           ELSE
               DISPLAY 'NO'
           END-IF
           STOP RUN.
       