       IDENTIFICATION DIVISION.
       PROGRAM-ID. transit_ease.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-COMMAND PIC X(100).

       PROCEDURE DIVISION.

           PERFORM COMPILATION-MODULES

           CALL 'SYSTEM' USING 'backend/user_profile_management' 
           END-CALL

           STOP RUN.

       COMPILATION-MODULES.
           STRING 'cobc -x backend/user_profile_management.cbl '
           DELIMITED BY SIZE
           '-o backend/user_profile_management' DELIMITED BY SIZE 
           INTO WS-COMMAND
           END-STRING

           CALL 'SYSTEM' USING WS-COMMAND
           END-CALL

           MOVE SPACES TO WS-COMMAND

           STRING 'cobc -x backend/schedule_manager.cbl '
           DELIMITED BY SIZE
           '-o backend/schedule_manager' DELIMITED BY SIZE 
           INTO WS-COMMAND
           END-STRING

           CALL 'SYSTEM' USING WS-COMMAND
           END-CALL

           MOVE SPACES TO WS-COMMAND

           STRING 'cobc -x backend/booking_engine.cbl '
           DELIMITED BY SIZE
           '-o backend/booking_engine' DELIMITED BY SIZE 
           INTO WS-COMMAND
           END-STRING

           CALL 'SYSTEM' USING WS-COMMAND
           END-CALL

           MOVE SPACES TO WS-COMMAND

           STRING 'cobc -x backend/pricing_engine.cbl '
           DELIMITED BY SIZE
           '-o backend/pricing_engine' DELIMITED BY SIZE 
           INTO WS-COMMAND
           END-STRING

           CALL 'SYSTEM' USING WS-COMMAND
           END-CALL

           MOVE SPACES TO WS-COMMAND

           STRING 'cobc -x backend/ticketing_module.cbl '
           DELIMITED BY SIZE
           '-o backend/ticketing_module' DELIMITED BY SIZE 
           INTO WS-COMMAND
           END-STRING

           CALL 'SYSTEM' USING WS-COMMAND
           END-CALL
           .
       
       