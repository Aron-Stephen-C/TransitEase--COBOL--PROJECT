       IDENTIFICATION DIVISION.
       PROGRAM-ID. TicketIDGenerator.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ticket-file ASSIGN TO "ticketfile.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ticket-id
               FILE STATUS IS file-status-code.

       DATA DIVISION.
       FILE SECTION.
       FD  ticket-file.
       01  ticket-record.
           02  ticket-id       PIC X(20).  *> ID: YYYYMMDDHHMMSSNNN
           02  passenger-name  PIC X(30).
           02  travel-date     PIC 9(8).
           02  departure       PIC X(20).
           02  destination     PIC X(20).
           02  ticket-status   PIC X(10).

       WORKING-STORAGE SECTION.
       01  ws-date-time       PIC X(14).  *> YYYYMMDDHHMMSS
       01  ws-date            PIC 9(8).   *> YYYYMMDD
       01  ws-time            PIC 9(6).   *> HHMMSS
       01  ws-increment       PIC 9(3) VALUE 0.  *> Incrementing value
       01  ws-last-id         PIC X(20).  *> Stores last generated ID
       01  ws-new-id          PIC X(20).  *> Stores new ID
       01  file-status-code   PIC XX VALUE SPACES. 
       01  EOF                PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Starting Ticket ID Generator..."

           *> Step 1: Retrieve system date and time
           CALL "CURRENT-DATE" USING ws-date-time.
           MOVE ws-date-time(1:8) TO ws-date.       *> Extract YYYYMMDD
           MOVE ws-date-time(9:6) TO ws-time.       *> Extract HHMMSS

           *> Step 2: Open the indexed file
           OPEN I-O ticket-file.
           IF file-status-code = "35"
               DISPLAY "File does not exist. Creating a new file."
               OPEN OUTPUT ticket-file
               CLOSE ticket-file
               OPEN I-O ticket-file
           END-IF.

           *> Step 3: Retrieve the last ID
           START ticket-file KEY IS GREATER THAN ticket-id. 
           READ ticket-file NEXT RECORD
               AT END
                   DISPLAY "No records found. Starting from scratch."
                   MOVE "001" TO ws-increment
                   MOVE "Y" TO EOF  *> Set EOF flag to "Yes"
               NOT AT END
                   MOVE "N" TO EOF  *> Set EOF flag to "No"
           END-READ.

           *> Loop until EOF is reached
           PERFORM UNTIL EOF = "Y"
               MOVE ticket-id TO ws-last-id
               READ ticket-file NEXT RECORD
                   AT END
                       MOVE "Y" TO EOF  *> Set EOF flag when the end is reached
                   NOT AT END
                       CONTINUE
           END-PERFORM.

           *> Step 4: Extract and increment the value
           IF ws-last-id NOT EQUAL SPACES
               MOVE ws-last-id(15:3) TO ws-increment
               ADD 1 TO ws-increment
           ELSE
               MOVE "001" TO ws-increment
           END-IF.

           *> Step 5: Generate the new ID
           STRING ws-date DELIMITED BY SIZE
                  ws-time DELIMITED BY SIZE
                  ws-increment DELIMITED BY SIZE
                  INTO ws-new-id.

           *> Step 6: Write the new record to the file
           MOVE ws-new-id TO ticket-id
           MOVE "John Doe" TO passenger-name
           MOVE "20250118" TO travel-date
           MOVE "Manila" TO departure
           MOVE "Cebu" TO destination
           MOVE "Booked" TO ticket-status

           WRITE ticket-record
               INVALID KEY
                   DISPLAY "Error: Unable to write record."
               NOT INVALID KEY
                   DISPLAY "New ticket generated with ID: " ws-new-id.

           *> Step 7: Close the file
           CLOSE ticket-file.
           DISPLAY "Ticket generation complete. File closed."

           STOP RUN.
