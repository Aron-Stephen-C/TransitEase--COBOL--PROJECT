      * This is the module that Manages User records
       IDENTIFICATION DIVISION.
       PROGRAM-ID. user_profile_management.
       
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       
       DATA DIVISION.
       FILE SECTION.
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
       
       LINKAGE SECTION.
       01  LS-USER-RECORD.
           02  LS-FIRST-NAME    PIC X(50).
           02  LS-LAST-NAME    PIC X(50).
           02  LS-EMAIL    PIC X(100).
           02  LS-PASSWORD    PIC X(100).
           02  LS-PHONE-NUMBER    PIC X(11).
           02  LS-ROLE    PIC X.
       
       PROCEDURE DIVISION USING LS-USER-RECORD.
           PERFORM CHECK-FILE-STATUS

           PERFORM INITIALIZE-RECORDS

           EVALUATE WS-ROLE
               WHEN 'P'
                   PERFORM RECORD-PASSENGER
               WHEN 'A'
                   PERFORM RECORD-ADMIN 
               WHEN OTHER
                   DISPLAY 'Error : <Invalid Role>'
                  CONTINUE
           END-EVALUATE
           GOBACK
           STOP RUN.

       INITIALIZE-RECORDS.
           MOVE LS-FIRST-NAME TO WS-FIRST-NAME
           MOVE LS-LAST-NAME TO WS-LAST-NAME
           MOVE LS-EMAIL TO WS-EMAIL
           PERFORM HASH-PASSWORD
           MOVE WS-HASHED-PASSWORD TO WS-PASSWORD
           MOVE WS-HASHED-PASSWORD TO LS-PASSWORD
           MOVE LS-PHONE-NUMBER TO WS-PHONE-NUMBER
           MOVE LS-ROLE TO WS-ROLE
           .
       
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
               LS-PASSWORD DELIMITED BY SIZE INTO WS-COMMAND.
           CALL "SYSTEM" USING WS-COMMAND RETURNING WS-RETURN-CODE.

           IF WS-RETURN-CODE = 0
               OPEN INPUT FS-HASHED-PASSWORD-FILE
               READ FS-HASHED-PASSWORD-FILE INTO FS-HASHED-PASSWORD
               END-READ
               CLOSE FS-HASHED-PASSWORD-FILE
               MOVE FS-HASHED-PASSWORD TO WS-HASHED-PASSWORD
           ELSE
               DISPLAY "Failed to hash the password."
           END-IF
           OPEN OUTPUT FS-HASHED-PASSWORD-FILE
           CLOSE FS-HASHED-PASSWORD-FILE
           .
