       IDENTIFICATION DIVISION.
       PROGRAM-ID. test-p.
       

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-OUTPUT-FILE ASSIGN TO 'Files/Output.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  FS-OUTPUT-FILE.
       01  FS-OUTPUT-NUM    PIC Z(3),Z(3),ZZ9.99.
       01  FS-OUTPUT-RECORD.
           02    FS-FILLER-1    PIC X(18).
           02    FS-FIRST-NUM    PIC Z(3),Z(3),ZZ9.99.
           02    FS-FILLER-2    PIC X(5).
           02    FS-SECOND-NUM    PIC Z(3),Z(3),ZZ9.99.
           02    FS-FILLER-3    PIC X(4).
           02    FS-RESULT    PIC Z(3),Z(3),ZZ9.99.

       WORKING-STORAGE SECTION.
       01  WS-CHOICE    PIC X.
       01  WS-BUFFER    PIC X.
       01  WS-PHRASE-TO-CONVERTED    PIC X(42).
       01  WS-PHRASE-TO-ACRONYM    PIC X(42).
       01  WS-PHRASE-TEMPORARY-VAR    PIC X(42).
       01  WS-COUNTER-I    PIC 9(2).
       01  WS-FLAG    PIC 9.
       01  WS-REPEAT-VAR    PIC X(3).
       88  WS-ALPHABET VALUES 'A' THRU 'Z'.
       01  WS-ROMAN-NUMERAL-VALUES.
           02 WS-ROMAN-NUMERAL-VALUE    PIC 9(4) OCCURS 13 TIMES.
           02 WS-ROMAN-NUMERAL-SYMBOL    PIC X(4) OCCURS 13 TIMES.
       01  WS-ROMAN-NUMERAL    PIC X(42).
       01  WS-ARABIC-NUMBER    PIC 9(9).
       01  WS-YEAR    PIC 9(9).
       01  WS-DECIMAL    PIC 9(9).
       01  WS-BINARY    PIC 9(12).
       01  WS-BASE    PIC 9(9).
       01  WS-RESULT-BINARY    PIC Z(11)9.
       01  WS-RESULT-DECIMAL    PIC Z(3),Z(3),ZZ9.
       01  WS-STATUS    PIC 99.
       01  WS-FIRST-NUM    PIC 9(9).
       01  WS-SECOND-NUM    PIC 9(9).
       01  WS-RESULT    PIC 9(9).

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU    
           STOP RUN.

       MAIN-MENU.
           PERFORM UNTIL WS-CHOICE = 'g'
               PERFORM CLEAR
               MOVE SPACES TO WS-REPEAT-VAR
               DISPLAY 'M E N U'
               DISPLAY ' '
               DISPLAY '(a) - Acronym'
               DISPLAY '(b) - Roman Numerals'
               DISPLAY '(c) - Leap Year'
               DISPLAY '(d) - Decimal to Binary'
               DISPLAY '(e) - Binary to Decimal'
               DISPLAY '(f) - Fila handling'
               DISPLAY '(g) - EXIT'
               DISPLAY ' '
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT WS-CHOICE
       
               MOVE FUNCTION LOWER-CASE(WS-CHOICE) TO WS-CHOICE
       
               EVALUATE WS-CHOICE
                   WHEN 'a'
                       PERFORM ACRONYM
                   WHEN 'b'
                       PERFORM ROMAN-NUMERAL
                   WHEN 'c'
                       PERFORM LEAP-YEAR
                   WHEN 'd'
                       PERFORM DECIMAL-BINARY
                   WHEN 'e'
                       PERFORM BINARY-DECIMAL
                   WHEN 'f'
                       PERFORM SPDQ-FILE
                   WHEN 'g'
                       PERFORM CLEAR
                       DISPLAY '[PROGRAM EXIT]'
                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Error : Invalid Input'
                       ACCEPT WS-BUFFER
       
               END-EVALUATE
       
           END-PERFORM
           .

       ACRONYM.
           PERFORM CLEAR
           DISPLAY '(a) - Acronym'
           DISPLAY ' '
           DISPLAY 'Enter phrase : ' WITH NO ADVANCING
           ACCEPT WS-PHRASE-TO-CONVERTED

           MOVE FUNCTION UPPER-CASE(WS-PHRASE-TO-CONVERTED) TO 
           WS-PHRASE-TO-CONVERTED

           MOVE 0 TO WS-COUNTER-I
           MOVE SPACES TO WS-PHRASE-TO-ACRONYM
           MOVE SPACES TO WS-PHRASE-TEMPORARY-VAR
           MOVE 1 TO WS-FLAG

           PERFORM VARYING WS-COUNTER-I FROM 1 BY 1 UNTIL WS-COUNTER-I >
           FUNCTION LENGTH(WS-PHRASE-TO-CONVERTED)
               MOVE SPACES TO WS-PHRASE-TEMPORARY-VAR

               IF WS-FLAG = 1 THEN 
                   STRING WS-PHRASE-TO-ACRONYM DELIMITED BY SPACE
                           WS-PHRASE-TO-CONVERTED(WS-COUNTER-I:1) 
                           DELIMITED BY SIZE
                           INTO WS-PHRASE-TEMPORARY-VAR

                           MOVE WS-PHRASE-TEMPORARY-VAR TO 
                           WS-PHRASE-TO-ACRONYM
                   MOVE 0 TO WS-FLAG
               END-IF

               IF WS-PHRASE-TO-CONVERTED(WS-COUNTER-I:1) = ' '
                   MOVE 1 TO WS-FLAG
               END-IF

           END-PERFORM

           DISPLAY ' '
           DISPLAY 'Acronym : ' WS-PHRASE-TO-ACRONYM

           PERFORM UNTIL WS-REPEAT-VAR = 'NO'
               PERFORM REPEAT-ALGO

               EVALUATE WS-REPEAT-VAR
                   WHEN 'YES'
                       PERFORM ACRONYM
                   WHEN 'NO'
                       DISPLAY ' '
                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Error : Invalid Input'
                       ACCEPT WS-BUFFER
               END-EVALUATE
           END-PERFORM
           .
       
       ROMAN-NUMERAL.
           PERFORM CLEAR
           DISPLAY '(b) - Roman Numeral'
           DISPLAY ' '
           DISPLAY 'Enter number : 'WITH NO ADVANCING
           ACCEPT WS-ARABIC-NUMBER

           PERFORM INITIALIZE-VALUE

           PERFORM UNTIL WS-ARABIC-NUMBER = 0 OR WS-COUNTER-I > 13
               MOVE SPACES TO WS-PHRASE-TEMPORARY-VAR
               IF WS-ARABIC-NUMBER >= 
               WS-ROMAN-NUMERAL-VALUE(WS-COUNTER-I) THEN
                   
                   STRING WS-ROMAN-NUMERAL DELIMITED BY SPACE
                          WS-ROMAN-NUMERAL-SYMBOL(WS-COUNTER-I) 
                          DELIMITED BY SIZE
                          INTO WS-PHRASE-TEMPORARY-VAR
                   END-STRING

                  MOVE WS-PHRASE-TEMPORARY-VAR TO 
                  WS-ROMAN-NUMERAL

                  SUBTRACT WS-ROMAN-NUMERAL-VALUE(WS-COUNTER-I) 
                  FROM WS-ARABIC-NUMBER 
               ELSE
                   ADD 1 TO WS-COUNTER-I
               END-IF
           END-PERFORM

           DISPLAY ' ' 
           DISPLAY 'Roman Numeral : ' WS-ROMAN-NUMERAL

           PERFORM UNTIL WS-REPEAT-VAR = 'NO'
               PERFORM REPEAT-ALGO

               EVALUATE WS-REPEAT-VAR
                   WHEN 'YES'
                       PERFORM ROMAN-NUMERAL
                   WHEN 'NO'
                       DISPLAY ' '
                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Error : Invalid Input'
                       ACCEPT WS-BUFFER
               END-EVALUATE
           END-PERFORM
           .

       INITIALIZE-VALUE.
           MOVE 1000 TO WS-ROMAN-NUMERAL-VALUE(1)
           MOVE 900 TO WS-ROMAN-NUMERAL-VALUE(2)
           MOVE 500 TO WS-ROMAN-NUMERAL-VALUE(3)
           MOVE 400 TO WS-ROMAN-NUMERAL-VALUE(4)
           MOVE 100 TO WS-ROMAN-NUMERAL-VALUE(5)
           MOVE 90 TO WS-ROMAN-NUMERAL-VALUE(6)
           MOVE 50 TO WS-ROMAN-NUMERAL-VALUE(7)
           MOVE 40 TO WS-ROMAN-NUMERAL-VALUE(8)
           MOVE 10 TO WS-ROMAN-NUMERAL-VALUE(9)
           MOVE 9 TO WS-ROMAN-NUMERAL-VALUE(10)
           MOVE 5 TO WS-ROMAN-NUMERAL-VALUE(11)
           MOVE 4 TO WS-ROMAN-NUMERAL-VALUE(12)
           MOVE 1 TO WS-ROMAN-NUMERAL-VALUE(13)

           MOVE 'M' TO WS-ROMAN-NUMERAL-SYMBOL(1)
           MOVE 'CM' TO WS-ROMAN-NUMERAL-SYMBOL(2)
           MOVE 'D' TO WS-ROMAN-NUMERAL-SYMBOL(3)
           MOVE 'CD' TO WS-ROMAN-NUMERAL-SYMBOL(4)
           MOVE 'C' TO WS-ROMAN-NUMERAL-SYMBOL(5)
           MOVE 'XC' TO WS-ROMAN-NUMERAL-SYMBOL(6)
           MOVE 'L' TO WS-ROMAN-NUMERAL-SYMBOL(7)
           MOVE 'XL' TO WS-ROMAN-NUMERAL-SYMBOL(8)
           MOVE 'X' TO WS-ROMAN-NUMERAL-SYMBOL(9)
           MOVE 'IX' TO WS-ROMAN-NUMERAL-SYMBOL(10)
           MOVE 'V' TO WS-ROMAN-NUMERAL-SYMBOL(11)
           MOVE 'IV' TO WS-ROMAN-NUMERAL-SYMBOL(12)
           MOVE 'I' TO WS-ROMAN-NUMERAL-SYMBOL(13)

           MOVE 1 TO WS-COUNTER-I
           MOVE SPACES TO WS-PHRASE-TEMPORARY-VAR
           MOVE SPACES TO WS-ROMAN-NUMERAL
           .

       REPEAT-ALGO.
           DISPLAY ' '
           DISPLAY 'Do you want to repeat again? [YES][NO] ' WITH NO 
           ADVANCING
           ACCEPT WS-REPEAT-VAR

           MOVE FUNCTION UPPER-CASE(WS-REPEAT-VAR) TO WS-REPEAT-VAR
           .
       
       CLEAR.
           CALL 'SYSTEM' USING 'CLEAR'.
       


       LEAP-YEAR.
           PERFORM CLEAR
           DISPLAY '(c) - Leap Year'
           DISPLAY ' '
           DISPLAY 'Enter year : ' WITH NO ADVANCING
           ACCEPT WS-YEAR

           DISPLAY ' '

           IF (FUNCTION MOD(WS-YEAR,100) = 0 AND 
           FUNCTION MOD(WS-YEAR, 400) NOT = 0) OR 
           FUNCTION MOD(WS-YEAR,4) NOT = 0 THEN
               DISPLAY 'NOT LEAP YEAR'
           ELSE 
               DISPLAY 'LEAP YEAR'
           END-IF

           PERFORM UNTIL WS-REPEAT-VAR = 'NO'
           
           PERFORM REPEAT-ALGO

           EVALUATE WS-REPEAT-VAR
               WHEN 'YES'
                    PERFORM LEAP-YEAR
               WHEN 'NO'
                   CONTINUE

               WHEN OTHER 
                   DISPLAY ' '
                   DISPLAY 'Invalid Input.'

           END-EVALUATE
           END-PERFORM
           
           .
       
       DECIMAL-BINARY.
           PERFORM CLEAR
           DISPLAY '(d) - Decimal to Binary'
           DISPLAY ' '
           DISPLAY 'Enter decimal : ' WITH NO ADVANCING
           ACCEPT WS-DECIMAL

           MOVE ZEROES TO WS-BINARY
           MOVE 1 TO WS-BASE

           PERFORM UNTIL WS-DECIMAL = 0
               COMPUTE WS-BINARY = WS-BINARY + 
               (FUNCTION MOD(WS-DECIMAL, 2) * WS-BASE)
               COMPUTE WS-DECIMAL = WS-DECIMAL / 2
               COMPUTE WS-BASE = WS-BASE * 10
           END-PERFORM

           MOVE WS-BINARY TO WS-RESULT-BINARY

           DISPLAY ' '
           DISPLAY 'Binary : ' WS-RESULT-BINARY

           PERFORM UNTIL WS-REPEAT-VAR = 'NO'
               PERFORM REPEAT-ALGO
               EVALUATE WS-REPEAT-VAR
                   WHEN 'YES'
                       PERFORM DECIMAL-BINARY
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER 
                       DISPLAY ' '
                       DISPLAY 'Invalid Input'
               END-EVALUATE
           END-PERFORM
           .

       BINARY-DECIMAL.
           PERFORM CLEAR
           DISPLAY '(e) - Binary to Decimal'
           DISPLAY ' '
           DISPLAY 'Enter Binary : ' WITH NO ADVANCING
           ACCEPT WS-BINARY

           MOVE ZEROES TO WS-DECIMAL
           MOVE 1 TO WS-BASE

           PERFORM UNTIL WS-BINARY = 0 
               COMPUTE WS-DECIMAL = WS-DECIMAL + 
               (FUNCTION MOD(WS-BINARY,10) * WS-BASE)
               COMPUTE WS-BINARY = WS-BINARY / 10
               COMPUTE WS-BASE = WS-BASE * 2
           END-PERFORM

           MOVE WS-DECIMAL TO WS-RESULT-DECIMAL

           DISPLAY ' '
           DISPLAY 'Decimal : ' WS-RESULT-DECIMAL

           PERFORM UNTIL WS-REPEAT-VAR = 'NO'
               PERFORM REPEAT-ALGO

               EVALUATE WS-REPEAT-VAR
                   WHEN 'YES'
                       PERFORM BINARY-DECIMAL
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      DISPLAY ' '
                      DISPLAY 'Invalid Input'
               END-EVALUATE
               
           END-PERFORM
           .

       SPDQ-FILE.
           PERFORM CLEAR

           OPEN EXTEND FS-OUTPUT-FILE
               IF WS-STATUS NOT = '00'
                   OPEN OUTPUT FS-OUTPUT-FILE
               END-IF

           DISPLAY '(f) - File Handling'
           DISPLAY ' '
           DISPLAY 'Enter first number : ' WITH NO ADVANCING
           ACCEPT WS-FIRST-NUM

           MOVE WS-FIRST-NUM TO FS-OUTPUT-NUM
           WRITE FS-OUTPUT-NUM

           DISPLAY 'Enter second number : ' WITH NO ADVANCING
           ACCEPT WS-SECOND-NUM

           MOVE WS-SECOND-NUM TO FS-OUTPUT-NUM
           WRITE FS-OUTPUT-NUM

           MOVE WS-FIRST-NUM TO FS-FIRST-NUM
           MOVE WS-SECOND-NUM TO FS-SECOND-NUM

           COMPUTE WS-RESULT = WS-FIRST-NUM + WS-SECOND-NUM
           MOVE 'The sum of ' TO FS-FILLER-1
           MOVE ' and ' TO FS-FILLER-2
           MOVE ' is ' TO FS-FILLER-3
           MOVE WS-RESULT TO FS-RESULT
           WRITE FS-OUTPUT-RECORD

           COMPUTE WS-RESULT = WS-FIRST-NUM - WS-SECOND-NUM
           MOVE 'The difference of ' TO FS-FILLER-1
           MOVE ' and ' TO FS-FILLER-2
           MOVE ' is ' TO FS-FILLER-3
           MOVE WS-RESULT TO FS-RESULT
           WRITE FS-OUTPUT-RECORD

           COMPUTE WS-RESULT = WS-FIRST-NUM * WS-SECOND-NUM
           MOVE 'The product of ' TO FS-FILLER-1
           MOVE ' and ' TO FS-FILLER-2
           MOVE ' is ' TO FS-FILLER-3
           MOVE WS-RESULT TO FS-RESULT
           WRITE FS-OUTPUT-RECORD

           COMPUTE WS-RESULT = WS-FIRST-NUM + WS-SECOND-NUM
           MOVE 'The quotient of ' TO FS-FILLER-1
           MOVE ' and ' TO FS-FILLER-2
           MOVE ' is ' TO FS-FILLER-3
           MOVE WS-RESULT TO FS-RESULT
           WRITE FS-OUTPUT-RECORD


           CLOSE FS-OUTPUT-FILE

           PERFORM UNTIL WS-REPEAT-VAR = 'NO'
               PERFORM REPEAT-ALGO
               EVALUATE WS-REPEAT-VAR
                   WHEN 'YES'
                       PERFORM SPDQ-FILE
                   WHEN 'NO'
                       CONTINUE
                   WHEN OTHER
                      DISPLAY ' '
                      DISPLAY 'Invalid Input'
               END-EVALUATE
               
           END-PERFORM
           .
