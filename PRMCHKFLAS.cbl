       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PRMCHKFLAS.
       AUTHOR.       AARON SEAVERS
      * CONVERT THE IN-LINE PERFORMS INTO PARAGRAPHS.
       DATE-WRITTEN. OCT 2017 
       DATA DIVISION. 
       WORKING-STORAGE SECTION.
      
       01 NUM-IN          PIC XXXX.
          88 INTEGER-ENTERED       VALUE "0000" THRU "9999".
          88 USER-WANTS-TO-QUIT    VALUE "x" "X" "XX" "xx".
       01 NUM-IN-INT REDEFINES
          NUM-IN          PIC 9999.
       01 WS-QUOTIENT     PIC 9999
                                   VALUE 0.
       01 WS-REMAINDER    PIC 9999 VALUE 1.
          88 NOT-PRIME-NUMBER      VALUE 0.
       01 WS-DIVISOR      PIC 9999 VALUE 0.
        
       PROCEDURE DIVISION.
       0000-DRIVER.
           DISPLAY "Prime Number Checking Program".
           PERFORM 1100-INPUT-CHECK
           UNTIL NUM-IN IS NUMERIC.

           PERFORM 1010-DIVISION-CALC
               VARYING WS-DIVISOR FROM 2 BY 1 
                   UNTIL WS-REMAINDER = 0 
                       OR WS-DIVISOR = (NUM-IN-INT - 1).
           perform 1020-PRIME-CHK.
           display "EXIT".
           GOBACK.

       1100-INPUT-CHECK.
             display "ENTER INTEGER 0000-9999 (WITH LEADING ZEROES)"
             display "(OR ENTER X TO QUIT)."
             ACCEPT NUM-IN.
             
       1010-DIVISION-CALC.
           
           divide NUM-IN-INT BY WS-DIVISOR
             GIVING WS-QUOTIENT remainder WS-REMAINDER.

       1020-PRIME-CHK.
           IF NOT-PRIME-NUMBER OR NUM-IN-INT = 1
             DISPLAY NUM-IN " IS NOT A PRIME"
           ELSE  
             display NUM-IN " IS A PRIME".