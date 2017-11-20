       IDENTIFICATION DIVISION.                                          
       PROGRAM-ID.    CALCPYAS.                                                          
       AUTHOR.        AARON SEAVERS                                                  
      **REMARKS.                                                                        
      *	******************************************************************              
      *	* -READ IN PAYROLL FILE AND USE THE HOURS WORKED AND RATE TO CALCLATE:            
      *	*    GROSS PAY            (RATE X HOURS, OVER 40 HOURS IS 1.5X RATE)            
      *	*    STATE TAX            (6.00% X GROSS)                                       
      *	*    MEDICARE/SOC SEC TAX (7.65% X GROSS)                                       
      *	*    LOCAL TAX            (1.00% X GROSS)                                       
      *	*    FEDERAL TAX                                                                
      *	*      IF DEDUCTIONS = 0  (20.00% X GROSS)                                      
      *	*      IF DEDUCTIONS = 1  (18.00% X GROSS)                                      
      *	*      IF DEDUCTIONS = 2  (15.00% X GROSS)                                      
      *	*      IF DEDUCTIONS = 3  (12.00% X GROSS)                                      
      *	*      IF DEDUCTIONS = 4  (10.00% X GROSS)                                      
      *	*    NET PAY              (GROSS - TAXES)                                       
      *	*                                                                               
      *	* -CREATE NEW PAYROLL FILE (PAYROLL-OUT) WITH ABOVE CALCULATIONS COMPLETED.                  
      *	*                                                                               
      *	* -PROGRAM SHOULD DISPLAY FIVE COUNTS:                                           
      *	*   RECORDS READ (COUNT ONE RECORD FOR                                          
      *	*   RECORDS WRITTEN                                                             
      *	*   FILE TOTAL GROSS AMOUNT (FORAMATTED WITH $ SIGNS)                           
      *	*   FILE TOTAL NET AMOUNT (FORAMATTED WITH $ SIGNS)
      *	*   FILE TOTAL WITHHELD (FORMATTED WITH $ SIGNS, (EQUALS GROSS - NET))                             
      *	*************************************************************************          
       ENVIRONMENT DIVISION.                                                            
       CONFIGURATION SECTION.                                                           
       INPUT-OUTPUT SECTION.                                                            
       FILE-CONTROL.                                                                    
             SELECT PAYROLL-IN  ASSIGN TO "C:\Users\Bob\PAYROLL1.txt"
               organization is line sequential
               file status is WS-PAYROLL-IN-STATUS.
             SELECT PAYROLL-OUT ASSIGN TO "C:\Users\Bob\PAYROLL2.txt"
               organization is line sequential
               file status is WS-PAYROLL-OUT-STATUS.             
                                                                                        
       DATA DIVISION.                                                                   
        FILE SECTION.                                                                   
                                                                                        
       FD  PAYROLL-IN                                                                   
           RECORDING MODE IS F                                                        
           DATA RECORD IS PAYROLL-REC-IN.                                                  
       01  PAYROLL-REC-IN.                                                                  
           05  PAYROLL-PAY-DATE-IN         PIC X(06).                                    
           05  PAYROLL-EMP-NUMBER-IN       PIC X(06).                                    
           05  PAYROLL-EMP-HOURS-IN        PIC 9(02)V99.                                 
           05  PAYROLL-EMP-RATE-IN         PIC 9(02)V99.                                 
           05  PAYROLL-EMP-DEDUCTIONS-IN   PIC X(01).                                    
                                                                                        
       FD  PAYROLL-OUT                                                                  
           RECORDING MODE IS F                                                        
           DATA RECORD IS PAYROLL-REC-OUT.                                                  
       01  PAYROLL-REC-OUT.                                                                
           05  PAYROLL-PAY-DATE-OUT         PIC X(06).                                    
           05  PAYROLL-EMP-NUMBER-OUT       PIC X(06).                                    
           05  PAYROLL-EMP-HOURS-OUT    PIC 9(02)V99.                                 
           05  PAYROLL-EMP-RATE-OUT         PIC 9(02)V99.                                 
           05  PAYROLL-EMP-DEDUCTIONS-OUT   PIC X(01).
           05  PAYROLL-EMP-CALCULATIONS.
               10 PAYROLL-BASE-PAY-OUT      PIC 9(04)V99.
               10 PAYROLL-EMP-GROSS-OUT     PIC 9(04)V99.                           
               10 PAYROLL-EMP-SSN-MED-OUT   PIC 9(03)V99.                            
               10 PAYROLL-EMP-STATE-OUT     PIC 9(03)V99.                            
               10 PAYROLL-EMP-LOCAL-OUT     PIC 9(03)V99.                            
               10 PAYROLL-EMP-FED-OUT       PIC 9(03)V99.                            
               10 PAYROLL-EMP-NET-OUT       PIC 9(04)V99.
               10 PAYROLL-DEDUCT-NET-OUT    PIC 9(04)V99.
               10 PAYROLL-OVERTIME-OUT      PIC 9(04)V99.
                                                                                        
       WORKING-STORAGE SECTION.                                                         
       01 WS-RECORDS-READ PIC 99.
       01 WS-RECORDS-WRITTEN PIC 9(02).
       01 WS-TOTAL-NET PIC 9(09)V9(02).
       01 WS-TOTAL-GROSS PIC 9(09)V9(02).
       01 WS-TOTAL-WITHHELD PIC 9(09)V9(02).
       01 WS-OVERTIME-HOURS PIC 9(02).
       01 WS-OVERTIME-GROSS PIC 9(09)V9(02).
       01 WS-NORMAL-PAY-GROSS PIC 9(09)V9(02).
       01 WS-STATE-TAX-WH PIC 9(09)V9(02).
       01 WS-MEDICARE-TAX-WH PIC 9(09)V9(02).
       01 WS-LOCAL-TAX-WH PIC 9(09)V9(02).
       01 WS-FEDERAL-TAX-WH PIC 9(09)V9(02).
       01 WS-TOTAL-FILE-GROSS PIC 9(09)V9(02).
       01 WS-TOTAL-FILE-NET PIC 9(09)V9(02).
       01 WS-TOTAL-FILE-WH PIC 9(09)V9(02).
     
       01 WS-PAYROLL-IN-STATUS PIC X(02).
           88 PAYROLL-IN-FILE-SUCCESSFUL VALUE "00".
           88 END-OF-PAYROLL-FILE VALUE "10".
           88 INVALID-PAYROLL-IN-FILE VALUE "11" THRU "99".
           88 PAYROLL-FILE-NOT-READY VALUE "01"THRU "99".

       01 WS-PAYROLL-OUT-STATUS PIC X(02).
           88 GOOD-PAYROLL-FILE-WRITE VALUE "00".

       01 WS-TOTAL-FILE-GROSS-EDIT PIC $ZZZZZZZZ9.99.
       01 WS-TOTAL-FILE-NET-EDIT PIC $ZZZZZZZZ9.99.
       01 WS-TOTAL-FILE-WH-EDIT PIC $ZZZZZZZZ9.99.
      *	******************************************************************              
                                                                                        
       PROCEDURE DIVISION.  
        
       DISPLAY "CALCPYFL FOR AARON SEAVERS".
       PERFORM 1000-INITIALIZE.
           PERFORM UNTIL END-OF-PAYROLL-FILE
               PERFORM 1010-READINPUT
               PERFORM 1011-CALCULATIONS
               PERFORM 1012-MOVEINSANDCALCS
               PERFORM 1013-WRITETOFILE.

           display "RECORDS READ :                 " WS-RECORDS-READ
      * NEED TO DEBUG WRITTEN RECORDS
           DISPLAY "RECORDS WRITTEN :              " WS-RECORDS-WRITTEN

           MOVE WS-TOTAL-FILE-GROSS TO WS-TOTAL-FILE-GROSS-EDIT
           DISPLAY "TOTAL FILE GROSS:   " WS-TOTAL-FILE-GROSS-EDIT
           MOVE WS-TOTAL-FILE-NET TO WS-TOTAL-FILE-NET-EDIT
           DISPLAY "TOTAL FILE NET:     " WS-TOTAL-FILE-NET-EDIT
           MOVE WS-TOTAL-FILE-WH TO WS-TOTAL-FILE-WH-EDIT
           DISPLAY "TOTAL FILE WITHELD: " WS-TOTAL-FILE-WH-EDIT
           close PAYROLL-IN
           PAYROLL-OUT
           DISPLAY "END CALCPYFL FOR AARON SEAVERS"
           goback.
               
      *    INITIALIZE:  OPEN FILES, PERFORM 1ST READ.
      *    PERFORM LOOP TO:
      *	    -DO CALCULATIONS
      *	    -UPDATE TOTALS
      *     -MOVE INPUT FIELDS AND CALCULATED FIELDS TO OUTPUT-RECORD   
      *	    -WRITE OUT NEW RECORD
      *	    -READ NEXT RECORD
      *	   DISPLAY PROGRAM TOTALS
      *	   CLOSE FILES

       1000-INITIALIZE.
           OPEN INPUT PAYROLL-IN.
           OPEN OUTPUT PAYROLL-OUT.

       1010-READINPUT.
           READ PAYROLL-IN INTO PAYROLL-REC-IN
           AT END 
               DISPLAY "END OF PAYROLL FILE"
           NOT AT END
               ADD 1 TO WS-RECORDS-READ
           end-read.

       1011-CALCULATIONS.
      * ZERO OUT THINGS
           MOVE ZEROES TO WS-OVERTIME-HOURS
           MOVE ZEROES TO WS-OVERTIME-GROSS
           MOVE ZEROES TO WS-NORMAL-PAY-GROSS
           MOVE ZEROES TO WS-TOTAL-GROSS
           MOVE ZEROES TO WS-STATE-TAX-WH
           MOVE ZEROES TO WS-MEDICARE-TAX-WH
           MOVE ZEROES TO WS-LOCAL-TAX-WH
           MOVE ZEROES TO WS-FEDERAL-TAX-WH
      * PAY

          IF PAYROLL-EMP-HOURS-IN IS GREATER THAN 40
             COMPUTE WS-OVERTIME-HOURS = PAYROLL-EMP-HOURS-IN - 40
             COMPUTE WS-OVERTIME-GROSS = (PAYROLL-EMP-RATE-IN * 1.5) * WS-OVERTIME-HOURS
             COMPUTE WS-NORMAL-PAY-GROSS = PAYROLL-EMP-HOURS-IN * PAYROLL-EMP-RATE-IN
             COMPUTE WS-TOTAL-GROSS = WS-OVERTIME-GROSS + WS-NORMAL-PAY-GROSS
             ADD WS-TOTAL-GROSS TO WS-TOTAL-FILE-GROSS
          else
             COMPUTE WS-NORMAL-PAY-GROSS = PAYROLL-EMP-HOURS-IN * PAYROLL-EMP-RATE-IN
             COMPUTE WS-TOTAL-GROSS = PAYROLL-EMP-HOURS-IN * PAYROLL-EMP-RATE-IN
             ADD WS-TOTAL-GROSS TO WS-TOTAL-FILE-GROSS
          end-if

      * TAXES   
          COMPUTE WS-STATE-TAX-WH = WS-TOTAL-GROSS * 0.06
          COMPUTE WS-MEDICARE-TAX-WH = WS-TOTAL-GROSS * .0765
          COMPUTE WS-LOCAL-TAX-WH = WS-TOTAL-GROSS * .01
          if
            PAYROLL-EMP-DEDUCTIONS-IN EQUALS 0 
              COMPUTE WS-FEDERAL-TAX-WH = WS-TOTAL-GROSS * .2
          ELSE IF 
                 PAYROLL-EMP-DEDUCTIONS-IN = 1
                   COMPUTE WS-FEDERAL-TAX-WH = WS-TOTAL-GROSS * .18
               ELSE IF 
                      PAYROLL-EMP-DEDUCTIONS-IN = 2
                        COMPUTE WS-FEDERAL-TAX-WH = WS-TOTAL-GROSS * .15
                    ELSE IF 
                           PAYROLL-EMP-DEDUCTIONS-IN = 3
                             COMPUTE WS-FEDERAL-TAX-WH = WS-TOTAL-GROSS * .12
                         ELSE
                           COMPUTE WS-FEDERAL-TAX-WH = WS-TOTAL-GROSS * .1 
                         END-IF
                    END-IF
               END-IF
          end-if
          ADD WS-TOTAL-WITHHELD TO WS-TOTAL-FILE-WH 
          COMPUTE WS-TOTAL-WITHHELD = WS-STATE-TAX-WH
          + WS-MEDICARE-TAX-WH +WS-LOCAL-TAX-WH
          + WS-FEDERAL-TAX-WH

          COMPUTE WS-TOTAL-NET = WS-TOTAL-GROSS
          - WS-TOTAL-WITHHELD.
           ADD WS-TOTAL-NET TO WS-TOTAL-FILE-NET.

       1012-MOVEINSANDCALCS.
        MOVE PAYROLL-PAY-DATE-IN TO PAYROLL-PAY-DATE-OUT
        MOVE PAYROLL-EMP-DEDUCTIONS-IN TO PAYROLL-EMP-DEDUCTIONS-OUT
        MOVE PAYROLL-EMP-NUMBER-IN TO PAYROLL-EMP-NUMBER-OUT
        MOVE PAYROLL-EMP-HOURS-IN TO PAYROLL-EMP-HOURS-OUT
        MOVE PAYROLL-EMP-RATE-IN TO PAYROLL-EMP-RATE-OUT
        MOVE WS-NORMAL-PAY-GROSS TO PAYROLL-BASE-PAY-OUT
        MOVE WS-OVERTIME-GROSS TO PAYROLL-OVERTIME-OUT
        MOVE WS-TOTAL-GROSS TO PAYROLL-EMP-GROSS-OUT
        MOVE WS-TOTAL-WITHHELD TO PAYROLL-DEDUCT-NET-OUT
        MOVE WS-TOTAL-NET TO PAYROLL-EMP-NET-OUT
        MOVE WS-MEDICARE-TAX-WH TO PAYROLL-EMP-SSN-MED-OUT
        MOVE WS-STATE-TAX-WH TO PAYROLL-EMP-STATE-OUT
        MOVE WS-LOCAL-TAX-WH TO PAYROLL-EMP-LOCAL-OUT
        MOVE WS-FEDERAL-TAX-WH TO PAYROLL-EMP-FED-OUT.


       1013-WRITETOFILE.
           IF END-OF-PAYROLL-FILE EQUALS FALSE
           WRITE PAYROLL-REC-OUT FROM PAYROLL-EMP-CALCULATIONS
           IF GOOD-PAYROLL-FILE-WRITE 
               ADD 1 TO WS-RECORDS-WRITTEN
           ELSE DISPLAY "BAD-WRITE"
           END-IF.


                  
       