       identification division.
       program-id. MATHSHPS.
      * Reads file of rug orders; calculate area and perimeter.
      * Determines price of rug.
      * Write out new file with results.
      * Creates a report of the input. 

       environment division.
       configuration section.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       select SHAPE-FILE-IN
         assign to "C:\Users\bob\MATHSHPS.txt"
      *  assign to "C:\Users\bob\MATHSHPS-BIG.txt" 
      *  assign to "C:\Users\bob\MATHSHPS-BIGGER.txt"          
         organization is line sequential
         file status is WS-SHAPE-FILE-IN-STATUS.
         
       SELECT SHAPE-FILE-OUT
         ASSIGN To "C:\Users\bob\MATHSHPSOUT.dat"
         organization is line sequential
         file status is WS-SHAPE-FILE-OUT-STATUS.
         
       SELECT SHAPE-REPORT
         ASSIGN To "C:\Users\bob\SHAPERPT1.rpt"
         organization is LINE sequential
         file status is WS-SHAPE-REPORT-STATUS.         

       DATA DIVISION.
       FILE SECTION.
       FD  SHAPE-FILE-IN                                    
           RECORDING MODE IS F  
           DATA RECORD IS SHAPE-REC-IN.                  
       01 SHAPE-REC-IN               PIC X(21).
       
       FD  SHAPE-FILE-OUT                                   
           RECORDING MODE IS F  
           DATA RECORD IS SHAPE-REC-OUT.                  
       01 SHAPE-REC-OUT              PIC X(39).
       
       FD  SHAPE-REPORT
           RECORDING MODE IS F
           DATA RECORD IS SHAPE-REPORT-RECORD.                  
       01 SHAPE-REPORT-RECORD        PIC X(133).
       
       working-storage section.
       01  CALCULATED-SHAPES-RECORD.
           05 SHAPE-TYPE             PIC X(10).
           05 SIZE-1                 PIC 9(03).
           05 SIZE-2                 PIC 9(03).
           05 SQ-FT-PRICE            PIC 9(03)V99.
           05 CALCULATED-FIELDS-OUT.
              10 AREA-OUT            PIC 9(06)V99.
              10 PERIMETER-OUT       PIC 9(04).
              10 PRICE-OUT           PIC 9(06)V99.
           
       01  WS-CALCULATION-FIELDS.
           05 WS-RECORD-COUNT-IN      PIC 9(02).
           05 WS-RECORD-COUNT-OUT     PIC 9(02).
           05 WS-TOTAL-FILE-COST      PIC 9(08)V99.
           05 WS-TOTAL-FILE-AREA      PIC 9(08)V99.
           05 WS-TOTAL-FILE-PERIMETER PIC 9(06)V99.
           05 WS-PAGE-COUNT           PIC 999.
           05 WS-LINE-COUNT           PIC 99.
      *
      * STD PAGES ARE 133 CHARACTERS, SO THE SUM OF PIC CLAUSE
      * LENGHTS SHOULD ADD UP TO 133, OR THE WIDTH OF THE REPORT
      * STD OF THE ORGANIZATION. THE HEADING TEXT IS USUALLY CENTERED.
       01  PAGE-HDG-01.
           05 PAGE-HDG-01-CC         PIC X(01).
           05 FILLER                 PIC X(60) VALUE "AARON SEAVERS".    
           05 FILLER                 PIC X(12) VALUE "SHAPE REPORT".
           05 FILLER                 PIC X(30) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE "PAGE: ".
           05 PAGE-HDG-01-PAGE       PIC ZZ9.   
       01  PAGE-HDG-02.
           05 PAGE-HDG-02-CC         PIC X(01).
           05 FILLER                 PIC X(56) VALUE SPACES.
           05 FILLER                 PIC X(10) VALUE "RUN DATE: ".
           05 PAGE-HDG-02-DATE       PIC XX/XX/XXXX.
           05 FILLER                 PIC X(56) VALUE SPACES.
       01  PAGE-HDG-03.
           05 PAGE-HDG-03-CC         PIC X(01).
           05 FILLER                 PIC X(20) VALUE SPACES.
           05 FILLER                 PIC X(05) VALUE "SHAPE".
           05 FILLER                 PIC X(13) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE "SIDE 1". 
           05 FILLER                 PIC X(07) VALUE SPACES.
           05 FILLER                 PIC X(06) VALUE "SIDE 2".
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 FILLER                 PIC X(04) VALUE "AREA".  
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 FILLER                 PIC X(09) VALUE "PERIMETER". 
           05 FILLER                 PIC X(13) VALUE SPACES.
           05 FILLER                 PIC X(05) VALUE "PRICE".
           05 FILLER                 PIC X(05) VALUE SPACES.
           05 FILLER                 PIC X(21) VALUE "PRICE PER SQ FOOT".
           05 FILLER                 PIC X(10) VALUE SPACES.           
           
       01  DETAIL-LINE-SHAPE.
           05 DTL-CC                 PIC X(01).
           05 FILLER                 PIC X(19) VALUE SPACES.
           05 DTL-SHAPE              PIC X(10).
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-SIZE-1       PIC ZZ9.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-SIZE-2       PIC ZZ9.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-AREA         PIC ZZZ,ZZ9.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-PERIMETER    PIC ZZZ,ZZ9.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-PRICE        PIC Z,ZZZ,ZZZ.99.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 DTL-SHAPE-PPSF         PIC ZZZ.99.
           
       01  DETAIL-LINE-LINE. 
           05 FILLER                 PIC X(20) VALUE SPACES.
           05 FILLER                 PIC X(113) VALUE ALL "_".
      * 05 FILLER                 PIC X(21) VALUE SPACES.
           
       01  WS-COST-OUT               PIC $ZZZ,ZZZ.99.
       
       01  TOTALS-LINE.
           05 TOTAL-LINE-CC          PIC X(01).
           05 FILLER                 PIC X(19) VALUE SPACES.
           05 FILLER                 PIC X(11) VALUE "FILE TOTALS".
           05 FILLER                 PIC X(06) VALUE SPACES.
           05 FILLER                 PIC X(14) VALUE "RECORD COUNT: ".
           05 TOTAL-LINE-COUNT       PIC Z,ZZ9.
           05 FILLER                 PIC X(07) VALUE SPACES.
           05 TOTAL-LINE-AREA        PIC ZZ,ZZZ,ZZ9.
           05 FILLER                 PIC X(10) VALUE SPACES.
           05 TOTAL-LINE-PERIMETER   PIC ZZZ,ZZ9.
           05 FILLER                 PIC X(08) VALUE SPACES.
           05 TOTAL-LINE-PRICE       PIC $$$,ZZZ,ZZ9.99.
                                                                        
       
       01  WS-SHAPE-FILE-IN-STATUS   pic X(02).
           88 SHAPE-FILE-IN-SUCCESSFUL         VALUE "00".
           88 END-OF-SHAPE-FILE                VALUE "10".
           88 INVALID-SHAPE-IN-FILE            VALUE "11" THRU "99".
           88 SHAPE-FILE-NOT-READY             VALUE "01" THRU "99".
           
       01  WS-SHAPE-FILE-OUT-STATUS  pic X(02).
           88 GOOD-SHAPE-FILE-WRITE            VALUE "00".
       01  WS-SHAPE-REPORT-STATUS    pic X(02).
           88 GOOD-SHAPE-REPORT                VALUE "00".           
       
       01 WS-RUN-DATE                PIC X(08).
       01 WS-RULER                   PIC X(39)
          VALUE "----+----1----+----2----+----3----+----".
       01 WS-REPORT-RULER.
          05 FILLER                  PIC X(50)
             VALUE "----+----1----+----2----+----3----+----4----+----5".
          05 FILLER                  PIC X(50)
             VALUE "----+----6----+----7----+----8----+----9----+----0".
          05 FILLER                  PIC X(33)
             VALUE "----+----1----+----2----+----3---".                                                                         
           
       procedure division.

           DISPLAY "START SHAPERPT".
           
           PERFORM XXXX-INITIALIZE.
           PERFORM XXXX-READ-SHAPES.
           
           perform until END-OF-SHAPE-FILE
             perform XXXX-CALCULATE-FIELDS
             perform XXXX-WRITE-SHAPES-RECORD
             perform XXXX-CREATE-DETAIL-LINE
             perform XXXX-READ-SHAPES
           END-PERFORM.
           
           write SHAPE-REC-OUT from WS-RULER.
           perform XXXX-CREATE-REPORT-TOTAL-LINE.
           move WS-TOTAL-FILE-COST to WS-COST-OUT.
           
           display " FILE COST      : " WS-COST-OUT.
           display " RECORDS READ   : " WS-RECORD-COUNT-IN.
           display " RECORDS WRITTEN: " WS-RECORD-COUNT-OUT.
           display "END OF SHAPERPT".
           
           close SHAPE-FILE-IN
                 SHAPE-FILE-OUT
                 SHAPE-REPORT.
         goback.
             
       XXXX-CALCULATE-FIELDS.
           COMPUTE AREA-OUT      = SIZE-1 * SIZE-2.
           COMPUTE PERIMETER-OUT = (SIZE-1 * 2) + (SIZE-2 * 2).
           COMPUTE PRICE-OUT     = AREA-OUT * SQ-FT-PRICE.
           compute WS-TOTAL-FILE-COST
                                 = WS-TOTAL-FILE-COST + PRICE-OUT.
           compute WS-TOTAL-FILE-AREA
                                 = WS-TOTAL-FILE-AREA + AREA-OUT.
           compute WS-TOTAL-FILE-PERIMETER
                                 = WS-TOTAL-FILE-PERIMETER +
                                   PERIMETER-OUT. 
           
       XXXX-READ-SHAPES.
           read SHAPE-FILE-IN into CALCULATED-SHAPES-RECORD
             at end
               display "END OF SHAPE FILE"
             not AT end
               add 1 to WS-RECORD-COUNT-IN.
               
       XXXX-WRITE-SHAPES-RECORD.
           WRITE SHAPE-REC-OUT FROM CALCULATED-SHAPES-RECORD
             after advancing 01 LINES.
           if GOOD-SHAPE-FILE-WRITE 
              add 1 to WS-RECORD-COUNT-OUT
           else  
              display "BAD WRITE - FILE STATUS: " 
                WS-SHAPE-FILE-OUT-STATUS.
                
       XXXX-CREATE-DETAIL-LINE.
           move SHAPE-TYPE    to DTL-SHAPE.
           move SIZE-1        to DTL-SHAPE-SIZE-1.
           move SIZE-2        to DTL-SHAPE-SIZE-2.
           move AREA-OUT      to DTL-SHAPE-AREA.
           move PERIMETER-OUT to DTL-SHAPE-PERIMETER.
           move PRICE-OUT     to DTL-SHAPE-PRICE.
           MOVE SQ-FT-PRICE   TO DTL-SHAPE-PPSF.
           
           add 1 to WS-LINE-COUNT.
           if WS-LINE-COUNT > 50
             PERFORM XXXX-WRITE-HEADINGS.
           
           write SHAPE-REPORT-RECORD from DETAIL-LINE-SHAPE
             after advancing 01 lines.
          
           
       XXXX-CREATE-REPORT-TOTAL-LINE.
           MOVE WS-RECORD-COUNT-OUT TO TOTAL-LINE-COUNT.
           MOVE WS-TOTAL-FILE-AREA  TO TOTAL-LINE-AREA.
           MOVE WS-TOTAL-FILE-PERIMETER
                                    TO TOTAL-LINE-PERIMETER.
           MOVE WS-TOTAL-FILE-COST  TO TOTAL-LINE-PRICE.
           write SHAPE-REPORT-RECORD from DETAIL-LINE-LINE
             AFTER ADVANCING 01 LINES.
           write SHAPE-REPORT-RECORD from TOTALS-LINE
            AFTER ADVANCING 02 LINES.
           write SHAPE-REPORT-RECORD from WS-REPORT-RULER
             after advancing 01 LINES.                                                                          
       
       XXXX-INITIALIZE.
           ACCEPT WS-RUN-DATE FROM DATE.
           MOVE   WS-RUN-DATE TO   PAGE-HDG-02-DATE.
           OPEN INPUT  SHAPE-FILE-IN.
           OPEN OUTPUT SHAPE-FILE-OUT
                       SHAPE-REPORT.
           MOVE ZEROES to WS-CALCULATION-FIELDS 
                          CALCULATED-FIELDS-OUT.
           WRITE SHAPE-REC-OUT FROM WS-RULER.
           PERFORM XXXX-WRITE-HEADINGS.
       
       XXXX-WRITE-HEADINGS.
           add 1 to WS-PAGE-COUNT.
           move WS-PAGE-COUNT TO PAGE-HDG-01-PAGE.
           WRITE SHAPE-REPORT-RECORD FROM WS-REPORT-RULER
             after advancing page.
           WRITE SHAPE-REPORT-RECORD 
            FROM PAGE-HDG-01 after advancing 01 LINES.
           WRITE SHAPE-REPORT-RECORD
            FROM PAGE-HDG-02 after advancing 01 LINES.
           WRITE SHAPE-REPORT-RECORD FROM PAGE-HDG-03
            AFTER ADVANCING 02 LINES.
           MOVE 4 TO WS-LINE-COUNT.