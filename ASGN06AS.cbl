       identification division.
       program-id. ASGN06AS.
       AUTHOR.     AARON SEAVERS.
      * ASSIGNMENT 6 - MATH CALCULATIONS
       environment division.
       configuration section.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                   
       data division.
       working-storage section.
          
           
       01  WS-FN-FL-WORKING-STORAGE.
      * Add & Modify working storage elements as needed.  Some
      * required elements are coded incorrectly, others are missing.
           05 WS-INPUT-1           pic 9(03)    VALUE 0.
           05 WS-INPUT-2           pic 9(03)    VALUE 0.
           05 WS-AVG               pic 9(03)    Value 0.
           05 WS-ROUNDAVG          pic 9(02)    Value 0.
           05 WS-YEAR              pic 9(04)    Value 0.
           05 ws-denominator       pic 9(01)    value 2.
           05 WS-AGE-IN            PIC 9(01)    VALUE 0.
           05 WS-WEIGHT-IN         PIC 9(02).
           05 WS-INCH-HGT-IN       PIC 9(01).
           05 WS-LAST-LEAP-YEAR    PIC 9(04)    VALUE 2016.
           05 WS-FIRSTNAME         PIC X(10).
           05 WS-LASTNAME          PIC X(10).
           05 ws-years1            pic 9(1)  value 4.
           05 ws-years2            pic 9(1)  value 8.
           05 ws-years3            pic 9(02)  value 12.
           05 ws-years4            pic 9(02)  value 16.
           05 ws-NEXTLEAPYEAR      pic 9(04)  value 0.
           05 WS-CURRENT-YEAR      pic 9(04)  value 0.
           
           
       01  WS-TIME.
           05 WS-TIME-HH           PIC X(02).
           05 WS-TIME-MM           PIC X(02).
           05 WS-TIME-SS           PIC X(02).
           05 WS-TIME-HS           PIC X(02).

       01  WS-TODAYS-DATE.
           05 WS-8-DATE-YEAR.
              10 WS-8-DATE-CC      PIC 9(02).
              10 WS-8-DATE-YY      PIC 9(02).
           05 WS-8-DATE-MM         PIC 9(02).
           05 WS-8-DATE-DD         PIC 9(02).
        
       procedure division.

           Move "Aaron" to WS-FIRSTNAME.
           Move "Seavers" to WS-LASTNAME.
           DISPLAY 'START ASGNO6FL FOR '  WS-FIRSTNAME WS-LASTNAME.
           ACCEPT WS-TODAYS-DATE from date yyyymmdd.
           DISPLAY "PROGRAM EXECUTION DATE: " WS-TODAYS-DATE.
           ACCEPT WS-TIME from time.
           DISPLAY "PROGRAM EXECUTION START TIME: " WS-TIME-HH
                   ":" WS-TIME-MM.
      * COMPLETE THE PROCEDURE DIVISION TO PERFORM THESE CALCULATIONS.
      * THIS WILL ALSO INCLUDE THE COMPLETION OF SOME WORKING STORAGE
      * ELEMENTS, AND THE CREATION OF SOME ELEMENTS. 
        
      * CALCULATE THE THE AVERAGE, UP TO 2 DECIMAL PLACES, OF 
      * TWO 2 DIGIT NUMBERS, ONCE USING THE ROUNDED STATEMENT, 
      * THE 2ND WITHOUT ROUNDING.
           DISPLAY "CALCULATE THE AVG OF 2 WHOLE NUMBERS". 
           ACCEPT WS-INPUT-1
           ACCEPT WS-INPUT-2
           COMPUTE WS-AVG = (WS-INPUT-1 + WS-INPUT-2) /ws-denominator.
           DISPLAY "The Average of " WS-INPUT-1 " and " WS-INPUT-2 
                   " is " WS-AVG
      *     check what he said about rounded ave
           COMPUTE WS-ROUNDAVG =(WS-INPUT-1 + WS-INPUT-2) / ws-denominator.
           DISPLAY "The Rounded Average of " WS-INPUT-1 " and " WS-INPUT-2 
                   " is " WS-RoundAVG
           
           DISPLAY "2016 WAS A LEAP YEAR. CALCULATE AND DISPLAY EACH OF The NEXT 4 LEAP YEARS: ".
           Compute WS-LAST-LEAP-YEAR = WS-LAST-LEAP-YEAR + ws-years1.
           DISPLAY "NEXT LEAP YEAR: " WS-LAST-LEAP-YEAR.
           move 2016 to WS-LAST-LEAP-YEAR.
           COMPUTE WS-LAST-LEAP-YEAR = WS-LAST-LEAP-YEAR + ws-years2
           DISPLAY "2ND LEAP YEAR FROM NOW: " WS-LAST-LEAP-YEAR
           move 2016 to WS-LAST-LEAP-YEAR.
           COMPUTE WS-LAST-LEAP-YEAR = WS-LAST-LEAP-YEAR + WS-years3.
           DISPLAY "3RD LEAP YEAR FROM NOW: " WS-LAST-LEAP-YEAR 
           move 2016 to WS-LAST-LEAP-YEAR.
           COMPUTE WS-LAST-LEAP-YEAR = WS-LAST-LEAP-YEAR +  ws-years4.
           DISPLAY "4TH LEAP YEAR FROM NOW: " WS-LAST-LEAP-YEAR
           move 2016 to WS-LAST-LEAP-YEAR.
           compute WS-LAST-LEAP-YEAR = WS-LAST-LEAP-YEAR + ws-years1.

           move WS-8-DATE-YEAR to WS-CURRENT-YEAR.
           compute ws-NEXTLEAPYEAR = WS-LAST-LEAP-YEAR - WS-CURRENT-YEAR.
           
          
       DISPLAY "It is " ws-NEXTLEAPYEAR " years to the next leap year".
           
           
      * ASK THE USER FOR THEIR AGE.  DISPLAY HOW OLD THEY
      * WILL BE FOR THE NEXT 2 LEAP YEARS.
           ACCEPT WS-USER-AGE.
      *    .......
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " ON NEXT LEAP YEAR".
      *    .......
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " IN 2 LEAP YEARS".
           
        DISPLAY FOR THE USER HOW MANY YEARS UNTIL THEY TURN 100
           .......
           
      * PROMPT THE USE FOR THEIR WEIGHT IN POUNDS AND HEIGHT IN INCHES.
      * CALCULATE AND DISPLAY THE USERS BMI TO TWO DECIMAL PLACES.
      * LOOK UP FORMULA ON WEB AND USE COBOL COMPUTE STATEMENT TO 
      * TO CALCULATE THE BMI.
      *    ACCEPT ......
      *    ACCEPT ......
      *    COMPUTE ......
      *    DISPLAY "YOUR BMI IS: " .......
           
      * FOR 5 EXTRA POINTS, CONVERT THE INPUT WEIGHT & HEIGHT TO 
      * METRIC MEASUREMENTS, DISPLAY THEM AND METRIC BMI.
           
               DISPLAY "END OF ASSIGNMENT 06".
           GOBACK.