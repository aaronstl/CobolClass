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
           05 WS-AVG               pic 9999v99    Value 0.
           05 WS-ROUNDAVG          pic 9(02)    Value 0.
           05 WS-YEAR              pic 9(04)    Value 0.
           05 ws-denominator       pic 9(01)    value 2.
           05 WS-AGE-IN            PIC 9(01)    VALUE 0.
           05 WS-WEIGHT-IN         PIC 9(04).
           05 WS-INCH-HGT-IN       PIC 9(03).
           05 WS-LAST-LEAP-YEAR    PIC 9(04)    VALUE 2016.
           05 ws-bmi-imperial      pic 99v99 value 0.
           05 WS-FIRSTNAME         PIC X(10).
           05 WS-LASTNAME          PIC X(10).
           05 ws-years1            pic 9(1)  value 4.
           05 ws-years2            pic 9(1)  value 8.
           05 ws-years3            pic 9(02)  value 12.
           05 ws-years4            pic 9(02)  value 16.
           05 ws-NEXTLEAPYEAR      pic 9(04)  value 0.
           05 WS-CURRENT-YEAR      pic 9(04)  value 0.
           05 ws-metric-height     pic 9(04)  value 0.
           05 ws-metric-height-conversion pic 9v99   value 2.54.
           05 ws-metric-weight     pic 999v99 value 0.
           05 ws-metric-weight-conversion pic 9v99999999 value 0.45359237.
           05 ws-metric-bmi        pic 99v9999999 value 0.
           

       01 WS-USER-AGE              pic 9(03)  value 0.
       01 ws-years-old             pic 9(03) value 0.
       01 ws-century-age           pic 9(03) value 0.
       01 ws-century               pic 9(03) value 100.
           
           
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
      
           compute WS-AVG rounded = (ws-input-1 + ws-input-2)/ws-denominator.
      *    COMPUTE WS-ROUNDAVG =(WS-INPUT-1 + WS-INPUT-2) / ws-denominator.
           DISPLAY "The Rounded Average of " WS-INPUT-1 " and " WS-INPUT-2 
                   " is " WS-AVG
           
           DISPLAY "2016 WAS A LEAP YEAR. CALCULATE AND DISPLAY EACH OF The NEXT 4 LEAP YEARS: ".
           Compute WS-NextLeapyear = WS-LAST-LEAP-YEAR + ws-years1.
           DISPLAY "NEXT LEAP YEAR: " WS-NextLeapyear.
           COMPUTE WS-nextleapyear = ws-nextleapyear + ws-years1.
           DISPLAY "2ND LEAP YEAR FROM NOW: " WS-nextleapyear
      *    move 2016 to WS-LAST-LEAP-YEAR.
           COMPUTE WS-nextleapyear = WS-nextleapyear + WS-years1.
           DISPLAY "3RD LEAP YEAR FROM NOW: " WS-nextleapyear 
      *    move 2016 to WS-LAST-LEAP-YEAR.
           COMPUTE WS-nextleapyear = WS-nextleapyear +  ws-years1.
           DISPLAY "4TH LEAP YEAR FROM NOW: " WS-nextleapyear
          move 2016 to WS-LAST-LEAP-YEAR.
           compute WS-LAST-LEAP-YEAR = WS-LAST-LEAP-YEAR + ws-years1.

           move WS-8-DATE-YEAR to WS-CURRENT-YEAR.
           compute ws-NEXTLEAPYEAR = WS-LAST-LEAP-YEAR - WS-CURRENT-YEAR.
           
          
       DISPLAY "It is " ws-NEXTLEAPYEAR " years to the next leap year".
           
           
      * ASK THE USER FOR THEIR AGE.  DISPLAY HOW OLD THEY
      * WILL BE FOR THE NEXT 2 LEAP YEARS.
           Display "Please enter your age:"
          ACCEPT WS-USER-AGE.
         compute  ws-years-old = ws-user-age + ws-nextleapyear.
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " ON NEXT LEAP YEAR".
           compute ws-years-old = WS-USER-AGE + ws-years1 + ws-years1.
           DISPLAY "YOU WILL BE: " WS-YEARS-OLD " IN 2 LEAP YEARS".
           
        DISPLAY "HOW MANY YEARS UNTIL THEY TURN 100".
           compute ws-century-age = ws-century - ws-user-age.
        display "It will be " ws-century-age " years until you turn 100".
           
      * PROMPT THE USE FOR THEIR WEIGHT IN POUNDS AND HEIGHT IN INCHES.
      * CALCULATE AND DISPLAY THE USERS BMI TO TWO DECIMAL PLACES.
      * LOOK UP FORMULA ON WEB AND USE COBOL COMPUTE STATEMENT TO 
      * TO CALCULATE THE BMI.
        Display "Please enter your weight;".
           ACCEPT WS-WEIGHT-IN
       Display "please enter your height in inches:".
           ACCEPT WS-INCH-HGT-IN
           COMPUTE ws-bmi-imperial = (WS-WEIGHT-IN *703)/ (WS-INCH-HGT-IN**2)
           DISPLAY "YOUR BMI IS: " ws-bmi-imperial.
           
      * FOR 5 EXTRA POINTS, CONVERT THE INPUT WEIGHT & HEIGHT TO 
      * METRIC MEASUREMENTS, DISPLAY THEM AND METRIC BMI.
       
       compute ws-metric-height = ws-inch-hgt-in * ws-metric-height-conversion.
       Display "Your height in metric is:" ws-metric-height.
       compute ws-metric-weight = WS-WEIGHT-IN * ws-metric-weight-conversion.
       Display "Your weight in metric is " ws-metric-weight.
       compute ws-metric-bmi = ws-metric-weight / (ws-metric-height **2).
       compute ws-metric-bmi = ws-metric-bmi * 10000.
           display "your metric bmi is " ws-metric-bmi.
       
           
               DISPLAY "END OF ASSIGNMENT 06".
           GOBACK.
