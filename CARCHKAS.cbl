       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARCHKAS.
       AUTHOR.  AARON SEAVERS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MakeOfCar        PIC X(10).
       88 Domestic  VALUE "encore", "renegade",
                         "ecosport", "trax". 
       88 Foreign  VALUE "hrv", "500X",
                         "x1", "countryman",
                         "juke". 
       88 OneThousandPoundTowing VALUE "encore", "ecosport","trax".
       88 TwoThousandPoundTowing Value "renegade", "x1".

       PROCEDURE DIVISION.
       Begin.
       DISPLAY "Choose a top Subcompact Crossover from this list:" WITH NO ADVANCING
       display "encore, renegade, ecosport, hrv, 500x, CX7, x1, countryman, juke, trax"
       
       ACCEPT MakeOfCar
       IF Domestic AND OneThousandPoundTowing then
       DISPLAY MakeofCar "is a Top 10 Domestic model with over 1000 lbs towing capacity."
       end-if

       if Domestic AND TwoThousandPoundTowing 
           display MakeOfCar "is a Top 10 Domestic model with 2000 lbs towing capacity"
           else
               if domestic then
           display MakeOfCar "is a top 10 domestic model with no towing capacity"
       end-if


       if Foreign and OneThousandPoundTowing
         DISPLAY MakeOfCar "is a Top 10 import model with 1000 pound towing capacity."
       end-if

       IF Foreign and TwoThousandPoundTowing
         DISPLAY MakeOfCar "is a top 10 import model with 2000 lbs  towing capacity"
       else
           if Foreign
           Display MakeOfCar "is a top 10 import model with no towing capacity"
       end-if

       if Foreign or Domestic
       else
           display MakeOfCar "is not on the top 10 domestic or import list"
       end-if
       END-IF

       STOP RUN.
