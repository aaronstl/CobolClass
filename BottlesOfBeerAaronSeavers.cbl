       program-id. "BottlesOfBeerAaronSeavers".

       data division.

       working-storage section.
       01 WS-BEERS-INPUT PIC XXX.
       01 WS-BEERS-INT REDEFINES
           WS-BEERS-INPUT PIC 999.
       01 WS-ONELESSBEER PIC 99 VALUE 0.

       procedure division.
       
       perform 1005-INPUTCHECK.
       
       

       1005-INPUTCHECK.
           DISPLAY "HOW MANY BEERS ARE ON THE WALL? ENTER 00-099"
           accept WS-BEERS-INPUT
           IF WS-BEERS-INPUT IS NOT numeric
               DISPLAY "THE VALUE ENTERED IS NOT NUMERIC"
           PERFORM 1005-INPUTCHECK
           ELSE IF WS-BEERS-INPUT IS GREATER THAN 099
                    display "ENTER A 099 OR LOWER"
           ELSE IF WS-BEERS-INPUT IS NUMERIC AND WS-BEERS-INPUT IS LESS THAN 100
                    PERFORM 1010-SINGBEERSONG
                END-IF
           END-IF.
           
           

       1010-SINGBEERSONG.
           PERFORM VARYING WS-BEERS-INT FROM WS-BEERS-INT BY -1 UNTIL WS-BEERS-INT
               equals 0

               IF WS-BEERS-INT EQUALS 1
                    DISPLAY WS-BEERS-INT " BOTTLE OF BEER ON THE WALL, "
                   WS-BEERS-INT " BOTTLE OF BEER. TAKE ONE DOWN, PASS IT"
                   DISPLAY " AROUND, NO MORE BOTTLES OF BEER ON THE"
                   DISPLAY "WALL. NO MORE BOTTLES OF BEER ON THE WALL,"
                   DISPLAY "NO MORE BOTTLES OF BEER. GET THE HAT"
                   DISPLAY " AND PASS IT AROUND"
             DISPLAY " TIME TO B DOUBLE EE DOUBLE R-U-N, BEER RUN."
               goback
               ELSE
               display WS-BEERS-INT " BOTTLES OF BEER ON THE WALL, "
               WS-BEERS-INT " BOTTLES OF BEER. TAKE ONE DOWN,"
               DISPLAY " PASS IT AROUND"
               MOVE WS-BEERS-INT TO WS-ONELESSBEER
               subtract 1 FROM WS-ONELESSBEER
               IF WS-ONELESSBEER IS GREATER THAN 1
               display WS-ONELESSBEER " BOTTLES OF BEER ON THE WALL"
               ELSE DISPLAY "ONE BOTTLE OF BEER ON THE WALL."
                   
                  
            
             
           
       end program BottlesOfBeerAaronSeavers.
