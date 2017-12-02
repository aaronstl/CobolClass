       program-id. PAYRDFAS.
      
       
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.  
           SELECT PRESIDENT-FILE-IN                                                 
             ASSIGN TO "C:\Users\Bob\USPRES.DAT"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-PRES-FILE-STATUS.
       
       data division.
       FILE SECTION.                                                            
       FD  PRESIDENT-FILE-IN                                                      
           RECORDING MODE IS F                                                  
           DATA RECORD IS PRES-INFO.
     
           01 PRES-INFO-IN. 
              05 PRES-NUM-IN    PIC X(02).
              05 PRES-NAME-IN   PIC X(27).
              

           

          
          

       working-storage section.

        01 PRES-RECORD-TABLE.
             05 PRES-NAME PIC X(27) OCCURS 45 TIMES.
             

       01 WS-PRES-FILE-STATUS PIC X(02).
           88 END-OF-PRES-FILE VALUE "10".

      \01 WS-PRES-SUB          PIC 9(02) VALUE 1.
       01 WS-PRES-SEARCH-INPUT    PIC X(02).
       01 WS-PRES-SEARCH-INPUT-NO REDEFINES WS-PRES-SEARCH-INPUT PIC 9(02).
      
      
       procedure division.

           PERFORM 1000-LOAD-TABLE.
           PERFORM 1100-PROMPT-USER.
           
       1000-LOAD-TABLE.
           OPEN INPUT PRESIDENT-FILE-IN.
           PERFORM UNTIL WS-PRES-SUB EQUALS 46
             READ PRESIDENT-FILE-IN INTO PRES-INFO-IN
              MOVE PRES-NAME-IN TO PRES-NAME(WS-PRES-SUB)
              ADD 1 TO WS-PRES-SUB.

       1100-PROMPT-USER.
           DISPLAY "CHOOSE A PRESIDENT BY ENTERING THEIR NUMBER (01-45).".
           DISPLAY "LIST ALL PRESIDENTS BY ENTERING 'LA'. ".
           DISPLAY "PRESS X TO EXIT THE PROGRAM".
           ACCEPT WS-PRES-SEARCH-INPUT
           IF WS-PRES-SEARCH-INPUT IS NOT NUMERIC 
            IF WS-PRES-SEARCH-INPUT equals "X"
                goback
            ELSE IF WS-PRES-SEARCH-INPUT EQUALS "LA"
                PERFORM 1111-DISPLAY-ALL-PRESIDENTS
                PERFORM 1100-PROMPT-USER
            END-IF
           else
               PERFORM 1110-DISPLAY-USER-INPUT
               PERFORM 1100-PROMPT-USER
            end-if.
       
       1110-DISPLAY-USER-INPUT.
           DISPLAY "YOU CHOSE: " PRES-NAME(WS-PRES-SEARCH-INPUT-NO).

       1111-DISPLAY-ALL-PRESIDENTS.
           MOVE 01 TO WS-PRES-SUB.
           PERFORM VARYING WS-PRES-SUB FROM 1 BY 1 UNTIL WS-PRES-SUB EQUALS 46
               DISPLAY PRES-NAME(WS-PRES-SUB).
