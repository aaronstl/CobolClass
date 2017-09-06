       program-id. CobolGreeting.

       *>Program to display COBOL greetings
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  IterNum   PIC 9 VALUE 3.

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM DisplayGreeting IterNum TIMES.
           STOP RUN.
   
       DisplayGreeting.
           DISPLAY "Greetings from Aaron Seavers".
           
       end program CobolGreeting.

