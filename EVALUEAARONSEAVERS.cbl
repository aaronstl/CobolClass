       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EVALUEAARONSEAVERS.
       AUTHOR.  Aaron Seavers.
       DATE-WRITTEN. 09-07-2017.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CharIn             PIC X.
       88 Vowel           VALUE "a", "e", "i", "o", "u".
       88 Consonant       VALUE "b", "c", "d", "f", "g", "h"
                             "j" THRU "n", "p" THRU "t", "v" THRU "z".
       88 Digit           VALUE "0" THRU "9".
       88 ValidCharacter  VALUE "a" THRU "z", "0" THRU "9".
       


       PROCEDURE DIVISION.
       0000-MAIN.
       DISPLAY "Enter lower case character or digit. Invalid char ends."
       ACCEPT CharIn
       PERFORM UNTIL NOT ValidCharacter
           evaluate true

      
        WHEN Vowel     DISPLAY "The letter " CharIn " is a vowel."
        WHEN Consonant DISPLAY "The letter " CharIn " is a consonant."
        WHEN Digit     DISPLAY CharIn " is a digit."
       end-evaluate

       accept CharIn
       end-perform
       goback.
   
