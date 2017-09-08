       program-id. DOCALCAARONSEAVERS.

       AUTHOR. AARON SEAVERS.

       data division.
       working-storage section.

       01 FirstNum       PIC 9     VALUE ZEROS.
       01 SecondNum      PIC 9     VALUE ZEROS.
       01 CalcResult     PIC 99    VALUE 0.
       01 UserPrompt     PIC X(38) VALUE
                  "Please enter two single digit numbers".

       PROCEDURE DIVISION.
       CalculateResult.
       DISPLAY UserPrompt
       ACCEPT FirstNum
       ACCEPT SecondNum
       COMPUTE CalcResult = FirstNum * SecondNum
       DISPLAY "Mulitiplication Result is: " CalcResult.


           goback.
           
       end program DOCALCAARONSEAVERS.
