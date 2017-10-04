       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZipCodeAS.
       AUTHOR.  Aaron Seavers.
       CONFIGURATION SECTION.
 
       SPECIAL-NAMES.
       CLASS ZipCodeClass IS "0" THRU "9", "A" THRU "Z", "a" Thru "z".
       
       DATA DIVISION.


       WORKING-STORAGE SECTION.

       
       
       01  ZipCode PIC X(5) VALUE ZERO.
      *2 DAYS
           88 StLouisArea       VALUE 63000 thru 63999. 
           88 KansasCityArea       VALUE 64000 thru 64999.
           88 ChicagoArea     VALUE 60000 thru 60999.
      *3 Days
           88 OutStateMissouri       VALUE 65000 thru 65999.
           88 OutStateIllinois    VALUE 61000 thru 61999.
           88 EastStLouis    VALUE 62000 thru 62999.
      *4 Days    
           88 OutsideBiStateArea   value 10000 thru 59999.
           



       PROCEDURE DIVISION.
       Begin.
       DISPLAY
         "Enter a five digit zip code to find shipping times - "
       
       ACCEPT ZipCode
       

       if ZipCode is not numeric
           if zipCode is ZipCodeClass
               display "only enter numbers"
               goback
           else if zipCode is not zipCodeClass
               display "you didnt enter enough numbers"
      * so close!!!
               goback
               else
                   display "You didnt enter enough values and you included a letter."
               goback

       end-if
       end-if
       end-if
           
        
           
       if OutsideBiStateArea 
           display "You are from outside of the bi- state area"
           Display " Your Zipcode " ZipCode " is 4 day shipping."
       else if
              OutStateMissouri
                display "you are from -65 out state Missouri."
                Display " Your Zipcode " ZipCode " is 3 day shipping."
            else if 
                   OutStateIllinois
                     display "you are from -61 out state illinois"
                     display "your zipcode " ZipCode " is 3 day shipping"
                 else if
                        EastStLouis
                          display "you are from -62 east st louis"
                          display "your zipcode " Zipcode " is 3 day shipping"
                      else if
                             stlouisarea
                               DISPLAY "You are from the -63  St. Louis Area."
                               Display " Your Zipcode " ZipCode " is 2 day shipping."
                           else if
                                  KansasCityArea
                                    display "You are from the -64 Kansas City Area."
                                   Display " Your Zipcode " ZipCode " is 2 day shipping."
                                   else if ChicagoArea
                                       Display "You are from the -60  Chicago Area."
                                       Display " Your Zipcode " ZipCode " is 2 day shipping."
                                       else
                                           display "You did not enter a valid zip code. Please try again."
                                   end-if
                                end-if
                           end-if
                      end-if
                 end-if
            end-if
       end-if
       goback