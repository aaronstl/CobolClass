000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ZipCodeAS.
000300 AUTHOR.  Aaron Seavers.
000400 CONFIGURATION SECTION.
000500
000600 SPECIAL-NAMES.
000700 CLASS ZipCodeClass
000800 IS "0" THRU "9","A" THRU "Z", "a" Thru "z".
000900 
001000 DATA DIVISION.
001100
001200 WORKING-STORAGE SECTION.
001300 01  ZipCode PIC X(5) VALUE ZERO.
001400*2 DAYS
001500     88 StLouisArea     VALUE 63000 thru 63999. 
001600     88 KansasCityArea  VALUE 64000 thru 64999.
001700     88 ChicagoArea     VALUE 60000 thru 60999.
001800*3 Days
001900     88 OutStateMissouri    VALUE 65000 thru 65999.
002000     88 OutStateIllinois    VALUE 61000 thru 61999.
002100     88 EastStLouis         VALUE 62000 thru 62999.
002200*4 Days    
002300     88 OutsideBiStateArea   value 10000 thru 59999.
002400     
002500 PROCEDURE DIVISION.
002600 Begin.
002700 DISPLAY
002800   "Enter a five digit zip code to find shipping times - "
002900 
003000 ACCEPT ZipCode
003100 
003200 if ZipCode is not numeric
003300     if zipCode is ZipCodeClass
003400         display "only enter numbers"
003500         goback
003600     else if zipCode is not zipCodeClass
003700         display "you didnt enter enough numbers"
003800         goback
003900 end-if
004000 end-if
004100 end-if
004200
004300 if OutsideBiStateArea 
004400     display "You are from outside of the bi- state area"
004500     Display " Your Zipcode " ZipCode " is 4 day shipping."
004600 else if
004700    OutStateMissouri
004800      display "you are from -65 out state Missouri."
004900      Display " Your Zipcode " ZipCode " is 3 day shipping."
005000      else if 
005100        OutStateIllinois
005200          display "you are from -61 out state illinois"
005300          display "your zipcode " ZipCode " is 3 day shipping"
005400           else if
005500             EastStLouis
005600              display "you are from -62 east st louis"
005700              display "your zipcode " Zipcode " is 3 day shipping"
005800                else if
005900                 stlouisarea
006000                   DISPLAY "You are from the -63  St. Louis Area."
006100                   Display " Your Zipcode " ZipCode " is 2 day shipping."
006200                     else if
006300                       KansasCityArea
006400                        display "You are from the -64 Kansas City Area."
006500                         Display " Your Zipcode " ZipCode " is 2 day shipping."
006600                          else if ChicagoArea
006700                            Display "You are from the -60  Chicago Area."
006800                            Display " Your Zipcode " ZipCode " is 2 day shipping."
006900                             else
007000                              display "You did not enter a valid zip code. Please try again."
007100                             end-if
007200                          end-if
007300                     end-if
007400                end-if
007500           end-if
007600      end-if
007700 end-if
007800 goback