000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID.   DATDIVAS.                                                  
000300 AUTHOR.       AARON SEAVERS.                                                    
000400 DATE-WRITTEN. SEPTEMBER 07 2017.                                           
000500*-----------------------------------------------------------------        
000600* CORRECT DATA DIVISION TO MATCH INPUT DATA                               
000700*-----------------------------------------------------------------        
000800 ENVIRONMENT DIVISION.                                                    
000900 DATA DIVISION.                                                           
001000                                                                          
001100 WORKING-STORAGE SECTION.                                                 
001200                                                                          
001300 01  BEGIN-WORKING-STORAGE       PIC X(50)   VALUE                        
001400      '** DATDIVFL BEGIN WORKING STORAGE **'.                             
001500                                                                          
001510 01 WS-DISPLAY-COUNT    PIC 99(01) VALUE 0.                                
001600 01 CUSTOMER-REC.                                                         
001700    05 CUST-ID          PIC X(13).                                        
001800    05 CUST-NAME.                                                         
001900       10 CUST-TITLE    PIC XXX.                                           
001910       10 CUST-INIT     PIC XX.                                           
002000       10 CUST-SURNAME  PIC X(09).                                        
002100    05 CUST-GENDER      PIC X(06).                                        
002200    05 CUST-PAYMENT     PIC 9(06)V9(3).                                     
002300                                                                          
002400 01  END-WORKING-STORAGE         PIC X(50)   VALUE                        
002500      '** DATDEVBL **  END WORKING-STORAGE **'.                           
002600                                                                          
002700 PROCEDURE DIVISION.                                                      
002800                                                                          
002900     DISPLAY 'DATDIVFL EXECUTION BEGINS ON '                              
003000       FUNCTION CURRENT-DATE (1:8) ' AT '                                 
003100       FUNCTION CURRENT-DATE (9:8).                                       
003200                                                                          
003300     PERFORM 2000-INITIALIZE-RECORD.                                      
003301                                   
003600     MOVE '75842'   TO CUST-ID.                                           
003700     MOVE 'MR'      TO CUST-TITLE.                                        
003710     MOVE 'RD'      TO CUST-INIT.                                         
003800     MOVE 'FITZROY' TO CUST-SURNAME.                                      
003810     MOVE 'MALE'    TO CUST-GENDER.                                       
003900     MOVE 34       TO CUST-PAYMENT.                                      
003901     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004100                                                                          
004110     MOVE 82014     TO CUST-ID.                                           
004120     MOVE 'MRS'     TO CUST-TITLE                                         
004130     MOVE 'NM'      TO CUST-INIT.                                         
004140     MOVE 'BAK'     TO CUST-SURNAME.                                      
004150     MOVE 'FEMALE'  TO CUST-GENDER.                                       
004160     MOVE 400045    TO CUST-PAYMENT.                                      
004170     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004180                                                                          
004190     MOVE 'A2214'   TO CUST-ID.                                           
004191     MOVE 'MRS'     TO CUST-TITLE.                                        
004192     MOVE 'KA'      TO CUST-INIT.                                         
004193     MOVE 'RICE'    TO CUST-SURNAME.                                      
004194     MOVE 'FEMALE'  TO CUST-GENDER.                                       
004195     MOVE .110    TO CUST-PAYMENT.                                      
004196     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004197                                                                          
004198     MOVE '225Z2'    TO CUST-ID.                                           
004200     MOVE 'MRS'     TO CUST-TITLE.                                        
004201     MOVE 'OB'      TO CUST-INIT.                                         
004202     MOVE 'KWIAIT'  TO CUST-SURNAME.                                      
004203     MOVE 'FEMALE'  TO CUST-GENDER.                                       
004204     MOVE 2.25      TO CUST-PAYMENT.                                      
004205     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004206                                                                          
004207     MOVE '#15R5'   TO CUST-ID.                                           
004208     MOVE 'MR '     TO CUST-TITLE.                                        
004209     MOVE 'IM'      TO CUST-INIT.                                         
004210     MOVE 'WRIGHT'  TO CUST-SURNAME.                                      
004211     MOVE 'MALE  '  TO CUST-GENDER.                                       
004212     MOVE 7734.34   TO CUST-PAYMENT.                                      
004213     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004214                                                                          
004216     MOVE '575T6'   TO CUST-ID.                                           
004217     MOVE 'MR '     TO CUST-TITLE.                                        
004218     MOVE 'UR'      TO CUST-INIT.                                         
004219     MOVE 'WONG  '  TO CUST-SURNAME.                                      
004220     MOVE 'MALE  '  TO CUST-GENDER.                                       
004221     MOVE 321      TO CUST-PAYMENT.                                      
004222     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004223                                                                          
004224     MOVE '78978'   TO CUST-ID.                                           
004225     MOVE 'MR '     TO CUST-TITLE.                                        
004226     MOVE 'IO'      TO CUST-INIT.                                         
004227     MOVE 'SILVER'  TO CUST-SURNAME.                                      
004228     MOVE 'MALE  '  TO CUST-GENDER.                                       
004229     MOVE .321      TO CUST-PAYMENT.                                      
004230     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004231                                                                          
004232     MOVE '2241A'   TO CUST-ID.                                           
004233     MOVE 'MR '     TO CUST-TITLE.                                        
004234     MOVE 'OH'      TO CUST-INIT.                                         
004235     MOVE 'TAKASHAYAWA'  TO CUST-SURNAME.                                 
004236     MOVE 'MALE  '  TO CUST-GENDER.                                       
004237     MOVE 99.88     TO CUST-PAYMENT.                                      
004238     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004239                                                                          
004240     MOVE 'A1DD3'   TO CUST-ID.                                           
004241     MOVE 'MR '     TO CUST-TITLE.                                        
004242     MOVE 'CF'      TO CUST-INIT.                                         
004243     MOVE 'EYECAIR' TO CUST-SURNAME.                                      
004244     MOVE 'MALE  '  TO CUST-GENDER.                                       
004245     MOVE 11000.1   TO CUST-PAYMENT.                                      
004246     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004247                                                                          
004248     MOVE 'FG134'   TO CUST-ID.                                           
004249     MOVE 'MR '     TO CUST-TITLE.                                        
004250     MOVE 'UB'      TO CUST-INIT.                                         
004251     MOVE 'WALKEN ' TO CUST-SURNAME.                                      
004252     MOVE 'MALE  '  TO CUST-GENDER.                                       
004253     MOVE 8.8       TO CUST-PAYMENT.                                      
004254     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004255                                                                          
004256     MOVE 'FRCDA'   TO CUST-ID.                                           
004257     MOVE 'MRS'     TO CUST-TITLE.                                        
004258     MOVE 'IC'      TO CUST-INIT.                                         
004259     MOVE 'LONDON ' TO CUST-SURNAME.                                      
004260     MOVE 'FEMALE'  TO CUST-GENDER.                                       
004261     MOVE 3.157     TO CUST-PAYMENT.                                      
004262                                                                          
004263     MOVE 'DF111'   TO CUST-ID.                                           
004264     MOVE 'MRS'     TO CUST-TITLE.                                        
004265     MOVE 'IC'      TO CUST-INIT.                                         
004266     MOVE 'FRANCE ' TO CUST-SURNAME.                                      
004267     MOVE 'FEMALE'  TO CUST-GENDER.                                       
004268     MOVE 333       TO CUST-PAYMENT.                                      
004269     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004270                                                                          
004271     MOVE '56321'   TO CUST-ID.                                           
004272     MOVE 'MR '     TO CUST-TITLE.                                        
004273     MOVE 'ES'      TO CUST-INIT.                                         
004274     MOVE 'KIMOPIE' TO CUST-SURNAME.                                      
004275     MOVE 'MALE'    TO CUST-GENDER.                                       
004276     MOVE 3.14      TO CUST-PAYMENT.                                      
004277     PERFORM 1000-DISPLAY-RECORD-FIELDS.                                  
004278                                                                          
004280     DISPLAY 'DATDIVFL EXECUTION CONCLUDES ON '                           
004300       FUNCTION CURRENT-DATE (1:8) ' AT '                                 
004400       FUNCTION CURRENT-DATE (9:8).                                       
004500     GOBACK.                                                              
004510 1000-DISPLAY-RECORD-FIELDS.                                              
004520                                                                          
004521     ADD 1 TO WS-DISPLAY-COUNT.                                           
004530     DISPLAY '--'.                                                        
004540     DISPLAY 'CUSTOMER NUMBER ' WS-DISPLAY-COUNT ':'.                     
004600     DISPLAY 'CUST-ID:        ' CUST-ID.                                  
004610     DISPLAY 'CUST-TITLE:     ' CUST-TITLE.                               
004611     DISPLAY 'CUST-NAME:      ' CUST-NAME.                                
004620     DISPLAY 'CUST-INIT:      ' CUST-INIT.                                
004630     DISPLAY 'CUST-SURNAME:   ' CUST-SURNAME.                             
004640     DISPLAY 'CUST-GENDER:    ' CUST-GENDER.                              
004650     DISPLAY 'CUST-PAYMENT:   ' CUST-PAYMENT.                             
004651                                                                          
004660 2000-INITIALIZE-RECORD.                                                  
004670     MOVE SPACES TO CUSTOMER-REC.                                         
