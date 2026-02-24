       IDENTIFICATION DIVISION.                                         00010001
                                                                        00020001
       PROGRAM-ID. RPT2000.                                             00030001
                                                                        00040001
       ENVIRONMENT DIVISION.                                            00050001
                                                                        00060001
       INPUT-OUTPUT SECTION.                                            00070001
                                                                        00080001
       FILE-CONTROL.                                                    00090001
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00100001
           SELECT SALESRPT ASSIGN TO RPOT2000.                          00110001
                                                                        00120001
       DATA DIVISION.                                                   00130001
                                                                        00140001
       FILE SECTION.                                                    00150001
                                                                        00160001
       FD  CUSTMAST                                                     00170001
           RECORDING MODE IS F                                          00180001
           LABEL RECORDS ARE STANDARD                                   00190001
           RECORD CONTAINS 130 CHARACTERS                               00200001
           BLOCK CONTAINS 130 CHARACTERS.                               00210001
       01  CUSTOMER-MASTER-RECORD.                                      00220001
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00230001
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00240001
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00250001
           05  CM-CUSTOMER-NAME        PIC X(20).                       00260001
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00270001
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00280001
           05  FILLER                  PIC X(87).                       00290001
                                                                        00300001
       FD  SALESRPT                                                     00310001
           RECORDING MODE IS F                                          00320001
           LABEL RECORDS ARE STANDARD                                   00330001
           RECORD CONTAINS 130 CHARACTERS                               00340001
           BLOCK CONTAINS 130 CHARACTERS.                               00350001
       01  PRINT-AREA      PIC X(130).                                  00360001
                                                                        00370001
       WORKING-STORAGE SECTION.                                         00380001
                                                                        00390001
                                                                        00400001
       01 CALCULATED-FIELDS.                                            00410001
          05 CHANGE-AMOUNT             PIC S9(5)V99.                    00420001
       01  SWITCHES.                                                    00430001
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00440001
                                                                        00450001
       01  PRINT-FIELDS.                                                00460001
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00470001
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00480001
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00490001
           05  SPACE-CONTROL   PIC S9.                                  00500001
                                                                        00510001
       01  TOTAL-FIELDS.                                                00520001
           05  GRAND-TOTAL-THIS-YTD   PIC S9(7)V99   VALUE ZERO.        00530001
           05  GRAND-TOTAL-LAST-YTD   PIC S9(7)V99   VALUE ZERO.        00540001
                                                                        00550001
       01  CURRENT-DATE-AND-TIME.                                       00560001
           05  CD-YEAR         PIC 9999.                                00570001
           05  CD-MONTH        PIC 99.                                  00580001
           05  CD-DAY          PIC 99.                                  00590001
           05  CD-HOURS        PIC 99.                                  00600001
           05  CD-MINUTES      PIC 99.                                  00610001
           05  FILLER          PIC X(9).                                00620001
                                                                        00630001
       01  HEADING-LINE-1.                                              00640001
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             00650001
           05  HL1-MONTH       PIC 9(2).                                00660001
           05  FILLER          PIC X(1)    VALUE "/".                   00670001
           05  HL1-DAY         PIC 9(2).                                00680001
           05  FILLER          PIC X(1)    VALUE "/".                   00690001
           05  HL1-YEAR        PIC 9(4).                                00700001
           05  FILLER          PIC X(16)   VALUE SPACE.                 00710002
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".00720001
           05  FILLER          PIC X(5)    VALUE "EPORT".               00730002
           05  FILLER          PIC X(22)   VALUE SPACE.                 00740002
           05  FILLER          PIC X(8)    VALUE "PAGE: ".              00750002
           05  Hl1-PAGE-NUMBER PIC ZZZ9.                                00760001
           05  FILLER          PIC X(43)   VALUE SPACE.                 00770002
                                                                        00780002
       01  HEADING-LINE-2.                                              00790001
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             00800001
           05  HL2-HOURS       PIC 9(2).                                00810001
           05  FILLER          PIC X(1)    VALUE ":".                   00820001
           05  HL2-MINUTES     PIC 9(2).                                00830001
           05  FILLER          PIC X(69)   VALUE SPACE.                 00840002
           05  FILLER          PIC X(10)   VALUE "RPT2000".             00850001
           05  FILLER          PIC X(38)   VALUE SPACE.                 00860002
                                                                        00870001
       01  HEADING-LINE-3.                                              00880001
           05  FILLER          PIC X(130)  VALUE SPACE.                 00890001
                                                                        00900001
       01  HEADING-LINE-4.                                              00910001
           05  FILLER      PIC X(13)   VALUE "BRANCH SALES ".           00920002
           05  FILLER      PIC X(4)    VALUE "CUST".                    00930002
           05  FILLER      PIC X(28)   VALUE SPACE.                     00931002
           05  FILLER      PIC X(5)    VALUE "SALES".                   00940002
           05  FILLER      PIC X(9)    VALUE SPACE.                     00941002
           05  FILLER      PIC X(5)    VALUE "SALES".                   00950002
           05  FILLER      PIC X(9)    VALUE SPACE.                     00951002
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    00960001
           05  FILLER      PIC X(39)   VALUE SPACE.                     00970002
                                                                        00980001
       01  HEADING-LINE-5.                                              00990001
           05  FILLER      PIC X(13)   VALUE " NUM    REP  ".           01000002
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01010001
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01020001
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01030001
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01040001
           05  FILLER      PIC X(39)   VALUE SPACE.                     01050002
                                                                        01060001
       01  HEADING-LINE-6.                                              01070001
                                                                        01080001
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01090002
           05  FILLER              PIC X(1)     VALUE SPACE.            01100002
           05  FILLER              PIC X(5)     VALUE ALL '-'.          01110002
           05  FILLER              PIC X(1)     VALUE SPACE.            01120002
           05  FILLER              PIC X(5)     VALUE ALL '-'.          01130002
           05  FILLER              PIC X(2)     VALUE SPACE.            01140002
           05  FILLER              PIC X(20)    VALUE ALL '-'.          01150002
           05  FILLER              PIC X(3)     VALUE SPACE.            01160002
           05  FILLER              PIC X(10)    VALUE ALL '-'.          01170002
           05  FILLER              PIC X(4)     VALUE SPACE.            01180002
           05  FILLER              PIC X(10)    VALUE ALL '-'.          01190002
           05  FILLER              PIC X(4)     VALUE SPACE.            01200002
           05  FILLER              PIC X(10)    VALUE ALL '-'.          01210002
           05  FILLER              PIC X(3)     VALUE SPACE.            01220002
           05  FILLER              PIC X(6)     VALUE ALL '-'.          01230002
                                                                        01240002
       01  CUSTOMER-LINE.                                               01250001
                                                                        01260002
           05  FILLER              PIC X(2)     VALUE SPACE.            01270002
           05  CL-BRANCH-NUMBER    PIC 9(2).                            01280002
           05  FILLER              PIC X(4)     VALUE SPACE.            01290002
           05  CL-SALESREP-NUMBER  PIC 9(2).                            01300002
           05  FILLER              PIC X(3)     VALUE SPACE.            01310002
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            01320001
           05  FILLER              PIC X(3)     VALUE SPACE.            01330002
           05  CL-CUSTOMER-NAME    PIC X(20).                           01340001
           05  FILLER              PIC X(3)     VALUE SPACE.            01350001
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      01360001
           05  FILLER              PIC X(4)     VALUE SPACE.            01370001
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      01380001
           05  FILLER              PIC X(4)     VALUE SPACE.            01390001
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      01400001
           05  FILLER              PIC X(3)     VALUE SPACE.            01410001
           05  CL-CHANGE-PERCENT   PIC ZZ9.9-.                          01420001
           05  FILLER              PIC X(43)    VALUE SPACE.            01430002
                                                                        01440001
       01  GRAND-TOTAL-HEADER.                                          01450002
           05  FILLER              PIC X(41)    VALUE SPACE.            01460002
           05  FILLER              PIC X(12)    VALUE ALL '='.          01470002
           05  FILLER              PIC X(3)     VALUE SPACE.            01480002
           05  FILLER              PIC X(12)    VALUE ALL '='.          01490002
           05  FILLER              PIC X(1)     VALUE SPACE.            01500002
           05  FILLER              PIC X(12)    VALUE ALL '='.          01510002
           05  FILLER              PIC X(3)     VALUE SPACE.            01520002
           05  FILLER              PIC X(6)     VALUE ALL '='.          01530002
                                                                        01540002
       01  GRAND-TOTAL-LINE.                                            01550001
           05  FILLER              PIC X(41)    VALUE SPACE.            01560002
           05  GTL-SALES-THIS-YTD  PIC Z,ZZZ,ZZ9.99-.                   01570001
           05  FILLER              PIC X(1)     VALUE SPACE.            01580001
           05  GTL-SALES-LAST-YTD  PIC Z,ZZZ,ZZ9.99-.                   01590001
           05  FILLER              PIC X(1)     VALUE SPACE.            01600001
           05  GTL-CHANGE-AMOUNT   PIC Z,ZZZ,ZZ9.99-.                   01610001
           05  FILLER              PIC X(3)     VALUE SPACE.            01620001
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          01630001
           05  FILLER              PIC X(55)    VALUE SPACE.            01640001
                                                                        01650001
       PROCEDURE DIVISION.                                              01660001
                                                                        01670001
       000-PREPARE-SALES-REPORT.                                        01680001
                                                                        01690001
           OPEN INPUT  CUSTMAST                                         01700001
                OUTPUT SALESRPT.                                        01710001
           PERFORM 100-FORMAT-REPORT-HEADING.                           01720001
           PERFORM 200-PREPARE-SALES-LINES                              01730001
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         01740001
           PERFORM 300-PRINT-GRAND-TOTALS.                              01750001
           CLOSE CUSTMAST                                               01760001
                 SALESRPT.                                              01770001
           STOP RUN.                                                    01780001
                                                                        01790001
       100-FORMAT-REPORT-HEADING.                                       01800001
                                                                        01810001
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         01820001
           MOVE CD-MONTH   TO HL1-MONTH.                                01830001
           MOVE CD-DAY     TO HL1-DAY.                                  01840001
           MOVE CD-YEAR    TO HL1-YEAR.                                 01850001
           MOVE CD-HOURS   TO HL2-HOURS.                                01860001
           MOVE CD-MINUTES TO HL2-MINUTES.                              01870001
                                                                        01880001
       200-PREPARE-SALES-LINES.                                         01890001
                                                                        01900001
           PERFORM 210-READ-CUSTOMER-RECORD.                            01910001
           IF CUSTMAST-EOF-SWITCH = "N"                                 01920001
             IF CM-SALES-THIS-YTD >= 10000                              01930001
               PERFORM 220-PRINT-CUSTOMER-LINE.                         01940001
                                                                        01950001
       210-READ-CUSTOMER-RECORD.                                        01960001
                                                                        01970001
           READ CUSTMAST                                                01980001
               AT END                                                   01990001
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     02000001
                                                                        02010001
       220-PRINT-CUSTOMER-LINE.                                         02020001
                                                                        02030001
           IF LINE-COUNT >= LINES-ON-PAGE                               02040001
               PERFORM 230-PRINT-HEADING-LINES.                         02050001
           MOVE CM-BRANCH-NUMBER    TO CL-BRANCH-NUMBER.                02060002
           MOVE CM-SALESREP-NUMBER  TO CL-SALESREP-NUMBER.              02070002
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              02080001
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                02090001
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               02100001
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               02110001
           COMPUTE CHANGE-AMOUNT =                                      02120001
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   02130001
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      02140001
           IF CM-SALES-LAST-YTD = ZERO                                  02150001
               MOVE 999.9 TO CL-CHANGE-PERCENT                          02160001
           ELSE                                                         02170001
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      02180001
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              02190001
                   ON SIZE ERROR                                        02200001
                       MOVE 999.9 TO CL-CHANGE-PERCENT.                 02210001
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            02220001
           WRITE PRINT-AREA.                                            02230001
           ADD 1 TO LINE-COUNT.                                         02240001
           ADD CM-SALES-THIS-YTD TO GRAND-TOTAL-THIS-YTD.               02250001
           ADD CM-SALES-LAST-YTD TO GRAND-TOTAL-LAST-YTD.               02260001
           MOVE 1 TO SPACE-CONTROL.                                     02270001
                                                                        02280001
       230-PRINT-HEADING-LINES.                                         02290001
                                                                        02300001
           ADD 1 TO PAGE-COUNT.                                         02310001
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      02320001
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           02330001
           WRITE PRINT-AREA.                                            02340001
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           02350001
           WRITE PRINT-AREA.                                            02360001
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           02370001
           WRITE PRINT-AREA.                                            02380001
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           02390001
           WRITE PRINT-AREA.                                            02400001
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           02410001
           WRITE PRINT-AREA.                                            02420002
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           02430002
           WRITE PRINT-AREA.                                            02440002
           MOVE ZERO TO LINE-COUNT.                                     02450001
           MOVE 2 TO SPACE-CONTROL.                                     02460001
                                                                        02470001
       300-PRINT-GRAND-TOTALS.                                          02480001
                                                                        02490001
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             02500001
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             02510001
           COMPUTE CHANGE-AMOUNT =                                      02520001
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             02530001
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     02540001
           IF GRAND-TOTAL-LAST-YTD = ZERO                               02550001
               MOVE 999.9 TO GTL-CHANGE-PERCENT                         02560001
           ELSE                                                         02570001
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     02580001
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           02590001
                   ON SIZE ERROR                                        02600001
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.                02610001
           MOVE GRAND-TOTAL-HEADER   TO PRINT-AREA.                     02620002
           WRITE PRINT-AREA.                                            02630002
           MOVE GRAND-TOTAL-LINE     TO PRINT-AREA.                     02640001
           WRITE PRINT-AREA.                                            02650001
