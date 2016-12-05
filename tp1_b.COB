       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1-B.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SUCURSALES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS SUC-ESTADO.

           SELECT NOVTIMES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV-ESTADO.

       DATA DIVISION.

       FILE SECTION.
       FD  SUCURSALES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Sucursales.txt".
       01  REG-SUC.
           03 REG-SUC-SUCURSAL PIC X(3).
           03 REG-SUC-RAZON    PIC 9(25).
           03 REG-SUC-DIRE     PIC X(20).
           03 REG-SUC-TEL      PIC X(20).
           03 REG-SUC-CUIT     PIC X(11).

       FD  NOVTIMES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes.txt".
       01  REG-NOV.
           03 REG-NOV-NUMERO      PIC X(5).
           03 REG-NOV-FECHA       PIC 9(8).
           03 FILLER REDEFINES REG-NOV-FECHA.
              05 REG-NOV-FECHA-AA PIC 9(4).
              05 REG-NOV-FECHA-MM PIC 9(2).
              05 REG-NOV-FECHA-DD PIC 9(2).
           03 REG-NOV-SUCURSAL    PIC X(3).
           03 REG-NOV-TIP_CLASE   PIC X(4).
           03 REG-NOV-HORAS       PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       77  SUC-ESTADO PIC XX.
           88 SUC-OK  VALUE 00.
           88 SUC-EOF VALUE 10.

       77  NOV-ESTADO PIC XX.
           88 NOV-OK  VALUE 00.
           88 NOV-EOF VALUE 10.

       01  WS-SUC-INDICE      PIC 9(2).
       01  WS-SUC-ANIO-INDICE PIC 9(4).
       01  WS-SUC-MES-INDICE  PIC 9(2).
       01  TAB-SUCURSALES.
           03 TAB-SUC OCCURS 3 TIMES INDEXED BY I.
              05 TAB-SUC-SUCURSAL            PIC X(3).
              05 TAB-SUC-RAZON               PIC X(25).
              05 TAB-SUC-ANIO OCCURS 5 TIMES.
                 07 TAB-SUC-ANIO-ANIO        PIC 9(4).
                 07 TAB-SUC-ANIO-TOT-HORAS   PIC 9(3)V99 VALUE ZERO.
                 07 TAB-SUC-MES OCCURS 12 TIMES.
                    09 TAB-SUC-MES-TOT-HORAS PIC 9(3)V99 VALUE ZERO.

       01  WS-MES-INDICE PIC 9(2).
       01  TAB-MESES.
           03 TAB-MES OCCURS 12 TIMES.
              05 TAB-MES-TOT-HORAS PIC 9(3) VALUE ZERO.

       01  WS-FECHA-ACT.
           03 WS-FECHA-ACT-AA PIC 9(4).
           03 WS-FECHA-ACT-MM PIC 9(2).
           03 WS-FECHA-ACT-DD PIC 9(2).

       77  WS-ANIO-DESDE       PIC 9(4).
       77  WS-TOT-GRAL-HORAS   PIC 9(4)V99.

       01  PTR-CAB.
           03 FILLER       PIC X(6)    VALUE "FECHA ".
           03 PTR-CAB-DD   PIC 99.
           03 FILLER       PIC X(1)    VALUE "/".
           03 PTR-CAB-MM   PIC 99.
           03 FILLER       PIC X(1)    VALUE "/".
           03 PTR-CAB-AA   PIC 9999.
           03 FILLER       PIC X(60)   VALUE SPACES.
           03 FILLER       PIC X(8)    VALUE "HOJA 001".

       01  PTR-TIT.
           03 FILLER PIC X(14) VALUE SPACES.
           03 FILLER PIC X(57) VALUE
           "Listado de estadístico de horas aplicadas por año y mes".
           03 FILLER PIC X(14) VALUE SPACES.

       01  PTR-SUC-CAB.
           03 FILLER PIC X(8)  VALUE "Sucursal".
           03 FILLER PIC X(23) VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Ene".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Feb".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Mar".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Abr".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "May".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Jun".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Jul".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Ago".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Sep".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Oct".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Nov".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(3)  VALUE "Dic".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(5)  VALUE "Total".

       01  PTR-SUC-DET.
           03 PTR-SUC-DET-RAZON PIC X(25).
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-AA    PIC 9999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-ENE   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-FEB   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-MAR   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-ABR   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-MAY   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-JUN   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-JUL   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-AGO   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-SEP   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-OCT   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-NOV   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-DIC   PIC 999.
           03 FILLER            PIC X(1)    VALUE SPACES.
           03 PTR-SUC-DET-TOT   PIC 9999.
           03 FILLER            PIC X(1)    VALUE SPACES.

       01  PTR-TOT-GRAL.
           03 FILLER           PIC X(7)   VALUE "Totales".
           03 FILLER           PIC X(24)  VALUE SPACES.
           03 PTR-TOT-GRAL-ENE PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-FEB PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-MAR PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-ABR PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-MAY PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-JUN PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-JUL PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-AGO PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-SEP PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-OCT PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-NOV PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-DIC PIC 999.
           03 FILLER           PIC X(1)   VALUE SPACES.
           03 PTR-TOT-GRAL-TOT PIC 9999.
           03 FILLER           PIC X(1)   VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIO.
           PERFORM CARGO-SUCURSALES.
           PERFORM LEO-NOVTIMES.
           MOVE ZERO TO WS-TOT-GRAL-HORAS.
           PERFORM PROC-TIMES UNTIL (NOV-EOF).
           PERFORM IMPRIMO-REPORTE.
           PERFORM FIN.
           STOP RUN.

       INICIO.
           ACCEPT WS-FECHA-ACT FROM DATE YYYYMMDD.
           MOVE WS-FECHA-ACT-AA TO WS-ANIO-DESDE.
           SUBTRACT 5 FROM WS-ANIO-DESDE.
           MOVE WS-FECHA-ACT-DD TO PTR-CAB-DD.
           MOVE WS-FECHA-ACT-MM TO PTR-CAB-MM.
           MOVE WS-FECHA-ACT-AA TO PTR-CAB-AA.

           OPEN INPUT SUCURSALES.
           IF (NOT SUC-OK) THEN
               DISPLAY "ERROR EN OPEN Sucursales: " SUC-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN INPUT NOVTIMES.
           IF (NOT NOV-OK) THEN
               DISPLAY "ERROR EN OPEN NovTimes: " NOV-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-SUCURSALES.
           READ SUCURSALES RECORD INTO REG-SUC.
           IF ((NOT SUC-OK) AND (NOT SUC-EOF)) THEN
               DISPLAY "ERROR EN READ Sucusales: " SUC-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-NOVTIMES.
           READ NOVTIMES RECORD INTO REG-NOV.
           IF ((NOT NOV-OK) AND (NOT NOV-EOF)) THEN
               DISPLAY "ERROR EN READ NovTimes: " NOV-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       CARGO-SUCURSALES.
           PERFORM LEO-SUCURSALES.
           MOVE 1 TO WS-SUC-INDICE.
           PERFORM UNTIL (SUC-EOF)
               MOVE REG-SUC-SUCURSAL TO TAB-SUC-SUCURSAL(WS-SUC-INDICE)
               MOVE REG-SUC-RAZON TO TAB-SUC-RAZON(WS-SUC-INDICE)
               PERFORM CARGO-ANIOS
               ADD 1 TO WS-SUC-INDICE
               PERFORM LEO-SUCURSALES
           END-PERFORM.

       CARGO-ANIOS.
           MOVE 1 TO WS-SUC-ANIO-INDICE.
           PERFORM UNTIL (WS-SUC-ANIO-INDICE > 5)
               MOVE WS-FECHA-ACT-AA
                    TO TAB-SUC-ANIO-ANIO(WS-SUC-INDICE,
                                         WS-SUC-ANIO-INDICE)
               SUBTRACT WS-SUC-ANIO-INDICE
                        FROM TAB-SUC-ANIO-ANIO(WS-SUC-INDICE,
                                               WS-SUC-ANIO-INDICE)
               ADD 1 TO TAB-SUC-ANIO-ANIO(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE)
               ADD 1 TO WS-SUC-ANIO-INDICE
           END-PERFORM.

       PROC-TIMES.
           IF (REG-NOV-FECHA-AA >= WS-ANIO-DESDE) THEN
               PERFORM BUSCO-SUCURSAL
               PERFORM BUSCO-ANIO
               ADD REG-NOV-HORAS
                   TO TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                            WS-SUC-ANIO-INDICE,
                                            REG-NOV-FECHA-MM)
               ADD REG-NOV-HORAS
                   TO TAB-SUC-ANIO-TOT-HORAS(WS-SUC-INDICE,
                                             WS-SUC-ANIO-INDICE)
               ADD REG-NOV-HORAS TO TAB-MES-TOT-HORAS(REG-NOV-FECHA-MM)
               ADD REG-NOV-HORAS TO WS-TOT-GRAL-HORAS
           END-IF.
           PERFORM LEO-NOVTIMES.

       BUSCO-SUCURSAL.
           SET I TO 1.
           SEARCH TAB-SUC
                  WHEN TAB-SUC-SUCURSAL(I) = REG-NOV-SUCURSAL
                       MOVE I TO WS-SUC-INDICE
           END-SEARCH.

       BUSCO-ANIO.
           MOVE WS-FECHA-ACT-AA TO WS-SUC-ANIO-INDICE.
           SUBTRACT REG-NOV-FECHA-AA FROM WS-SUC-ANIO-INDICE.
           ADD 1 TO WS-SUC-ANIO-INDICE.

       IMPRIMO-REPORTE.
           DISPLAY PTR-CAB.
           DISPLAY PTR-TIT.
           DISPLAY PTR-SUC-CAB.
           MOVE 1 TO WS-SUC-INDICE.
           PERFORM UNTIL WS-SUC-INDICE > 3
               PERFORM IMPRIMO-SUC-DET
               ADD 1 TO WS-SUC-INDICE
           END-PERFORM.
           PERFORM IMPRIMO-TOT-GRAL.

       IMPRIMO-SUC-DET.
           MOVE TAB-SUC-RAZON(WS-SUC-INDICE) TO PTR-SUC-DET-RAZON.
           MOVE 5 TO WS-SUC-ANIO-INDICE.
           PERFORM UNTIL WS-SUC-ANIO-INDICE = 0
               IF WS-SUC-ANIO-INDICE NOT = 5 THEN
                   MOVE SPACES TO PTR-SUC-DET-RAZON
               END-IF
               MOVE TAB-SUC-ANIO-ANIO(WS-SUC-INDICE,
                                      WS-SUC-ANIO-INDICE)
                    TO PTR-SUC-DET-AA
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          1)
                    TO PTR-SUC-DET-ENE
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          2)
                    TO PTR-SUC-DET-FEB
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          3)
                    TO PTR-SUC-DET-MAR
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          4)
                    TO PTR-SUC-DET-ABR
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          5)
                    TO PTR-SUC-DET-MAY
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          6)
                    TO PTR-SUC-DET-JUN
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          7)
                    TO PTR-SUC-DET-JUL
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          8)
                    TO PTR-SUC-DET-AGO
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          9)
                    TO PTR-SUC-DET-SEP
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          10)
                    TO PTR-SUC-DET-OCT
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          11)
                    TO PTR-SUC-DET-NOV
               MOVE TAB-SUC-MES-TOT-HORAS(WS-SUC-INDICE,
                                          WS-SUC-ANIO-INDICE,
                                          12)
                    TO PTR-SUC-DET-DIC
               MOVE TAB-SUC-ANIO-TOT-HORAS(WS-SUC-INDICE,
                                           WS-SUC-ANIO-INDICE)
                    TO PTR-SUC-DET-TOT
               SUBTRACT 1 FROM WS-SUC-ANIO-INDICE
               DISPLAY PTR-SUC-DET
           END-PERFORM.

       IMPRIMO-TOT-GRAL.
           MOVE TAB-MES-TOT-HORAS(1) TO PTR-TOT-GRAL-ENE
           MOVE TAB-MES-TOT-HORAS(2) TO PTR-TOT-GRAL-FEB
           MOVE TAB-MES-TOT-HORAS(3) TO PTR-TOT-GRAL-MAR
           MOVE TAB-MES-TOT-HORAS(4) TO PTR-TOT-GRAL-ABR
           MOVE TAB-MES-TOT-HORAS(5) TO PTR-TOT-GRAL-MAY
           MOVE TAB-MES-TOT-HORAS(6) TO PTR-TOT-GRAL-JUN
           MOVE TAB-MES-TOT-HORAS(7) TO PTR-TOT-GRAL-JUL
           MOVE TAB-MES-TOT-HORAS(8) TO PTR-TOT-GRAL-AGO
           MOVE TAB-MES-TOT-HORAS(9) TO PTR-TOT-GRAL-SEP
           MOVE TAB-MES-TOT-HORAS(10) TO PTR-TOT-GRAL-OCT
           MOVE TAB-MES-TOT-HORAS(11) TO PTR-TOT-GRAL-NOV
           MOVE TAB-MES-TOT-HORAS(12) TO PTR-TOT-GRAL-DIC
           MOVE WS-TOT-GRAL-HORAS TO PTR-TOT-GRAL-TOT
           DISPLAY PTR-TOT-GRAL.

       FIN.
           CLOSE SUCURSALES.
           CLOSE NOVTIMES.

       END PROGRAM TP1-B.
