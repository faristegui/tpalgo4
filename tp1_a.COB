       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1-A.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROFESORES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PROF-ESTADO.

           SELECT TIPOS_CLASE ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS TIP-ESTADO.

           SELECT NOVTIMES1 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV1-ESTADO.

           SELECT NOVTIMES2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV2-ESTADO.

           SELECT NOVTIMES3 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV3-ESTADO.

           SELECT NOVTIMES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV-ESTADO.

       DATA DIVISION.

       FILE SECTION.
       FD  PROFESORES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Profesores.txt".
       01  REG-PROF.
           03 REG-PROF-NUMERO  PIC X(5).
           03 REG-PROF-DNI     PIC 9(8).
           03 REG-PROF-NOMBRE  PIC X(25).
           03 REG-PROF-DIRE    PIC X(20).
           03 REG-PROF-TEL     PIC X(20).

       FD  TIPOS_CLASE LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/TiposClase.txt".
       01  REG-TIP.
           03 REG-TIP-TIP_CLASE    PIC X(04).
           03 REG-TIP-DESC         PIC X(20).
           03 REG-TIP-TARIFA       PIC 9(5)V99.

       FD  NOVTIMES1 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes1.txt".
       01  REG-NOV1.
           03 REG-NOV1-NUMERO      PIC X(5).
           03 REG-NOV1-FECHA       PIC 9(8).
           03 REG-NOV1-SUCURSAL    PIC X(3).
           03 REG-NOV1-TIP_CLASE   PIC X(4).
           03 REG-NOV1-HORAS       PIC 9(2)V99.

       FD  NOVTIMES2 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes2.txt".
       01  REG-NOV2.
           03 REG-NOV2-NUMERO      PIC X(5).
           03 REG-NOV2-FECHA       PIC 9(8).
           03 REG-NOV2-SUCURSAL    PIC X(3).
           03 REG-NOV2-TIP_CLASE   PIC X(4).
           03 REG-NOV2-HORAS       PIC 9(2)V99.

       FD  NOVTIMES3 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes3.txt".
       01  REG-NOV3.
           03 REG-NOV3-NUMERO      PIC X(5).
           03 REG-NOV3-FECHA       PIC 9(8).
           03 REG-NOV3-SUCURSAL    PIC X(3).
           03 REG-NOV3-TIP_CLASE   PIC X(4).
           03 REG-NOV3-HORAS       PIC 9(2)V99.

       FD  NOVTIMES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes.txt".
       01  REG-NOV.
           03 REG-NOV-NUMERO       PIC X(5).
           03 REG-NOV-FECHA        PIC 9(8).
           03 REG-NOV-SUCURSAL     PIC X(3).
           03 REG-NOV-TIP_CLASE    PIC X(4).
           03 REG-NOV-HORAS        PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       77  PROF-ESTADO PIC XX.
       77  PROF-EOF    PIC XX  VALUE   "NO".

       77  TIP-ESTADO  PIC XX.
       77  TIP-EOF     PIC XX  VALUE   "NO".

       77  NOV1-ESTADO PIC XX.
       77  NOV1-EOF    PIC XX  VALUE   "NO".

       77  NOV2-ESTADO PIC XX.
       77  NOV2-EOF    PIC XX  VALUE   "NO".

       77  NOV3-ESTADO PIC XX.
       77  NOV3-EOF    PIC XX  VALUE   "NO".

       77  NOV-ESTADO  PIC XX.
       77  NOV-EOF     PIC XX  VALUE   "NO".

       77  WS-TIP-INDICE   PIC 9(4).
       01  TAB-TIP_CLASE.
           03  TAB-TIP                 OCCURS 50 TIMES.
               05 TAB-TIP-TIP_CLASE    PIC X(4).
               05 TAB-TIP-DESC         PIC X(20).
               05 TAB-TIP-TARIFA       PIC 9(5)V99.

       77  WS-TOT-GRAL-HORAS   PIC 9(4)V99.
       77  WS-TOT-PROF-HORAS   PIC 9(4)V99.
       77  WS-TOT-FECH-HORAS   PIC 9(4)V99.

       01  WS-PROF-ACT.
           03 WS-PROF-ACT-NUMERO   PIC X(5).
           03 WS-PROF-ACT-NOMBRE   PIC X(25).

       01  WS-TIP-ACT.
           03 WS-TIP-ACT-TIP_CLASE PIC X(04).
           03 WS-TIP-ACT-DESC      PIC X(20).
           03 WS-TIP-ACT-TARIFA    PIC 9(5)V99.

       01  WS-NOV-MEN.
           03 WS-NOV-MEN-NUMERO    PIC X(5).
           03 WS-NOV-MEN-FECHA     PIC 9(8).
           03 WS-NOV-MEN-SUCURSAL  PIC X(3).

       01  WS-NOV-ACT.
           03 WS-NOV-ACT-NUMERO    PIC X(5).
           03 WS-NOV-ACT-FECHA     PIC 9(8).
           03 WS-NOV-ACT-SUCURSAL  PIC X(3).
           03 WS-NOV-ACT-TIP_CLASE PIC X(4).
           03 WS-NOV-ACT-HORAS     PIC 9(2)V99.
           03 WS-NOV-ACT-IMPORTE   PIC 9(7)V99.

       01  PTR-NOV.
           03 PTR-NOV-FECHA        PIC 99/99/9999.
           03 FILLER               PIC X(4)        VALUE SPACES.
           03 PTR-NOV-SUCURSAL     PIC X(3).
           03 FILLER               PIC X(4)        VALUE SPACES.
           03 PTR-NOV-TIP_CLASE    PIC X(20).
           03 FILLER               PIC X(4)        VALUE SPACES.
           03 PTR-NOV-TARIFA       PIC ZZZZ9,99.
           03 FILLER               PIC X(4)        VALUE SPACES.
           03 PTR-NOV-HORAS        PIC Z9,99.
           03 FILLER               PIC X(4)        VALUE SPACES.
           03 PTR-NOV-IMPORTE      PIC ZZZZZZ9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIO.
           PERFORM LEO-PROFESORES.
           PERFORM LEO-NOVTIMES1.
           PERFORM LEO-NOVTIMES2.
           PERFORM LEO-NOVTIMES3.
           PERFORM CARGO-TIPOS_CLASE.
           MOVE ZERO TO WS-TOT-GRAL-HORAS.
           PERFORM PROC-GENERAL UNTIL
                           PROF-EOF="SI"
                           AND NOV1-EOF="SI"
                           AND NOV2-EOF="SI"
                           AND NOV3-EOF="SI".
      *     DISPLAY WS-TOT-GRAL-HORAS.
           PERFORM FIN.
           STOP RUN.

       INICIO.
           OPEN INPUT NOVTIMES1.
           IF NOV1-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN NovTimes1: " NOV1-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN INPUT NOVTIMES2.
           IF NOV2-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN NovTimes2: " NOV2-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN INPUT NOVTIMES3.
           IF NOV3-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN NovTimes3: " NOV3-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN INPUT PROFESORES.
           IF PROF-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN Profesores: " PROF-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN INPUT TIPOS_CLASE.
           IF TIP-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN TiposClase: " TIP-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN OUTPUT NOVTIMES.
           IF NOV-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN NovTimes: " NOV-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-PROFESORES.
           READ PROFESORES AT END MOVE "SI" TO PROF-EOF.
           IF PROF-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ Profesores: " PROF-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-TIPOS_CLASE.
           READ TIPOS_CLASE AT END MOVE "SI" TO TIP-EOF.
           IF TIP-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ TiposClase: " TIP-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-NOVTIMES1.
           READ NOVTIMES1 AT END MOVE "SI" TO NOV1-EOF.
           IF NOV1-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ NovTimes1: " NOV1-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-NOVTIMES2.
           READ NOVTIMES2 AT END MOVE "SI" TO NOV2-EOF.
           IF NOV2-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ NovTimes2: " NOV2-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-NOVTIMES3.
           READ NOVTIMES3 AT END MOVE "SI" TO NOV3-EOF.
           IF NOV3-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ NovTimes3: " NOV3-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       CARGO-TIPOS_CLASE.
           PERFORM LEO-TIPOS_CLASE.
           MOVE 1 TO WS-TIP-INDICE.
           PERFORM
           UNTIL TIP-EOF="SI"
               MOVE REG-TIP TO TAB-TIP(WS-TIP-INDICE)
               ADD 1 TO WS-TIP-INDICE
               PERFORM LEO-TIPOS_CLASE
           END-PERFORM.

       PROC-GENERAL.
           PERFORM DET-PROFESOR-MEN.
           IF WS-NOV-MEN-NUMERO = REG-PROF-NUMERO THEN
               PERFORM CARGO-PROFESOR
           END-IF.
           MOVE ZERO TO WS-TOT-PROF-HORAS.
      *     DISPLAY WS-PROF-ACT-NOMBRE.
           PERFORM PROC-PROFESOR UNTIL
                           (PROF-EOF="SI"
                           AND NOV1-EOF="SI"
                           AND NOV2-EOF="SI"
                           AND NOV3-EOF="SI")
                           OR
                           (REG-NOV1-NUMERO NOT = WS-NOV-MEN-NUMERO
                           AND REG-NOV2-NUMERO NOT = WS-NOV-MEN-NUMERO
                           AND REG-NOV3-NUMERO NOT = WS-NOV-MEN-NUMERO).
           ADD WS-TOT-PROF-HORAS TO WS-TOT-GRAL-HORAS.
      *     DISPLAY WS-TOT-PROF-HORAS.

       DET-PROFESOR-MEN.
           MOVE REG-PROF-NUMERO TO WS-NOV-MEN-NUMERO.
           IF REG-NOV1-NUMERO < WS-NOV-MEN-NUMERO THEN
               MOVE REG-NOV1-NUMERO TO WS-NOV-MEN-NUMERO
           END-IF.
           IF REG-NOV2-NUMERO < WS-NOV-MEN-NUMERO THEN
               MOVE REG-NOV2-NUMERO TO WS-NOV-MEN-NUMERO
           END-IF.
           IF REG-NOV3-NUMERO < WS-NOV-MEN-NUMERO THEN
               MOVE REG-NOV3-NUMERO TO WS-NOV-MEN-NUMERO
           END-IF.

       CARGO-PROFESOR.
           MOVE REG-PROF-NUMERO TO WS-PROF-ACT-NUMERO.
           MOVE REG-PROF-NOMBRE TO WS-PROF-ACT-NOMBRE.
           PERFORM LEO-PROFESORES.

       PROC-PROFESOR.
           PERFORM DET-FECHA-MEN.
           MOVE ZERO TO WS-TOT-FECH-HORAS.
           PERFORM PROC-FECHA UNTIL
                           (PROF-EOF="SI"
                           AND NOV1-EOF="SI"
                           AND NOV2-EOF="SI"
                           AND NOV3-EOF="SI")
                           OR
                           (REG-NOV1-NUMERO NOT = WS-NOV-MEN-NUMERO
                           AND REG-NOV2-NUMERO NOT = WS-NOV-MEN-NUMERO
                           AND REG-NOV3-NUMERO NOT = WS-NOV-MEN-NUMERO)
                           OR
                           (REG-NOV1-FECHA NOT = WS-NOV-MEN-FECHA
                           AND REG-NOV2-FECHA NOT = WS-NOV-MEN-FECHA
                           AND REG-NOV3-FECHA NOT = WS-NOV-MEN-FECHA).
           ADD WS-TOT-FECH-HORAS TO WS-TOT-PROF-HORAS.

       DET-FECHA-MEN.
           MOVE REG-NOV1-FECHA TO WS-NOV-MEN-FECHA.
           IF REG-NOV2-FECHA < WS-NOV-MEN-FECHA THEN
               MOVE REG-NOV2-FECHA TO WS-NOV-MEN-FECHA
           END-IF.
           IF REG-NOV3-FECHA < WS-NOV-MEN-FECHA THEN
               MOVE REG-NOV3-FECHA TO WS-NOV-MEN-FECHA
           END-IF.

       PROC-FECHA.
           PERFORM DET-SUCURSAL-MEN.
           PERFORM PROC-NOVTIMES1 UNTIL
                       NOV1-EOF="SI"
                       OR REG-NOV1-NUMERO NOT = WS-NOV-MEN-NUMERO
                       OR REG-NOV1-FECHA NOT = WS-NOV-MEN-FECHA
                       OR REG-NOV1-SUCURSAL NOT = WS-NOV-MEN-SUCURSAL.
           PERFORM PROC-NOVTIMES2 UNTIL
                       NOV2-EOF="SI"
                       OR REG-NOV2-NUMERO NOT = WS-NOV-MEN-NUMERO
                       OR REG-NOV2-FECHA NOT = WS-NOV-MEN-FECHA
                       OR REG-NOV2-SUCURSAL NOT = WS-NOV-MEN-SUCURSAL.
           PERFORM PROC-NOVTIMES3 UNTIL
                       NOV3-EOF="SI"
                       OR REG-NOV3-NUMERO NOT = WS-NOV-MEN-NUMERO
                       OR REG-NOV3-FECHA NOT = WS-NOV-MEN-FECHA
                       OR REG-NOV3-SUCURSAL NOT = WS-NOV-MEN-SUCURSAL.

       DET-SUCURSAL-MEN.
           MOVE REG-NOV1-SUCURSAL TO WS-NOV-MEN-SUCURSAL.
           IF REG-NOV2-SUCURSAL < WS-NOV-MEN-SUCURSAL THEN
               MOVE REG-NOV2-SUCURSAL TO WS-NOV-MEN-SUCURSAL
           END-IF.
           IF REG-NOV3-SUCURSAL < WS-NOV-MEN-SUCURSAL THEN
               MOVE REG-NOV3-SUCURSAL TO WS-NOV-MEN-SUCURSAL
           END-IF.

       PROC-NOVTIMES1.
           MOVE REG-NOV1 TO WS-NOV-ACT.
           PERFORM PROC-NOVTIMES.
           PERFORM LEO-NOVTIMES1.

       PROC-NOVTIMES2.
           MOVE REG-NOV2 TO WS-NOV-ACT.
           PERFORM PROC-NOVTIMES.
           PERFORM LEO-NOVTIMES2.

       PROC-NOVTIMES3.
           MOVE REG-NOV3 TO WS-NOV-ACT.
           PERFORM PROC-NOVTIMES.
           PERFORM LEO-NOVTIMES3.

       PROC-NOVTIMES.
           ADD WS-NOV-ACT-HORAS TO WS-TOT-FECH-HORAS.
           MOVE WS-NOV-ACT TO REG-NOV.
           WRITE REG-NOV.
           PERFORM BUSCO-TIPO_CLASE.
           MOVE WS-TIP-ACT-TARIFA TO WS-NOV-ACT-IMPORTE.
           MULTIPLY WS-NOV-ACT-HORAS BY WS-NOV-ACT-IMPORTE.
           PERFORM IMPRIMO-NOV.

       BUSCO-TIPO_CLASE.
           MOVE 1 TO WS-TIP-INDICE.
           PERFORM
           UNTIL TAB-TIP-TIP_CLASE(WS-TIP-INDICE) = WS-NOV-ACT-TIP_CLASE
                 OR WS-TIP-INDICE = 51
               ADD 1 TO WS-TIP-INDICE
           END-PERFORM.
           IF WS-TIP-INDICE <= 50 THEN
               MOVE TAB-TIP-DESC(WS-TIP-INDICE) TO WS-TIP-ACT-DESC
               MOVE TAB-TIP-TARIFA(WS-TIP-INDICE) TO WS-TIP-ACT-TARIFA
           ELSE
               MOVE "Sin descripcion" TO WS-TIP-ACT-DESC
               MOVE 0 TO WS-TIP-ACT-TARIFA
           END-IF.

       IMPRIMO-NOV.
           MOVE WS-NOV-ACT-FECHA TO PTR-NOV-FECHA.
           MOVE WS-NOV-ACT-SUCURSAL TO PTR-NOV-SUCURSAL.
           MOVE WS-TIP-ACT-DESC TO PTR-NOV-TIP_CLASE.
           MOVE WS-TIP-ACT-TARIFA TO PTR-NOV-TARIFA.
           MOVE WS-NOV-ACT-HORAS TO PTR-NOV-HORAS.
           MOVE WS-NOV-ACT-IMPORTE TO PTR-NOV-IMPORTE.
           DISPLAY PTR-NOV.

       FIN.
           CLOSE NOVTIMES1.
           CLOSE NOVTIMES2.
           CLOSE NOVTIMES3.
           CLOSE PROFESORES.
           CLOSE TIPOS_CLASE.
           CLOSE NOVTIMES.

       END PROGRAM TP1-A.
