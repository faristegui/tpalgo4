       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENERADOR-TP1.

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

       DATA DIVISION.

       FILE SECTION.
       FD  PROFESORES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Profesores.txt".
       01  REG-PROF.
           03 REG-PROF-NUMERO PIC X(5).
           03 REG-PROF-DNI    PIC 9(8).
           03 REG-PROF-NOMBRE PIC X(25).
           03 REG-PROF-DIRE   PIC X(20).
           03 REG-PROF-TEL    PIC X(20).

       FD  TIPOS_CLASE LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/TiposClase.txt".
       01  REG-TIP.
           03 REG-TIP-TIP_CLASE PIC X(04).
           03 REG-TIP-DESC      PIC X(20).
           03 REG-TIP-TARIFA    PIC 9(5)V99.

       FD  NOVTIMES1 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes1.txt".
       01  REG-NOV1.
           03 REG-NOV1-NUMERO      PIC X(5).
           03 REG-NOV1-FECHA       PIC 9(8).
           03 FILLER REDEFINES REG-NOV1-FECHA.
              05 REG-NOV1-FECHA-AA PIC 9(4).
              05 REG-NOV1-FECHA-MM PIC 9(2).
              05 REG-NOV1-FECHA-DD PIC 9(2).
           03 REG-NOV1-SUCURSAL    PIC X(3).
           03 REG-NOV1-TIP_CLASE   PIC X(4).
           03 REG-NOV1-HORAS       PIC 9(2)V99.

       FD  NOVTIMES2 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes2.txt".
       01  REG-NOV2.
           03 REG-NOV2-NUMERO      PIC X(5).
           03 REG-NOV2-FECHA       PIC 9(8).
           03 FILLER REDEFINES REG-NOV2-FECHA.
              05 REG-NOV2-FECHA-AA PIC 9(4).
              05 REG-NOV2-FECHA-MM PIC 9(2).
              05 REG-NOV2-FECHA-DD PIC 9(2).
           03 REG-NOV2-SUCURSAL    PIC X(3).
           03 REG-NOV2-TIP_CLASE   PIC X(4).
           03 REG-NOV2-HORAS       PIC 9(2)V99.

       FD  NOVTIMES3 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes3.txt".
       01  REG-NOV3.
           03 REG-NOV3-NUMERO      PIC X(5).
           03 REG-NOV3-FECHA       PIC 9(8).
           03 FILLER REDEFINES REG-NOV3-FECHA.
              05 REG-NOV3-FECHA-AA PIC 9(4).
              05 REG-NOV3-FECHA-MM PIC 9(2).
              05 REG-NOV3-FECHA-DD PIC 9(2).
           03 REG-NOV3-SUCURSAL    PIC X(3).
           03 REG-NOV3-TIP_CLASE   PIC X(4).
           03 REG-NOV3-HORAS       PIC 9(2)V99.

       WORKING-STORAGE SECTION.
       77  PROF-ESTADO PIC XX.
           88 PROF-OK  VALUE 00.
           88 PROF-EOF VALUE 10.

       77  TIP-ESTADO PIC XX.
           88 TIP-OK  VALUE 00.
           88 TIP-EOF VALUE 10.

       77  NOV1-ESTADO PIC XX.
           88 NOV1-OK  VALUE 00.
           88 NOV1-EOF VALUE 10.

       77  NOV2-ESTADO PIC XX.
           88 NOV2-OK  VALUE 00.
           88 NOV2-EOF VALUE 10.

       77  NOV3-ESTADO PIC XX.
           88 NOV3-OK  VALUE  00.
           88 NOV3-EOF VALUE 10.

       77  NOV-ESTADO PIC XX.
           88 NOV-OK VALUE 00.

       77  WS-TIP-INDICE PIC 9(4).
       01  TAB-TIP_CLASE.
           03 TAB-TIP-CANTIDAD     PIC 9(2).
           03 TAB-TIP OCCURS 50 TIMES.
              05 TAB-TIP-TIP_CLASE PIC X(4).
              05 TAB-TIP-DESC      PIC X(20).
              05 TAB-TIP-TARIFA    PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIO.
           PERFORM LEO-PROFESORES.
           PERFORM CARGO-TIPOS_CLASE.
           PERFORM PROC-PROFESOR UNTIL (PROF-EOF).
           PERFORM FIN.
           STOP RUN.

       INICIO.
           MOVE "001" TO REG-NOV1-SUCURSAL.
           MOVE "002" TO REG-NOV2-SUCURSAL.
           MOVE "003" TO REG-NOV3-SUCURSAL.

           MOVE 2,5 TO REG-NOV1-HORAS.
           MOVE 2,5 TO REG-NOV2-HORAS.
           MOVE 2,5 TO REG-NOV3-HORAS.

           OPEN INPUT PROFESORES.
           IF (NOT PROF-OK) THEN
               DISPLAY "ERROR EN OPEN Profesores: " PROF-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN INPUT TIPOS_CLASE.
           IF (NOT TIP-OK) THEN
               DISPLAY "ERROR EN OPEN TiposClase: " TIP-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN OUTPUT NOVTIMES1.
           IF (NOT NOV1-OK) THEN
               DISPLAY "ERROR EN OPEN NovTimes1: " NOV1-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN OUTPUT NOVTIMES2.
           IF (NOT NOV2-OK) THEN
               DISPLAY "ERROR EN OPEN NovTimes2: " NOV2-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

           OPEN OUTPUT NOVTIMES3.
           IF (NOT NOV3-OK) THEN
               DISPLAY "ERROR EN OPEN NovTimes3: " NOV3-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-PROFESORES.
           READ PROFESORES RECORD INTO REG-PROF.
           IF ((NOT PROF-OK) AND (NOT PROF-EOF)) THEN
               DISPLAY "ERROR EN READ Profesores: " PROF-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-TIPOS_CLASE.
           READ TIPOS_CLASE RECORD INTO REG-TIP.
           IF ((NOT TIP-OK) AND (NOT TIP-EOF)) THEN
               DISPLAY "ERROR EN READ TiposClase: " TIP-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       CARGO-TIPOS_CLASE.
           PERFORM LEO-TIPOS_CLASE.
           MOVE 1 TO WS-TIP-INDICE.
           MOVE 0 TO TAB-TIP-CANTIDAD.
           PERFORM
           UNTIL (TIP-EOF)
               MOVE REG-TIP TO TAB-TIP(WS-TIP-INDICE)
               ADD 1 TO WS-TIP-INDICE
               ADD 1 TO TAB-TIP-CANTIDAD
               PERFORM LEO-TIPOS_CLASE
           END-PERFORM.
           MOVE 1 TO WS-TIP-INDICE.

       PROC-PROFESOR.
           MOVE REG-PROF-NUMERO TO REG-NOV1-NUMERO.
           MOVE REG-PROF-NUMERO TO REG-NOV2-NUMERO.
           MOVE REG-PROF-NUMERO TO REG-NOV3-NUMERO.
           PERFORM PROC-ANIOS.
           PERFORM LEO-PROFESORES.

       PROC-ANIOS.
           MOVE 2012 TO REG-NOV1-FECHA-AA.
           MOVE 2012 TO REG-NOV2-FECHA-AA.
           MOVE 2012 TO REG-NOV3-FECHA-AA.
           PERFORM UNTIL (REG-NOV1-FECHA-AA > 2016
                          AND REG-NOV2-FECHA-AA > 2016
                          AND REG-NOV3-FECHA-AA > 2016)
               PERFORM PROC-MESES
               ADD 1 TO REG-NOV1-FECHA-AA
               ADD 1 TO REG-NOV2-FECHA-AA
               ADD 1 TO REG-NOV3-FECHA-AA
           END-PERFORM.

       PROC-MESES.
           MOVE 01 TO REG-NOV1-FECHA-MM.
           MOVE 01 TO REG-NOV2-FECHA-MM.
           MOVE 01 TO REG-NOV3-FECHA-MM.
           PERFORM UNTIL (REG-NOV1-FECHA-MM > 12
                          AND REG-NOV2-FECHA-MM > 12
                          AND REG-NOV3-FECHA-MM > 12)
               PERFORM PROC-DIAS
               ADD 1 TO REG-NOV1-FECHA-MM
               ADD 1 TO REG-NOV2-FECHA-MM
               ADD 1 TO REG-NOV3-FECHA-MM
           END-PERFORM.

       PROC-DIAS.
           MOVE 1 TO REG-NOV1-FECHA-DD.
           MOVE 1 TO REG-NOV2-FECHA-DD.
           MOVE 1 TO REG-NOV3-FECHA-DD.
           PERFORM UNTIL (REG-NOV1-FECHA-DD > 3
                          AND REG-NOV2-FECHA-DD > 3
                          AND REG-NOV3-FECHA-DD > 3)
               PERFORM PROC-TIP_CLASES
               ADD 1 TO REG-NOV1-FECHA-DD
               ADD 1 TO REG-NOV2-FECHA-DD
               ADD 1 TO REG-NOV3-FECHA-DD
           END-PERFORM.

       PROC-TIP_CLASES.
           PERFORM PROC-TIP-CLASE 2 TIMES.

       PROC-TIP-CLASE.
           IF (WS-TIP-INDICE > TAB-TIP-CANTIDAD) THEN
               MOVE 1 TO WS-TIP-INDICE
           END-IF.
           MOVE TAB-TIP-TIP_CLASE(WS-TIP-INDICE) TO REG-NOV1-TIP_CLASE.
           MOVE TAB-TIP-TIP_CLASE(WS-TIP-INDICE) TO REG-NOV2-TIP_CLASE.
           MOVE TAB-TIP-TIP_CLASE(WS-TIP-INDICE) TO REG-NOV3-TIP_CLASE.
           WRITE REG-NOV1.
           WRITE REG-NOV2.
           WRITE REG-NOV3.
           ADD 1 TO WS-TIP-INDICE.

       FIN.
           CLOSE PROFESORES.
           CLOSE TIPOS_CLASE.
           CLOSE NOVTIMES1.
           CLOSE NOVTIMES2.
           CLOSE NOVTIMES3.

       END PROGRAM GENERADOR-TP1.
