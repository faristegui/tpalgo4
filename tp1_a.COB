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
           03 REG-NOV1-NUMERO                  PIC X(5).
           03 REG-NOV1-FECHA                   PIC 9(8).
           03 FILLER REDEFINES REG-NOV1-FECHA.
               05 REG-NOV1-FECHA-AA            PIC 9(4).
               05 REG-NOV1-FECHA-MM            PIC 9(2).
               05 REG-NOV1-FECHA-DD            PIC 9(2).
           03 REG-NOV1-SUCURSAL                PIC X(3).
           03 REG-NOV1-TIP_CLASE               PIC X(4).
           03 REG-NOV1-HORAS                   PIC 9(2)V99.


       FD  NOVTIMES2 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes2.txt".
       01  REG-NOV2.
           03 REG-NOV2-NUMERO                  PIC X(5).
           03 REG-NOV2-FECHA                   PIC 9(8).
           03 FILLER REDEFINES REG-NOV2-FECHA.
               05 REG-NOV2-FECHA-AA            PIC 9(4).
               05 REG-NOV2-FECHA-MM            PIC 9(2).
               05 REG-NOV2-FECHA-DD            PIC 9(2).
           03 REG-NOV2-SUCURSAL                PIC X(3).
           03 REG-NOV2-TIP_CLASE               PIC X(4).
           03 REG-NOV2-HORAS                   PIC 9(2)V99.

       FD  NOVTIMES3 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes3.txt".
       01  REG-NOV3.
           03 REG-NOV3-NUMERO                  PIC X(5).
           03 REG-NOV3-FECHA                   PIC 9(8).
           03 FILLER REDEFINES REG-NOV3-FECHA.
               05 REG-NOV3-FECHA-AA            PIC 9(4).
               05 REG-NOV3-FECHA-MM            PIC 9(2).
               05 REG-NOV3-FECHA-DD            PIC 9(2).
           03 REG-NOV3-SUCURSAL                PIC X(3).
           03 REG-NOV3-TIP_CLASE               PIC X(4).
           03 REG-NOV3-HORAS                   PIC 9(2)V99.

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
       77  PROF-EOF    PIC XX  VALUE "NO".

       77  TIP-ESTADO  PIC XX.
       77  TIP-EOF     PIC XX  VALUE "NO".

       77  NOV1-ESTADO PIC XX.
       77  NOV1-EOF    PIC XX  VALUE "NO".

       77  NOV2-ESTADO PIC XX.
       77  NOV2-EOF    PIC XX  VALUE "NO".

       77  NOV3-ESTADO PIC XX.
       77  NOV3-EOF    PIC XX  VALUE "NO".

       77  NOV-ESTADO  PIC XX.
       77  NOV-EOF     PIC XX  VALUE "NO".

       77  WS-TIP-INDICE   PIC 9(4).
       01  TAB-TIP_CLASE.
           03  TAB-TIP                 OCCURS 50 TIMES.
               05 TAB-TIP-TIP_CLASE    PIC X(4).
               05 TAB-TIP-DESC         PIC X(20).
               05 TAB-TIP-TARIFA       PIC 9(5)V99.

       77  WS-TOT-HOJAS        PIC 9(3).
       77  WS-TOT-GRAL-HORAS   PIC 9(5)V99.
       77  WS-TOT-GRAL-IMPORTE PIC 9(10)V99.
       77  WS-TOT-PROF-HORAS   PIC 9(4)V99.
       77  WS-TOT-PROF-IMPORTE PIC 9(9)V99.
       77  WS-TOT-FECH-HORAS   PIC 9(3)V99.
       77  WS-TOT-FECH-IMPORTE PIC 9(8)V99.

       01  WS-FECHA-ACT.
           03 WS-FECHA-ACT-AA PIC 9(4).
           03 WS-FECHA-ACT-MM PIC 9(2).
           03 WS-FECHA-ACT-DD PIC 9(2).

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

       01  WS-NOV-INV.
           03 WS-NOV-INV-NUMERO    PIC X(5)    VALUE "ZZZZZ".
           03 WS-NOV-INV-FECHA     PIC 9(8)    VALUE 99999999.
           03 WS-NOV-INV-SUCURSAL  PIC X(3)    VALUE "ZZZ".
           03 WS-NOV-INV-TIP_CLASE PIC X(4)    VALUE "9999".
           03 WS-NOV-INV-HORAS     PIC 9(2)V99 VALUE 0.

       01  WS-NOV-ACT.
           03 WS-NOV-ACT-NUMERO                    PIC X(5).
           03 WS-NOV-ACT-FECHA                     PIC 9(8).
           03 FILLER REDEFINES WS-NOV-ACT-FECHA.
               05 WS-NOV-ACT-FECHA-AA              PIC 9(4).
               05 WS-NOV-ACT-FECHA-MM              PIC 9(2).
               05 WS-NOV-ACT-FECHA-DD              PIC 9(2).
           03 WS-NOV-ACT-SUCURSAL                  PIC X(3).
           03 WS-NOV-ACT-TIP_CLASE                 PIC X(4).
           03 WS-NOV-ACT-HORAS                     PIC 9(2)V99.
           03 WS-NOV-ACT-IMPORTE                   PIC 9(7)V99.

       01  PTR-CAB.
           03 FILLER       PIC X(6)    VALUE "FECHA ".
           03 PTR-CAB-DD   PIC 99.
           03 FILLER       PIC X(1)    VALUE "/".
           03 PTR-CAB-MM   PIC 99.
           03 FILLER       PIC X(1)    VALUE "/".
           03 PTR-CAB-AA   PIC 9999.
           03 FILLER       PIC X(48)   VALUE SPACES.
           03 FILLER       PIC X(5)    VALUE "HOJA ".
           03 PTR-CAB-HOJA PIC 999.

       01  PTR-TIT.
           03 FILLER   PIC X(20)   VALUE SPACES.
           03 FILLER   PIC X(26)   VALUE "Listado de horas aplicadas".
           03 FILLER   PIC X(20)   VALUE SPACES.

       01  PTR-PROF.
           03 FILLER           PIC X(10) VALUE "Profesor: ".
           03 PTR-PROF-NUMERO  PIC X(5).
           03 FILLER           PIC X(4) VALUE SPACES.
           03 FILLER           PIC X(8) VALUE "Nombre: ".
           03 PTR-PROF-NOMBRE  PIC X(25).

       01  PTR-NOV-CAB.
           03 FILLER PIC X(5)  VALUE "Fecha".
           03 FILLER PIC X(6)  VALUE SPACES.
           03 FILLER PIC X(8)  VALUE "Sucursal".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(13) VALUE "Tipo de clase".
           03 FILLER PIC X(8)  VALUE SPACES.
           03 FILLER PIC X(6)  VALUE "Tarifa".
           03 FILLER PIC X(3)  VALUE SPACES.
           03 FILLER PIC X(5)  VALUE "Horas".
           03 FILLER PIC X(4)  VALUE SPACES.
           03 FILLER PIC X(7)  VALUE "Importe".

       01  PTR-NOV-DET.
           03 PTR-NOV-DET-DD           PIC 99.
           03 FILLER                   PIC X(1)        VALUE "/".
           03 PTR-NOV-DET-MM           PIC 99.
           03 FILLER                   PIC X(1)        VALUE "/".
           03 PTR-NOV-DET-AA           PIC 9999.
           03 FILLER                   PIC X(1)        VALUE SPACES.
           03 PTR-NOV-DET-SUCURSAL     PIC X(3).
           03 FILLER                   PIC X(6)        VALUE SPACES.
           03 PTR-NOV-DET-TIP_CLASE    PIC X(20).
           03 FILLER                   PIC X(1)        VALUE SPACES.
           03 PTR-NOV-DET-TARIFA       PIC ZZZZ9,99.
           03 FILLER                   PIC X(4)        VALUE SPACES.
           03 PTR-NOV-DET-HORAS        PIC Z9,99.
           03 FILLER                   PIC X(4)        VALUE SPACES.
           03 PTR-NOV-DET-IMPORTE      PIC ZZZZZZ9,99.

       01  PTR-TOT-FECH.
           03 FILLER               PIC X(17)  VALUE "Totales por fecha".
           03 FILLER               PIC X(35)  VALUE SPACES.
           03 PTR-TOT-FECH-HORAS   PIC ZZ9,99.
           03 FILLER               PIC X(3)   VALUE SPACES.
           03 PTR-TOT-FECH-IMPORTE PIC ZZZZZZZ9,99.

       01  PTR-TOT-PROF.
           03 FILLER            PIC X(20) VALUE "Totales por profesor".
           03 FILLER               PIC X(31)  VALUE SPACES.
           03 PTR-TOT-PROF-HORAS   PIC ZZZ9,99.
           03 FILLER               PIC X(2)   VALUE SPACES.
           03 PTR-TOT-PROF-IMPORTE PIC ZZZZZZZZ9,99.

       01  PTR-TOT-GRAL.
           03 FILLER               PIC X(13)      VALUE "Total general".
           03 FILLER               PIC X(37)      VALUE SPACES.
           03 PTR-TOT-GRAL-HORAS   PIC ZZZZ9,99.
           03 FILLER               PIC X(1)       VALUE SPACES.
           03 PTR-TOT-GRAL-IMPORTE PIC ZZZZZZZZZ9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIO.
           PERFORM LEO-PROFESORES.
           PERFORM LEO-NOVTIMES1.
           PERFORM LEO-NOVTIMES2.
           PERFORM LEO-NOVTIMES3.
           PERFORM CARGO-TIPOS_CLASE.
           MOVE ZERO TO WS-TOT-HOJAS.
           MOVE ZERO TO WS-TOT-GRAL-HORAS.
           MOVE ZERO TO WS-TOT-GRAL-IMPORTE.
           PERFORM PROC-GENERAL UNTIL
                           PROF-EOF="SI"
                           AND NOV1-EOF="SI"
                           AND NOV2-EOF="SI"
                           AND NOV3-EOF="SI".
           PERFORM IMPRIMO-TOT-GRAL.
           PERFORM FIN.
           STOP RUN.

       INICIO.
           ACCEPT WS-FECHA-ACT FROM DATE YYYYMMDD.
           MOVE WS-FECHA-ACT-DD TO PTR-CAB-DD.
           MOVE WS-FECHA-ACT-MM TO PTR-CAB-MM.
           MOVE WS-FECHA-ACT-AA TO PTR-CAB-AA.

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
           IF (NOV1-EOF = "SI") THEN
               MOVE WS-NOV-INV TO REG-NOV1
           END-IF.

       LEO-NOVTIMES2.
           READ NOVTIMES2 AT END MOVE "SI" TO NOV2-EOF.
           IF NOV2-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ NovTimes2: " NOV2-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.
           IF (NOV2-EOF = "SI") THEN
               MOVE WS-NOV-INV TO REG-NOV2
           END-IF.

       LEO-NOVTIMES3.
           READ NOVTIMES3 AT END MOVE "SI" TO NOV3-EOF.
           IF NOV3-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ NovTimes3: " NOV3-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.
           IF (NOV3-EOF = "SI") THEN
               MOVE WS-NOV-INV TO REG-NOV3
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
           MOVE ZERO TO WS-TOT-PROF-IMPORTE.
           PERFORM IMPRIMO-CAB.
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
           ADD WS-TOT-PROF-IMPORTE TO WS-TOT-GRAL-IMPORTE.
           PERFORM IMPRIMO-TOT-PROF.

       DET-PROFESOR-MEN.
           MOVE "ZZZZZ" TO WS-NOV-MEN-NUMERO.
           MOVE REG-PROF-NUMERO TO WS-NOV-MEN-NUMERO
           IF (REG-NOV1-NUMERO < WS-NOV-MEN-NUMERO) THEN
               MOVE REG-NOV1-NUMERO TO WS-NOV-MEN-NUMERO
           END-IF.
           IF (REG-NOV2-NUMERO < WS-NOV-MEN-NUMERO) THEN
               MOVE REG-NOV2-NUMERO TO WS-NOV-MEN-NUMERO
           END-IF.
           IF (REG-NOV3-NUMERO < WS-NOV-MEN-NUMERO) THEN
               MOVE REG-NOV3-NUMERO TO WS-NOV-MEN-NUMERO
           END-IF.

       CARGO-PROFESOR.
           MOVE REG-PROF-NUMERO TO WS-PROF-ACT-NUMERO.
           MOVE REG-PROF-NOMBRE TO WS-PROF-ACT-NOMBRE.
           PERFORM LEO-PROFESORES.

       PROC-PROFESOR.
           PERFORM DET-FECHA-MEN.
           MOVE ZERO TO WS-TOT-FECH-HORAS.
           MOVE ZERO TO WS-TOT-FECH-IMPORTE.
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
           ADD WS-TOT-FECH-IMPORTE TO WS-TOT-PROF-IMPORTE.
           PERFORM IMPRIMO-TOT-FECH.

       DET-FECHA-MEN.
           MOVE 99999999 TO WS-NOV-MEN-FECHA.
           IF (REG-NOV1-NUMERO = WS-NOV-MEN-NUMERO) THEN
               MOVE REG-NOV1-FECHA TO WS-NOV-MEN-FECHA
           END-IF.
           IF (REG-NOV2-NUMERO = WS-NOV-MEN-NUMERO) THEN
               IF (REG-NOV2-FECHA < WS-NOV-MEN-FECHA) THEN
                   MOVE REG-NOV2-FECHA TO WS-NOV-MEN-FECHA
               END-IF
           END-IF.
           IF (REG-NOV3-NUMERO = WS-NOV-MEN-NUMERO) THEN
               IF (REG-NOV3-FECHA < WS-NOV-MEN-FECHA) THEN
                   MOVE REG-NOV3-FECHA TO WS-NOV-MEN-FECHA
               END-IF
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
           MOVE "ZZZ" TO WS-NOV-MEN-SUCURSAL.
           IF (REG-NOV1-NUMERO = WS-NOV-MEN-NUMERO) THEN
               IF (REG-NOV1-FECHA = WS-NOV-MEN-FECHA) THEN
                   MOVE REG-NOV1-SUCURSAL TO WS-NOV-MEN-SUCURSAL
               END-IF
           END-IF.
           IF (REG-NOV2-NUMERO = WS-NOV-MEN-NUMERO) THEN
               IF (REG-NOV2-FECHA = WS-NOV-MEN-FECHA) THEN
                   IF (REG-NOV2-SUCURSAL < WS-NOV-MEN-SUCURSAL) THEN
                       MOVE REG-NOV2-SUCURSAL TO WS-NOV-MEN-SUCURSAL
                   END-IF
               END-IF
           END-IF.
           IF (REG-NOV3-NUMERO = WS-NOV-MEN-NUMERO) THEN
               IF (REG-NOV3-FECHA = WS-NOV-MEN-FECHA) THEN
                   IF (REG-NOV3-SUCURSAL < WS-NOV-MEN-SUCURSAL) THEN
                       MOVE REG-NOV3-SUCURSAL TO WS-NOV-MEN-SUCURSAL
                   END-IF
               END-IF
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
           MOVE WS-NOV-ACT TO REG-NOV.
           WRITE REG-NOV.
           PERFORM BUSCO-TIPO_CLASE.
           MOVE WS-TIP-ACT-TARIFA TO WS-NOV-ACT-IMPORTE.
           MULTIPLY WS-NOV-ACT-HORAS BY WS-NOV-ACT-IMPORTE.
           ADD WS-NOV-ACT-HORAS TO WS-TOT-FECH-HORAS.
           ADD WS-NOV-ACT-IMPORTE TO WS-TOT-FECH-IMPORTE.
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

       IMPRIMO-CAB.
           ADD 1 TO WS-TOT-HOJAS.
           MOVE WS-TOT-HOJAS TO PTR-CAB-HOJA.
           MOVE WS-PROF-ACT-NUMERO TO PTR-PROF-NUMERO.
           MOVE WS-PROF-ACT-NOMBRE TO PTR-PROF-NOMBRE.
           DISPLAY PTR-CAB.
           DISPLAY PTR-TIT.
           DISPLAY PTR-PROF.
           DISPLAY PTR-NOV-CAB.

       IMPRIMO-NOV.
           MOVE WS-NOV-ACT-FECHA-DD TO PTR-NOV-DET-DD.
           MOVE WS-NOV-ACT-FECHA-MM TO PTR-NOV-DET-MM.
           MOVE WS-NOV-ACT-FECHA-AA TO PTR-NOV-DET-AA.
           MOVE WS-NOV-ACT-SUCURSAL TO PTR-NOV-DET-SUCURSAL.
           MOVE WS-TIP-ACT-DESC TO PTR-NOV-DET-TIP_CLASE.
           MOVE WS-TIP-ACT-TARIFA TO PTR-NOV-DET-TARIFA.
           MOVE WS-NOV-ACT-HORAS TO PTR-NOV-DET-HORAS.
           MOVE WS-NOV-ACT-IMPORTE TO PTR-NOV-DET-IMPORTE.
           DISPLAY PTR-NOV-DET.

       IMPRIMO-TOT-FECH.
           MOVE WS-TOT-FECH-HORAS TO PTR-TOT-FECH-HORAS.
           MOVE WS-TOT-FECH-IMPORTE TO PTR-TOT-FECH-IMPORTE.
           DISPLAY PTR-TOT-FECH.

       IMPRIMO-TOT-PROF.
           MOVE WS-TOT-PROF-HORAS TO PTR-TOT-PROF-HORAS.
           MOVE WS-TOT-PROF-IMPORTE TO PTR-TOT-PROF-IMPORTE.
           DISPLAY PTR-TOT-PROF.

       IMPRIMO-TOT-GRAL.
           MOVE WS-TOT-GRAL-HORAS TO PTR-TOT-GRAL-HORAS.
           MOVE WS-TOT-GRAL-IMPORTE TO PTR-TOT-GRAL-IMPORTE.
           DISPLAY PTR-TOT-GRAL.

       FIN.
           CLOSE NOVTIMES1.
           CLOSE NOVTIMES2.
           CLOSE NOVTIMES3.
           CLOSE PROFESORES.
           CLOSE TIPOS_CLASE.
           CLOSE NOVTIMES.

       END PROGRAM TP1-A.
