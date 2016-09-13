       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1-A.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOVTIMES1 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV1-ESTADO.

           SELECT NOVTIMES2 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV2-ESTADO.

           SELECT NOVTIMES3 ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NOV3-ESTADO.

           SELECT PROFESORES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PROF-ESTADO.

           SELECT SUCURSALES ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS SUC-ESTADO.

           SELECT TIPOS_CLASE ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS TIP-ESTADO.

       DATA DIVISION.

       FILE SECTION.
       FD  NOVTIMES1 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "NovTimes1.txt".
       01  NOV1.
           03 NOV1-NUMERO      PIC X(5).
           03 NOV1-FECHA       PIC 9(8).
           03 NOV1-SUCURSAL    PIC X(03).
           03 NOV1-TIP_CLASE   PIC X(04).
           03 NOV1-HORAS       PIC 9(2)V99.

       FD  NOVTIMES2 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "NovTimes2.txt".
       01  NOV2.
           03 NOV2-NUMERO      PIC X(5).
           03 NOV2-FECHA       PIC 9(8).
           03 NOV2-SUCURSAL    PIC X(03).
           03 NOV2-TIP_CLASE   PIC X(04).
           03 NOV2-HORAS       PIC 9(2)V99.

       FD  NOVTIMES3 LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "NovTimes3.txt".
       01  NOV3.
           03 NOV3-NUMERO      PIC X(5).
           03 NOV3-FECHA       PIC 9(8).
           03 NOV3-SUCURSAL    PIC X(03).
           03 NOV3-TIP_CLASE   PIC X(04).
           03 NOV3-HORAS       PIC 9(2)V99.

       FD  PROFESORES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "Profesores.txt".
       01  PROF.
           03 PROF-NUMERO  PIC X(5).
           03 PROF-DNI     PIC 9(8).
           03 PROF-NOMBRE  PIC X(25).
           03 PROF-DIRE    PIC X(20).
           03 PROF-TEL     PIC X(20).

       FD  SUCURSALES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "Sucursales.txt".
       01  SUC.
           03 SUC-SUCURSAL PIC X(03).
           03 SUC-RAZON    PIC X(25).
           03 SUC-DIRE     PIC X(20).
           03 SUC-TEL      PIC X(20).
           03 SUC-CUIT     PIC 9(11).

       FD  TIPOS_CLASE LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "TiposClase.txt".
       01  TIP.
           03 TIP-TIP_CLASE    PIC X(04).
           03 TIP-DESC         PIC X(20).
           03 TIP-TARIFA       PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       77  NOV1-ESTADO PIC XX.
       77  NOV1-EOF    PIC XX  VALUE   "NO".

       77  NOV2-ESTADO PIC XX.
       77  NOV2-EOF    PIC XX  VALUE   "NO".

       77  NOV3-ESTADO PIC XX.
       77  NOV3-EOF    PIC XX  VALUE   "NO".

       77  PROF-ESTADO PIC XX.
       77  PROF-EOF    PIC XX  VALUE   "NO".

       77  SUC-ESTADO  PIC XX.
       77  SUC-EOF     PIC XX  VALUE   "NO".

       77  TIP-ESTADO  PIC XX.
       77  TIP-EOF     PIC XX  VALUE   "NO".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIO.
           PERFORM LEO-NOVTIMES1.
           PERFORM LEO-NOVTIMES2.
           PERFORM LEO-NOVTIMES3.
           PERFORM LEO-PROFESORES.
           PERFORM LEO-SUCURSALES.
           PERFORM LEO-TIPOS_CLASE.
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

           OPEN INPUT SUCURSALES.
           IF SUC-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN Sucursales: " SUC-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF

           OPEN INPUT TIPOS_CLASE.
           IF TIP-ESTADO NOT = ZERO THEN
               DISPLAY "ERROR EN OPEN TiposClase: " TIP-ESTADO
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

       LEO-PROFESORES.
           READ PROFESORES AT END MOVE "SI" TO PROF-EOF.
           IF PROF-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ Profesores: " PROF-ESTADO
               PERFORM FIN
               STOP RUN
           END-IF.

       LEO-SUCURSALES.
           READ SUCURSALES AT END MOVE "SI" TO SUC-EOF.
           IF SUC-ESTADO NOT = ZERO AND 10 THEN
               DISPLAY "ERROR EN READ Sucursales: " SUC-ESTADO
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

       FIN.
           CLOSE NOVTIMES1.
           CLOSE NOVTIMES2.
           CLOSE NOVTIMES3.
           CLOSE PROFESORES.
           CLOSE SUCURSALES.
           CLOSE TIPOS_CLASE.

       END PROGRAM TP1-A.
