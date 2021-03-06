       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUCURSALES.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SUCURSALES ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS REG-SUC-SUCURSAL
           ALTERNATE RECORD KEY IS REG-SUC-CUIT
           ALTERNATE RECORD KEY IS REG-SUC-RAZON WITH DUPLICATES
           FILE STATUS IS SUC-ESTADO.

       DATA DIVISION.

       FILE SECTION.
       FD  SUCURSALES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Sucursales.dat".
       01  REG-SUC.
           03 REG-SUC-SUCURSAL PIC X(03).
           03 REG-SUC-RAZON    PIC X(25).
           03 REG-SUC-DIRE     PIC X(20).
           03 REG-SUC-TEL      PIC X(20).
           03 REG-SUC-CUIT     PIC 9(11).

       WORKING-STORAGE SECTION.
       01  WS-SUC-GENERICA.
           03 FILLER PIC X(03) VALUE "NNN".
           03 FILLER PIC X(25) VALUE "SIN DESCRIPCION".
           03 FILLER PIC X(20) VALUE "SIN DIRECCION".
           03 FILLER PIC X(20) VALUE "SIN TELEFONO".
           03 FILLER PIC 9(11) VALUE ZEROES.

       LINKAGE SECTION.
       01  SUCURSAL.
           03 SUC-CUIT   PIC 9(11).
           03 SUC-RAZON  PIC X(25).
           03 SUC-ACCION PIC XX.
           03 SUC-ESTADO PIC XX.
              88 SUC-OK  VALUE 00.
              88 SUC-EOF VALUE 10.

       PROCEDURE DIVISION USING SUCURSAL.
       MAIN-PROCEDURE.
           IF (SUC-ACCION = 1) THEN
               PERFORM INICIO
           ELSE IF (SUC-ACCION = 2) THEN
               PERFORM BUSCO
           ELSE IF (SUC-ACCION = 3) THEN
               PERFORM FIN
           ELSE
               MOVE 99 TO SUC-ESTADO
           END-IF.
           EXIT PROGRAM.

       INICIO.
           OPEN INPUT SUCURSALES.

       BUSCO.
           PERFORM LEO
           MOVE REG-SUC-RAZON TO SUC-RAZON.
           MOVE 00 TO SUC-ESTADO.

       LEO.
           MOVE WS-SUC-GENERICA TO REG-SUC.
           MOVE SUC-CUIT TO REG-SUC-CUIT.
           READ SUCURSALES RECORD INTO REG-SUC KEY IS REG-SUC-CUIT.

       FIN.
           CLOSE SUCURSALES.

       END PROGRAM SUCURSALES.
