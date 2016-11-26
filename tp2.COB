       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP2.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDEN ASSIGN TO DISK
           FILE STATUS IS ORD-ESTADO.

           SELECT NOVTIMES ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           RECORD KEY IS REG-TIM-CLAVE
           ALTERNATE RECORD KEY IS REG-TIM-CUIT WITH DUPLICATES
           FILE STATUS IS TIM-ESTADO.

           SELECT PROFESORES ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           RECORD KEY IS REG-PROF-NUMERO
           FILE STATUS IS PROF-ESTADO.

           SELECT TARIFAS ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           RECORD KEY IS REG-TAR-CLAVE
           FILE STATUS IS TAR-ESTADO.

           SELECT PARAMETROS ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS PAR-ESTADO.

       DATA DIVISION.

       FILE SECTION.
       SD  ORDEN DATA RECORD IS REG-ORD.
       01  REG-ORD.
           03 REG-ORD-NUMERO    PIC X(5).
           03 REG-ORD-FECHA     PIC 9(8).
           03 REG-ORD-CUIT      PIC 9(11).
           03 REG-ORD-TIP_CLASE PIC X(4).
           03 REG-ORD-HORAS     PIC 9(2)V99.
           03 REG-ORD-RAZON     PIC X(25).

       FD  NOVTIMES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/NovTimes.dat".
       01  REG-TIM.
           03 REG-TIM-CLAVE.
              05 REG-TIM-NUMERO PIC X(5).
              05 REG-TIM-FECHA  PIC 9(8).
              05 REG-TIM-CUIT   PIC 9(11).
              05 REG-TIM-SEC    PIC 9(4).
           03 REG-TIM-TIP_CLASE PIC X(4).
           03 REG-TIM-HORAS     PIC 9(2)V99.

       FD  PROFESORES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Profesores.dat".
       01  REG-PROF.
           03 REG-PROF-NUMERO PIC X(5).
           03 REG-PROF-DNI    PIC 9(8).
           03 REG-PROF-NOMBRE PIC X(25).
           03 REG-PROF-DIRE   PIC X(20).
           03 REG-PROF-TEL    PIC X(20).

       FD  TARIFAS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Tarifas.dat".
       01  REG-TAR.
           03 REG-TAR-CLAVE.
              05 REG-TAR-TIP_CLASE PIC X(4).
              05 REG-TAR-VIG-DES   PIC 9(8).
           03 REG-TAR-TARIFA       PIC 9(5)V99.

       FD  PARAMETROS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "files/Parametros.dat".
       01  REG-PAR.
           03 REG-PAR-CUIT-DESDE PIC 9(11).
           03 REG-PAR-CUIT-HASTA PIC 9(11).

       WORKING-STORAGE SECTION.
       77  ORD-ESTADO PIC XX.
           88 ORD-OK  VALUE 00.
           88 ORD-EOF VALUE 10.

       77  TIM-ESTADO PIC XX.
           88 TIM-OK  VALUE 00.
           88 TIM-EOF VALUE 10.

       77  PROF-ESTADO PIC XX.
           88 PROF-OK  VALUE 00.
           88 PROF-EOF VALUE 10.

       77  TAR-ESTADO PIC XX.
           88 TAR-OK  VALUE 00.
           88 TAR-EOF VALUE 10.

       77  PAR-ESTADO PIC XX.
           88 PAR-OK  VALUE 00.
           88 PAR-EOF VALUE 10.

       01  SUCURSAL.
           03 SUC-CUIT   PIC 9(11).
           03 SUC-RAZON  PIC X(25).
           03 SUC-ACCION PIC XX.
           03 SUC-ESTADO PIC XX.
              88 SUC-OK  VALUE 00.

       01  WS-PROF-GENERICO.
           03 FILLER PIC X(5)  VALUE "NNNNN".
           03 FILLER PIC 9(8)  VALUE ZEROES.
           03 FILLER PIC X(25) VALUE "SIN DESCRIPCION".
           03 FILLER PIC X(20) VALUE "SIN DIRECCION".
           03 FILLER PIC X(20) VALUE "SIN TELEFONO".

       01  WS-TAR-GENERICA.
           03 FILLER PIC X(4)    VALUE "NNNN".
           03 FILLER PIC 9(8)    VALUE 99999999.
           03 FILLER PIC 9(5)V99 VALUE ZEROES.

       01  WS-RAZON-ACT       PIC X(25).
       01  WS-CUIT-ACT        PIC 9(11).
       01  WS-FECHA-ACT       PIC X(8).
       01  FILLER REDEFINES WS-FECHA-ACT.
           03 WS-FECHA-ACT-AA PIC 9(4).
           03 WS-FECHA-ACT-MM PIC 9(2).
           03 WS-FECHA-ACT-DD PIC 9(2).
       01  WS-NUMERO-ACT      PIC X(5).
       01  WS-TAR-ACTUAL.
           03 WS-TAR-ACTUAL-TIP_CLASE PIC X(4).
           03 WS-TAR-ACTUAL-VIG-DES   PIC 9(8).
           03 WS-TAR-ACTUAL-TARIFA    PIC 9(5)V99.
       01  WS-IMPORTE-ACT     PIC 9(7)V99.

       01  WS-TOT-HOJAS            PIC 9(3).
       01  WS-TOT-FECHA-HORAS      PIC 9(3)V99.
       01  WS-TOT-FECHA-IMPORTE    PIC 9(8)V99.
       01  WS-TOT-SUCURSAL-HORAS   PIC 9(4)V99.
       01  WS-TOT-SUCURSAL-IMPORTE PIC 9(9)V99.
       01  WS-TOT-GENERAL-HORAS    PIC 9(5)V99.
       01  WS-TOT-GENERAL-IMPORTE  PIC 9(10)V99.

       01  PTR-CABECERA.
           03 FILLER       PIC X(6)  VALUE "FECHA ".
           03 PTR-CAB-DD   PIC 99.
           03 FILLER       PIC X(1)  VALUE "/".
           03 PTR-CAB-MM   PIC 99.
           03 FILLER       PIC X(1)  VALUE "/".
           03 PTR-CAB-AA   PIC 9999.
           03 FILLER       PIC X(48) VALUE SPACES.
           03 FILLER       PIC X(5)  VALUE "HOJA ".
           03 PTR-CAB-HOJA PIC 999.

       01  PTR-TITULO.
           03 FILLER PIC X(20) VALUE SPACES.
           03 FILLER PIC X(28) VALUE "Horas aplicadas por sucursal".
           03 FILLER PIC X(20) VALUE SPACES.

       01  PTR-SUCURSAL-RAZON.
           03 FILLER        PIC X(9) VALUE "Sucursal:".
           03 FILLER        PIC X(1) VALUE SPACES.
           03 PTR-SUC-RAZON PIC X(25).

       01  PTR-SUCURSAL-CUIT.
           03 FILLER       PIC X(5) VALUE "Cuit:".
           03 FILLER       PIC X(1) VALUE SPACES.
           03 PTR-SUC-CUIT PIC 9(11).

       01  PTR-NOV-CABECERA.
           03 FILLER PIC X(5)  VALUE "Fecha".
           03 FILLER PIC X(6)  VALUE SPACES.
           03 FILLER PIC X(8)  VALUE "Profesor".
           03 FILLER PIC X(1)  VALUE SPACES.
           03 FILLER PIC X(6)  VALUE "Nombre".
           03 FILLER PIC X(20) VALUE SPACES.
           03 FILLER PIC X(5)  VALUE "Horas".
           03 FILLER PIC X(3)  VALUE SPACES.
           03 FILLER PIC X(7)  VALUE "Importe".

       01  PTR-NOV-DETALLE.
           03 PTR-NOV-DETALLE-DD      PIC 99.
           03 FILLER                  PIC X(1)        VALUE "/".
           03 PTR-NOV-DETALLE-MM      PIC 99.
           03 FILLER                  PIC X(1)        VALUE "/".
           03 PTR-NOV-DETALLE-AA      PIC 9999.
           03 FILLER                  PIC X(1)        VALUE SPACES.
           03 PTR-NOV-DETALLE-NUMERO  PIC X(5).
           03 FILLER                  PIC X(4)        VALUE SPACES.
           03 PTR-NOV-DETALLE-NOMBRE  PIC X(25).
           03 FILLER                  PIC X(3)        VALUE SPACES.
           03 PTR-NOV-DETALLE-HORAS   PIC Z9,99.
           03 FILLER                  PIC X(4)        VALUE SPACES.
           03 PTR-NOV-DETALLE-IMPORTE PIC ZZZZZZ9,99.

       01  PTR-TOT-FECHA.
           03 FILLER                PIC X(17)     VALUE
                                                    "Totales por fecha".
           03 FILLER                PIC X(30)     VALUE SPACES.
           03 PTR-TOT-FECHA-HORAS   PIC ZZ9,99.
           03 FILLER                PIC X(3)      VALUE SPACES.
           03 PTR-TOT-FECHA-IMPORTE PIC Z(7)9,99.

       01  PTR-TOT-SUCURSAL.
           03 FILLER                   PIC X(20)     VALUE
                                                 "Totales por sucursal".
           03 FILLER                   PIC X(26)     VALUE SPACES.
           03 PTR-TOT-SUCURSAL-HORAS   PIC ZZZ9,99.
           03 FILLER                   PIC X(2)      VALUE SPACES.
           03 PTR-TOT-SUCURSAL-IMPORTE PIC Z(8)9,99.

       01  PTR-TOT-GENERAL.
           03 FILLER                  PIC X(13)     VALUE
                                                        "Total general".
           03 FILLER                  PIC X(32)     VALUE SPACES.
           03 PTR-TOT-GENERAL-HORAS   PIC ZZZZ9,99.
           03 FILLER                  PIC X(1)      VALUE SPACES.
           03 PTR-TOT-GENERAL-IMPORTE PIC Z(9)9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT ORDEN ON ASCENDING KEY REG-ORD-RAZON
                      ON ASCENDING KEY REG-ORD-CUIT
                      ON ASCENDING KEY REG-ORD-FECHA
                      ON ASCENDING KEY REG-ORD-NUMERO
                INPUT PROCEDURE IS ENTRADA
                OUTPUT PROCEDURE IS SALIDA.
           STOP RUN.

       ENTRADA SECTION.
       PROCESO-ENTRADA.
           PERFORM INICIO-ENTRADA.
           PERFORM LEO-PARAMETROS.
           PERFORM POSICIONO-NOVTIMES.
           IF (TIM-OK) THEN
               PERFORM LEO-NOVTIMES
               PERFORM PROC-ENTRADA
                       UNTIL (TIM-EOF
                              OR REG-TIM-CUIT > REG-PAR-CUIT-HASTA)
           END-IF.
           PERFORM FIN-ENTRADA.

       ENTRADA-PROCESOS SECTION.
       INICIO-ENTRADA.
           OPEN INPUT NOVTIMES.
           IF (NOT TIM-OK) THEN
               DISPLAY "ERROR EN OPEN NovTimes: " TIM-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

           OPEN INPUT PARAMETROS.
           IF (NOT PAR-OK) THEN
               DISPLAY "ERROR EN OPEN Parametros: " PAR-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

           MOVE 1 TO SUC-ACCION.
           CALL 'SUCURSALES' USING SUCURSAL.
           IF (NOT SUC-OK) THEN
               DISPLAY "ERROR EN CALL SUCURSALES: " SUC-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

       LEO-PARAMETROS.
           READ PARAMETROS RECORD INTO REG-PAR.
           IF ((NOT PAR-OK) AND (NOT PAR-EOF)) THEN
               DISPLAY "ERROR EN READ Profesores: " PROF-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

       POSICIONO-NOVTIMES.
           MOVE REG-PAR-CUIT-DESDE TO REG-TIM-CUIT.
           START NOVTIMES KEY IS >= REG-TIM-CUIT.

       LEO-NOVTIMES.
           READ NOVTIMES NEXT RECORD INTO REG-TIM.
           IF ((NOT TIM-OK) AND (NOT TIM-EOF)) THEN
               DISPLAY "ERROR EN READ Times: " TIM-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

       PROC-ENTRADA.
           PERFORM BUSCO-SUCURSAL.
           IF (SUC-OK) THEN
               PERFORM LIBERA-REGISTRO
               PERFORM LEO-NOVTIMES
           END-IF.

       BUSCO-SUCURSAL.
           MOVE REG-TIM-CUIT TO SUC-CUIT.
           MOVE 2 TO SUC-ACCION.
           CALL 'SUCURSALES' USING SUCURSAL.
           IF (NOT SUC-OK) THEN
               DISPLAY "ERROR EN CALL SUCURSALES: " SUC-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

       LIBERA-REGISTRO.
           MOVE REG-TIM-NUMERO TO REG-ORD-NUMERO.
           MOVE REG-TIM-FECHA TO REG-ORD-FECHA.
           MOVE REG-TIM-CUIT TO REG-ORD-CUIT.
           MOVE REG-TIM-TIP_CLASE TO REG-ORD-TIP_CLASE.
           MOVE REG-TIM-HORAS TO REG-ORD-HORAS.
           MOVE SUC-RAZON TO REG-ORD-RAZON.
           RELEASE REG-ORD.

       FIN-ENTRADA.
           CLOSE NOVTIMES.
           CLOSE PARAMETROS.

           MOVE 3 TO SUC-ACCION.
           CALL 'SUCURSALES' USING SUCURSAL.
           IF (NOT SUC-OK) THEN
               DISPLAY "ERROR EN CALL SUCURSALES: " SUC-ESTADO
               PERFORM FIN-ENTRADA
               STOP RUN
           END-IF.

       SALIDA SECTION.
       PROCESO-SALIDA.
           PERFORM INICIO-SALIDA.
           PERFORM RETORNO-REGISTRO.
           PERFORM INICIO-TOT-GENERAL.
           PERFORM PROC-SALIDA UNTIL
                               (ORD-EOF).
           PERFORM IMPRIMO-TOT-GENERAL.
           PERFORM FIN-SALIDA.

       SALIDA-PROCESOS SECTION.
       INICIO-SALIDA.
           ACCEPT WS-FECHA-ACT FROM DATE YYYYMMDD.
           MOVE WS-FECHA-ACT-DD TO PTR-CAB-DD.
           MOVE WS-FECHA-ACT-MM TO PTR-CAB-MM.
           MOVE WS-FECHA-ACT-AA TO PTR-CAB-AA.
           MOVE ZERO TO WS-TOT-HOJAS.

           OPEN INPUT PROFESORES.
           IF (NOT PROF-OK) THEN
               DISPLAY "ERROR EN OPEN Profesores: " PROF-ESTADO
               PERFORM FIN-SALIDA
               STOP RUN
           END-IF.

           OPEN INPUT TARIFAS.
           IF (NOT TAR-OK) THEN
               DISPLAY "ERROR EN OPEN Tarifas: " TAR-ESTADO
               PERFORM FIN-SALIDA
               STOP RUN
           END-IF.

       RETORNO-REGISTRO.
           RETURN ORDEN RECORD INTO REG-ORD
                  AT END MOVE 10 TO ORD-ESTADO.

       INICIO-TOT-GENERAL.
           MOVE ZERO TO WS-TOT-GENERAL-HORAS.
           MOVE ZERO TO WS-TOT-GENERAL-IMPORTE.

       PROC-SALIDA.
           MOVE REG-ORD-RAZON TO WS-RAZON-ACT.

           PERFORM INICIO-TOT-SUCURSAL.
           PERFORM PROC-SUCURSAL
                   UNTIL
                   (ORD-EOF
                    OR WS-RAZON-ACT NOT = REG-ORD-RAZON).
           PERFORM SUMO-TOT-GENERAL.
           PERFORM IMPRIMO-TOT-SUCURSAL.

       IMPRIMO-TOT-GENERAL.
           MOVE WS-TOT-GENERAL-HORAS TO PTR-TOT-GENERAL-HORAS.
           MOVE WS-TOT-GENERAL-IMPORTE TO PTR-TOT-GENERAL-IMPORTE.
           DISPLAY PTR-TOT-GENERAL.

       INICIO-TOT-SUCURSAL.
           MOVE ZERO TO WS-TOT-SUCURSAL-HORAS.
           MOVE ZERO TO WS-TOT-SUCURSAL-IMPORTE.

       SUMO-TOT-GENERAL.
           ADD WS-TOT-SUCURSAL-HORAS TO WS-TOT-GENERAL-HORAS.
           ADD WS-TOT-SUCURSAL-IMPORTE TO WS-TOT-GENERAL-IMPORTE.

       IMPRIMO-TOT-SUCURSAL.
           MOVE WS-TOT-SUCURSAL-HORAS TO PTR-TOT-SUCURSAL-HORAS.
           MOVE WS-TOT-SUCURSAL-IMPORTE TO PTR-TOT-SUCURSAL-IMPORTE.
           DISPLAY PTR-TOT-SUCURSAL.

       PROC-SUCURSAL.
           MOVE REG-ORD-CUIT TO WS-CUIT-ACT.
           PERFORM IMPRIMO-CAB-SUCURSAL.
           PERFORM PROC-CUIT
                   UNTIL
                   (ORD-EOF
                    OR WS-RAZON-ACT NOT = REG-ORD-RAZON
                    OR WS-CUIT-ACT NOT = REG-ORD-CUIT).

       IMPRIMO-CAB-SUCURSAL.
           ADD 1 TO WS-TOT-HOJAS.
           MOVE WS-TOT-HOJAS TO PTR-CAB-HOJA.
           MOVE REG-ORD-RAZON TO PTR-SUC-RAZON.
           MOVE REG-ORD-CUIT TO PTR-SUC-CUIT.
           DISPLAY PTR-CABECERA.
           DISPLAY PTR-TITULO.
           DISPLAY PTR-SUCURSAL-RAZON.
           DISPLAY PTR-SUCURSAL-CUIT.
           DISPLAY PTR-NOV-CABECERA.

       PROC-CUIT.
           MOVE REG-ORD-FECHA TO WS-FECHA-ACT.
           PERFORM INICIO-TOT-FECHA.
           PERFORM PROC-FECHA
                   UNTIL
                   (ORD-EOF
                    OR WS-RAZON-ACT NOT = REG-ORD-RAZON
                    OR WS-CUIT-ACT NOT = REG-ORD-CUIT
                    OR WS-FECHA-ACT NOT = REG-ORD-FECHA).
           PERFORM SUMO-TOT-SUCURSAL.
           PERFORM IMPRIMO-TOT-FECHA.

       INICIO-TOT-FECHA.
           MOVE ZERO TO WS-TOT-FECHA-HORAS.
           MOVE ZERO TO WS-TOT-FECHA-IMPORTE.

       SUMO-TOT-SUCURSAL.
           ADD WS-TOT-FECHA-HORAS TO WS-TOT-SUCURSAL-HORAS.
           ADD WS-TOT-FECHA-IMPORTE TO WS-TOT-SUCURSAL-IMPORTE.

       IMPRIMO-TOT-FECHA.
           MOVE WS-TOT-FECHA-HORAS TO PTR-TOT-FECHA-HORAS.
           MOVE WS-TOT-FECHA-IMPORTE TO PTR-TOT-FECHA-IMPORTE.
           DISPLAY PTR-TOT-FECHA.

       PROC-FECHA.
           MOVE REG-ORD-NUMERO TO WS-NUMERO-ACT.
           PERFORM POSICIONO-PROFESOR.
           IF (PROF-OK) THEN
               PERFORM LEO-PROFESOR
           END-IF.
           PERFORM PORC-PROFESOR
                   UNTIL
                   (ORD-EOF
                    OR WS-RAZON-ACT NOT = REG-ORD-RAZON
                    OR WS-CUIT-ACT NOT = REG-ORD-CUIT
                    OR WS-FECHA-ACT NOT = REG-ORD-FECHA
                    OR WS-NUMERO-ACT NOT = REG-ORD-NUMERO).

       POSICIONO-PROFESOR.
           MOVE WS-PROF-GENERICO TO REG-PROF
           MOVE REG-ORD-NUMERO TO REG-PROF-NUMERO.
           START PROFESORES KEY IS = REG-PROF-NUMERO.

       LEO-PROFESOR.
           READ PROFESORES NEXT RECORD INTO REG-PROF.

       PORC-PROFESOR.
           PERFORM BUSCO-TARIFA.
           PERFORM SUMO-TOT-FECHA.
           PERFORM IMPRIMO-NOVTIMES.
           PERFORM RETORNO-REGISTRO.

       IMPRIMO-NOVTIMES.
           MOVE WS-FECHA-ACT-DD TO PTR-NOV-DETALLE-DD.
           MOVE WS-FECHA-ACT-MM TO PTR-NOV-DETALLE-MM.
           MOVE WS-FECHA-ACT-AA TO PTR-NOV-DETALLE-AA.
           MOVE WS-NUMERO-ACT TO PTR-NOV-DETALLE-NUMERO.
           MOVE REG-PROF-NOMBRE TO PTR-NOV-DETALLE-NOMBRE.
           MOVE REG-ORD-HORAS TO PTR-NOV-DETALLE-HORAS.
           MOVE WS-IMPORTE-ACT TO PTR-NOV-DETALLE-IMPORTE.
           DISPLAY PTR-NOV-DETALLE.

       SUMO-TOT-FECHA.
           ADD REG-ORD-HORAS TO WS-TOT-FECHA-HORAS.
           MULTIPLY REG-ORD-HORAS BY WS-TAR-ACTUAL-TARIFA
                                  GIVING WS-IMPORTE-ACT.
           ADD WS-IMPORTE-ACT TO WS-TOT-FECHA-IMPORTE.

       BUSCO-TARIFA.
           PERFORM POSICIONO-TARIFA.
           IF (TAR-OK) THEN
               PERFORM LEO-TARIFA
               PERFORM ASIGNO-TARIFA-ACT
                       UNTIL (TAR-EOF
                            OR REG-ORD-TIP_CLASE NOT = REG-TAR-TIP_CLASE
                            OR REG-ORD-FECHA < REG-TAR-VIG-DES)
               IF (REG-ORD-TIP_CLASE NOT = WS-TAR-ACTUAL-TIP_CLASE) THEN
                   MOVE WS-TAR-GENERICA TO WS-TAR-ACTUAL
               END-IF
           END-IF.

       POSICIONO-TARIFA.
           MOVE WS-TAR-GENERICA TO REG-TAR
           MOVE REG-ORD-TIP_CLASE TO REG-TAR-TIP_CLASE.
           MOVE ZEROES TO REG-TAR-VIG-DES.
           START TARIFAS KEY IS > REG-TAR-CLAVE.

       LEO-TARIFA.
           READ TARIFAS NEXT RECORD INTO REG-TAR.

       ASIGNO-TARIFA-ACT.
           MOVE REG-TAR TO WS-TAR-ACTUAL.
           PERFORM LEO-TARIFA.

       FIN-SALIDA.
           CLOSE PROFESORES.
           CLOSE TARIFAS.

       END PROGRAM TP2.
