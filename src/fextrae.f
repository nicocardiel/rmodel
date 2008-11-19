C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
C
C This file is part of rmodel.
C 
C Rmodel is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C Rmodel is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with rmodel. If not, see <http://www.gnu.org/licenses/>.
C------------------------------------------------------------------------------
C Extrae de la cadena CLINEA, el numero ubicado en la columna N-esima. Esta
C funcion no destruye la informacion de la variable CLINEA. Si hay problemas
C al leer, ISTATUS retorna 0. Si no se puede leer el numero (por la presencia
C de caracteres no numericos, por ejemplo) ISTATUS retorna -1. La variable
C CBACKUP se utiliza como cadena intermedia, por lo que tiene que estar
C dimensionada en el programa principal con la misma dimension que CLINEA.
        REAL FUNCTION FEXTRAE(CLINEA,N,ISTATUS,CBACKUP)
        IMPLICIT NONE
        CHARACTER*(*) CLINEA
        INTEGER N
        INTEGER ISTATUS
        CHARACTER*(*) CBACKUP
C
        INTEGER TRUEBEG,TRUELEN
C
        INTEGER NCOL,NEXT
        INTEGER L1,L2
C------------------------------------------------------------------------------
        ISTATUS=0                          !salvo que se demuestre lo contrario
C caso trivial (linea vacia)
        IF(TRUELEN(CLINEA).EQ.0) GOTO 901
C
        CBACKUP=CLINEA!trabajamos con la cadena CBACKUP para no modificar CLINEA
C------------------------------------------------------------------------------
        NCOL=1            !almacenaremos en esta variable el numero de columnas
        L1=TRUEBEG(CBACKUP)                !primer elemento valido de la cadena
        L2=TRUELEN(CBACKUP)                !ultimo caracter valido de la cadena
C
10      NEXT=INDEX(CBACKUP(L1:L2),' ')             !siguiente espacio en blanco
        IF(NEXT.EQ.0)THEN                     !ya no hay mas espacios en blanco
          IF(N.EQ.NCOL)THEN
            READ(CBACKUP(L1:L2),*,ERR=902) FEXTRAE
            ISTATUS=1
            RETURN
          ELSE
            GOTO 901
          END IF
        END IF
        IF(N.EQ.NCOL)THEN
          READ(CBACKUP(L1:L1+NEXT-2),*,ERR=902) FEXTRAE
          ISTATUS=1
          RETURN
        END IF
        NCOL=NCOL+1                 !numero de la siguiente columna a encontrar
        L1=L1+NEXT
        L1=L1+TRUEBEG(CBACKUP(L1:L2))-1
        GOTO 10
C------------------------------------------------------------------------------
901     WRITE(*,*)
        WRITE(*,101) 'ERROR: unexpected end of row in FEXTRAE'
        WRITE(*,100) 'CLINEA: '
        WRITE(*,101) CLINEA(1:TRUELEN(CLINEA))
        WRITE(*,100) 'N.....: '
        WRITE(*,*) N
        RETURN
C..............................................................................
902     ISTATUS=-1
        RETURN
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
