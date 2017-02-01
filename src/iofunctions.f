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
C******************************************************************************
C******************************************************************************
C Funciones de entrada/salida por teclado             (c) ncl@astrax.fis.ucm.es
C******************************************************************************
C******************************************************************************
C
        CHARACTER*(*) FUNCTION READC(IDLOG,CQUESTION,CDEF,CVAL)
        IMPLICIT NONE
        INTEGER IDLOG
        CHARACTER*(*) CQUESTION
        CHARACTER*(*) CDEF
        CHARACTER*(*) CVAL
C
        INTEGER INDEXR
C
        INTEGER I,L1,L2,LL1,LL2
        INTEGER TRUEBEG,TRUELEN
        INTEGER NERR
        CHARACTER*255 CADENA
        LOGICAL LECHO
        LOGICAL LOOP
C
        COMMON/BLKLECHO/LECHO
C------------------------------------------------------------------------------
        L1=1 !evita un warning de compilacion
        NERR=0
10      L2=TRUELEN(CQUESTION)
        IF(L2.NE.0) WRITE(*,100) CQUESTION(1:L2)
        IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100) ' ['
            WRITE(*,100) CDEF(L1:L2)
            WRITE(*,100) '] ? '
          END IF
        ELSE
          WRITE(*,100) '? '
        END IF
        IF(IDLOG.EQ.0)THEN
          READ(*,101,ERR=20) CADENA
          IF(LECHO) WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        ELSE
          LOOP=.TRUE.
          DO WHILE(LOOP)
            READ(IDLOG,101,ERR=20) CADENA
            IF(CADENA(1:1).NE.'#') LOOP=.FALSE. !saltamos comentario en col. 1
          END DO
          LL2=INDEXR(CADENA,'#') !truncamos comentario no en primera columna
          IF(LL2.GT.1)THEN
            CADENA=CADENA(1:LL2-1)
          END IF
          LL1=TRUEBEG(CADENA) !eliminamos espacios por delante
          LL2=TRUELEN(CADENA) !eliminamos espacios por detras
          CADENA=CADENA(LL1:LL2)
          WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        END IF
        IF(CVAL.EQ.'@')THEN
          IF(TRUELEN(CADENA).EQ.0)THEN
            IF(CDEF.EQ.'@')THEN
              GOTO 10
            END IF
            CADENA=CDEF(L1:L2)
          END IF
        ELSE
          IF(TRUELEN(CADENA).EQ.0)THEN
            IF(CDEF.EQ.'@')THEN
              GOTO 10
            END IF
            CADENA=CDEF(L1:L2)
          ELSE
            DO I=1,TRUELEN(CADENA)
              IF(INDEX(CVAL,CADENA(I:I)).EQ.0)THEN
                WRITE(*,101) '**ERROR** Press <CR> to continue...'
                IF(IDLOG.NE.0)THEN
                  STOP
                END IF
                READ(*,*)
ccc             IF(CDEF.EQ.'@') WRITE(*,100)'? '
                NERR=NERR+1
                IF(NERR.GT.10) 
     +           STOP 'FATAL ERROR: too many errors in READC.'
                GOTO 10
              END IF
            END DO
          END IF
        END IF
        READC=CADENA
        RETURN
20      CONTINUE
        WRITE(*,101) '**ERROR** Press <CR> to continue...'
        IF(IDLOG.NE.0)THEN
          STOP
        END IF
        READ(*,*)
ccc     IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors in READC.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        INTEGER FUNCTION READI(IDLOG,CQUESTION,CDEF)
        IMPLICIT NONE
        INTEGER IDLOG
        CHARACTER*(*) CQUESTION
        CHARACTER*(*) CDEF
C
        INTEGER I,L1,L2
        INTEGER LCOMMENT
        INTEGER N
        INTEGER NERR
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CADENA
        LOGICAL LECHO
        COMMON/BLKLECHO/LECHO
C------------------------------------------------------------------------------
        NERR=0
10      L2=TRUELEN(CQUESTION)
        IF(L2.NE.0) WRITE(*,100) CQUESTION(1:L2)
        IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100) ' ['
            WRITE(*,100) CDEF(L1:L2)
            WRITE(*,100) '] ? '
          END IF
        ELSE
          WRITE(*,100) '? '
        END IF
        IF(IDLOG.EQ.0)THEN
          READ(*,101,ERR=20) CADENA
          IF(LECHO) WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(IDLOG,101,ERR=20) CADENA
          LCOMMENT=INDEX(CADENA,'#')
          IF(LCOMMENT.NE.0) CADENA=CADENA(1:LCOMMENT-1) !elimina el comentario
          WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        END IF
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) N
        READI=N
        RETURN
20      CONTINUE
        WRITE(*,101) '**ERROR** Press <CR> to continue...'
        IF(IDLOG.NE.0)THEN
          STOP
        END IF
        READ(*,*)
ccc     IF(CDEF.EQ.'@') WRITE(*,100) '? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors in READI.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        INTEGER FUNCTION READILIM(IDLOG,CQUESTION,CDEF,N1,N2)
        IMPLICIT NONE
        INTEGER IDLOG
        CHARACTER*(*) CQUESTION
        CHARACTER*(*) CDEF
        INTEGER N1,N2
C
        INTEGER I,L1,L2
        INTEGER LCOMMENT
        INTEGER N
        INTEGER NERR
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CDUMMY
        CHARACTER*255 CADENA
        LOGICAL LECHO
        COMMON/BLKLECHO/LECHO
C------------------------------------------------------------------------------
        IF(N2.LT.N1) STOP 'ERROR: N2.LT.N1 in function: READILIM'
        NERR=0
10      L2=TRUELEN(CQUESTION)
        IF(L2.NE.0) WRITE(*,100) CQUESTION(1:L2)
        WRITE(CDUMMY,'(A1,I10,A5,I10,A1)') '(',N1,',...,',N2,')'
        CALL RMBLANK(CDUMMY,CDUMMY,L2)
        WRITE(*,100) ' '//CDUMMY(1:L2)
        IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100) ' ['
            WRITE(*,100) CDEF(L1:L2)
            WRITE(*,100) '] ? '
          END IF
        ELSE
          WRITE(*,100) '? '
        END IF
        IF(IDLOG.EQ.0)THEN
          READ(*,101,ERR=20) CADENA
          IF(LECHO) WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(IDLOG,101,ERR=20) CADENA
          LCOMMENT=INDEX(CADENA,'#')
          IF(LCOMMENT.NE.0) CADENA=CADENA(1:LCOMMENT-1) !elimina el comentario
          WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        END IF
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) N
        READILIM=N
C
        IF((N.LT.N1).OR.(N.GT.N2)) GOTO 20
        RETURN
C------------------------------------------------------------------------------
20      CONTINUE
        WRITE(*,101) '**ERROR** Press <CR> to continue...'
        IF(IDLOG.NE.0)THEN
          STOP
        END IF
        READ(*,*)
C
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors in READILIM.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        REAL FUNCTION READF(IDLOG,CQUESTION,CDEF)
        IMPLICIT NONE
        INTEGER IDLOG
        CHARACTER*(*) CQUESTION
        CHARACTER*(*) CDEF
C
        INTEGER I,L1,L2
        INTEGER LCOMMENT
        INTEGER NERR
        REAL F
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*1 C
        CHARACTER*255 CADENA
        LOGICAL LECHO
        COMMON/BLKLECHO/LECHO
C------------------------------------------------------------------------------
        NERR=0
10      L2=TRUELEN(CQUESTION)
        IF(L2.NE.0) WRITE(*,100) CQUESTION(1:L2)
        IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100) ' ['
            WRITE(*,100) CDEF(L1:L2)
            WRITE(*,100) '] ? '
          END IF
        ELSE
          WRITE(*,100) '? '
        END IF
        IF(IDLOG.EQ.0)THEN
          READ(*,101,ERR=20) CADENA
          IF(LECHO) WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        ELSE
          READ(IDLOG,101,ERR=20) CADENA
          LCOMMENT=INDEX(CADENA,'#')
          IF(LCOMMENT.GT.0) CADENA=CADENA(1:LCOMMENT-1) !elimina el comentario
          WRITE(*,101) CADENA(1:TRUELEN(CADENA))
        END IF
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcfghijklmnoprstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCFGHIJKLMNOPRSTUVWXYZ/',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) F
        READF=F
        RETURN
20      CONTINUE
        WRITE(*,101) '**ERROR** Press <CR> to continue...'
        IF(IDLOG.NE.0)THEN
          STOP
        END IF
        READ(*,*)
ccc     IF(CDEF.EQ.'@') WRITE(*,100)'? '
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors in READF.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        SUBROUTINE RMBLANK(C1,C2,L)
        IMPLICIT NONE
        INTEGER L
        CHARACTER*(*) C1,C2
C
        INTEGER I,K,L0
C------------------------------------------------------------------------------
        K=0
        L0=LEN(C1)
        DO I=1,L0
          IF(C1(I:I).NE.CHAR(32))THEN
            K=K+1
            C2(K:K)=C1(I:I)
          END IF
        END DO
        L=K
        L0=LEN(C2)
        IF(L.LT.L0)THEN
          DO I=L+1,L0
            C2(I:I)=' '
          END DO
        END IF
        END
C
C******************************************************************************
C
        INTEGER FUNCTION TRUELEN(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        TRUELEN=0
        L=LEN(CADENA)
C
        IF(L.GT.0)THEN
          DO I=L,1,-1
            IF(ICHAR(CADENA(I:I)).GT.32)THEN
              TRUELEN=I
              RETURN
            END IF
          END DO
        END IF
        END
C
C******************************************************************************
C
        INTEGER FUNCTION TRUEBEG(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        TRUEBEG=0
        L=LEN(CADENA)
C
        IF(L.GT.0)THEN
          DO I=1,L
            IF(ICHAR(CADENA(I:I)).GT.32)THEN
              TRUEBEG=I
              RETURN
            END IF
          END DO
        END IF
        END
