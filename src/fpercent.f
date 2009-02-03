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
Comment
C
C REAL FUNCTION FPERCENT(N,X,PERCENTILE)
C
C Input: N,X,PERCENTILE
C Output: FPERCENT (function)
C
C Calculate a fixed percentile of X(N)
C
C INTEGER N -> no. of elements
C REAL    X(N) -> input matrix
C REAL    PERCENTILE -> percentile to be computed
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION FPERCENT(N,X,PERCENTILE)
        IMPLICIT NONE
        INTEGER N
        REAL X(N),PERCENTILE
C
        INTEGER NMAX
        PARAMETER (NMAX=10000)
C
        INTEGER I,N1,N2
        INTEGER IFLAG
        REAL XSORTED(NMAX),XNUM(NMAX)
        REAL FRACTION
        REAL LININTERP
C------------------------------------------------------------------------------
        IF(N.GT.NMAX)THEN
          WRITE(*,101) 'FATAL ERROR in subroutine FPERCENT:'
          WRITE(*,101) 'N.GT.NMAX.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          FPERCENT=0.
          RETURN
        END IF
C
        IF(N.EQ.1)THEN
          FPERCENT=X(1)
          RETURN
        END IF
C
        DO I=1,N
          XNUM(I)=REAL(I)
          XSORTED(I)=X(I)
        END DO
C
        IF((PERCENTILE.LT.0.0).OR.(PERCENTILE.GT.100.0))THEN
          WRITE(*,101) 'FATAL ERROR in subroutine FPERCENT:'
          WRITE(*,100) 'PERCENTILE= '
          WRITE(*,*) PERCENTILE
          WRITE(*,101) 'PERCENTILE out of range.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          FPERCENT=0.
          RETURN
        END IF
C
        CALL ORDENA1F(N,XSORTED)
C
        FRACTION=1.+REAL(N-1)*PERCENTILE/100.
        FPERCENT=LININTERP(N,XNUM,XSORTED,FRACTION,IFLAG,N1,N2)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
