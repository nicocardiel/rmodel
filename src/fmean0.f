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
C REAL FUNCTION FMEAN0(N,X,SIGMA)
C
C Input: N,X
C Output: FMEAN0 (function),SIGMA
C
C Calculate the mean value of X(N) and its r.m.s.
C
C INTEGER N -> no. of elements
C REAL    X(N) -> input matrix
C REAL SIGMA -> r.m.s. around the mean value
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION FMEAN0(N,X,SIGMA)
        IMPLICIT NONE
        INTEGER N
        REAL X(N)
        REAL SIGMA
C
        INTEGER I
        REAL SUM
C------------------------------------------------------------------------------
        IF(N.LE.0) STOP 'FATAL ERROR: in function FMEAN0: N.LE.0'
        SUM=0.
        DO I=1,N
          SUM=SUM+X(I)
        END DO
        FMEAN0=SUM/REAL(N)
C
        IF(N.EQ.1)THEN
          SIGMA=0.
        ELSE
          SUM=0.
          DO I=1,N
            SUM=SUM+(X(I)-FMEAN0)*(X(I)-FMEAN0)
          END DO
          SIGMA=SQRT(SUM/REAL(N-1))
        END IF
C
        END
