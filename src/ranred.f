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
C REAL FUNCTION RANRED(NSEED)
C
C Input: NSEED
C Output: RANRED (function)
C
C Return a random number in the range [0,1) using the intrinsic fortran
C function RAND(). If NSEED<0 a previous call to SRAND(TIME()) is also 
C performed.
C
C INTEGER NSEED -> NSEED=0: RANRED returns the next random number in the
C                           sequence.
C                  NSEED<0: RANRED performs a previous call to the
C                           intrinsic fortran function SRAND(TIME()), and
C                           generates a random number in the new sequence.
C                           In this case NSEED returns 0.
C                  NSEED>0: RANRED performs a previous call to the
C                           intrinsic fortran function SRAND(NSEED), and
C                           generates a random number in the new sequence.
C                           In this case NSEED returns 0.
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION RANRED(NSEED)
        IMPLICIT NONE
        INTEGER NSEED
C------------------------------------------------------------------------------
        IF(NSEED.EQ.0)THEN
          RANRED=RAND()
          RETURN
        ELSEIF(NSEED.LT.0)THEN
          CALL SRAND(TIME())
        ELSEIF(NSEED.GT.0)THEN
          CALL SRAND(NSEED)
        END IF
        NSEED=0
        RANRED=RAND()
C
        END
