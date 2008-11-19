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
        INTEGER FUNCTION FINDINDEX(CNAME,NINDICES)
        IMPLICIT NONE
        CHARACTER*(*) CNAME
        INTEGER NINDICES
C
        INCLUDE 'dimensions.inc'
C
        INTEGER TRUEBEG,TRUELEN
C
        INTEGER I,IFOUND
        INTEGER L1,L2,LL1,LL2
        CHARACTER*255 CINDEX(NMAX_INDICES)
C
        COMMON/BLKCINDEX/CINDEX
C------------------------------------------------------------------------------
        IFOUND=0
        L1=TRUEBEG(CNAME)
        L2=TRUELEN(CNAME)
        DO I=1,NINDICES
          LL1=TRUEBEG(CINDEX(I))
          LL2=TRUELEN(CINDEX(I))
          IF((L2-L1).EQ.(LL2-LL1))THEN
            IF(CINDEX(I)(LL1:LL2).EQ.CNAME(L1:L2))THEN
              IFOUND=I
            END IF
          END IF
        END DO
C
        FINDINDEX=IFOUND
        END
