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
C concatena C1 y C2, y las devuelve en COUT, sustituyendo los espacios por "_"
        SUBROUTINE RSPACE(C1,C2,COUT)
        IMPLICIT NONE
        CHARACTER*(*) C1
        CHARACTER*(*) C2
        CHARACTER*(*) COUT
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER I,L
        INTEGER L1,L2,LL1,LL2
C------------------------------------------------------------------------------
        L1=TRUEBEG(C1)
        L2=TRUELEN(C1)
        LL1=TRUEBEG(C2)
        LL2=TRUELEN(C2)
        COUT=C1(L1:L2)//' '//C2(LL1:LL2)
        L=TRUELEN(COUT)
        DO I=1,L
          IF(COUT(I:I).EQ.' ') COUT(I:I)="_"
        END DO
        END
