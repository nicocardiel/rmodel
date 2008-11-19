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
C Funcion para escribir en el log-file una cadena de caracteres
C
        SUBROUTINE TOLOG77_STRING(CSTRING,CTEXT)
        IMPLICIT NONE
        CHARACTER*(*) CSTRING
        CHARACTER*(*) CTEXT
C
        INTEGER TRUELEN,TRUEBEG
C
        INTEGER L1,L2,DL
        INTEGER LL1,LL2
        CHARACTER*12 CBLANK
C------------------------------------------------------------------------------
        CBLANK='            '
        L1=TRUEBEG(CSTRING)
        L2=TRUELEN(CSTRING)
        DL=L2-L1+1
        LL1=TRUEBEG(CTEXT)
        LL2=TRUELEN(CTEXT)
        IF(DL.LT.12)THEN
          WRITE(77,100) CSTRING(L1:L2)
          WRITE(77,100) CBLANK(1:12-DL)
          WRITE(77,100) ' # '
          WRITE(77,101) CTEXT(LL1:LL2)
        ELSE
          WRITE(77,100) CSTRING(L1:L2)
          WRITE(77,100) ' # '
          WRITE(77,101) CTEXT(LL1:LL2)
        END IF
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
