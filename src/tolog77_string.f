C------------------------------------------------------------------------------
C Version 22-October-2008                                File: tolog77_string.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
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
