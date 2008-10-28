C------------------------------------------------------------------------------
C Version 22-October-2008                                        File: indexr.f
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
C Busca la posicion en la que aparece en caracter CH en la cadena
C CSTRING, empezando la busqueda por el final
        INTEGER FUNCTION INDEXR(CSTRING,CH)
        IMPLICIT NONE
        CHARACTER*(*) CSTRING
        CHARACTER*1 CH
C
        INTEGER TRUELEN
C
        INTEGER I,L
C------------------------------------------------------------------------------
        INDEXR=0 !salvo que se demuestre lo contrario
C
        L=TRUELEN(CSTRING)
        IF(L.EQ.0) RETURN
C
        DO I=L,1,-1
          IF(CSTRING(I:I).EQ.CH)THEN
            INDEXR=I
            RETURN
          END IF
        END DO
        END
