C------------------------------------------------------------------------------
C Version 22-October-2008                                     File: findindex.f
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
