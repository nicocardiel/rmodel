C------------------------------------------------------------------------------
C Version 22-October-2008                                        File: rspace.f
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
