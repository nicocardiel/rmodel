C------------------------------------------------------------------------------
C Version 23-October-2008                                 File: set_my_colors.f
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
C This subroutine sets the colors as specified in the file mycolors.dat
        SUBROUTINE SET_MY_COLORS
        IMPLICIT NONE
C
        INCLUDE 'rmodel.inc'
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER CI
        INTEGER L1,L2
        REAL CR,CG,CB
        CHARACTER*255 RMODEL_DIR
        CHARACTER*255 CLINE
        LOGICAL LOGFILE
C------------------------------------------------------------------------------
        RMODEL_DIR=RMODEL_DIR_
        L1=TRUEBEG(RMODEL_DIR)
        L2=TRUELEN(RMODEL_DIR)
        INQUIRE(FILE=RMODEL_DIR(L1:L2)//'/mycolors.dat',EXIST=LOGFILE)
        IF(LOGFILE)THEN
          OPEN(47,FILE=RMODEL_DIR(L1:L2)//'/mycolors.dat',STATUS='OLD',
     +     FORM='FORMATTED')
10        READ(47,101,END=20) CLINE
          IF(CLINE(1:1).EQ.'#') GOTO 10 !ignora comentario en primera columna
          READ(CLINE,*,ERR=90) CI,CR,CG,CB
          CALL PGSCR(CI,CR,CG,CB)
          GOTO 10
20        CLOSE(47)
        ELSE
          WRITE(*,*)
          WRITE(*,101) '********************************************'
          WRITE(*,101) 'WARGNING: the following file does not exist!'
          WRITE(*,101) RMODEL_DIR(L1:L2)//'/mycolors.dat'
          WRITE(*,101) '********************************************'
          WRITE(*,*)
        END IF
        RETURN
C
90      WRITE(*,101) 'FATAL ERROR while reading the file:'
        WRITE(*,101) RMODEL_DIR(L1:L2)//'/mycolors.dat'
        CLOSE(47)
        STOP
C
101     FORMAT(A)
        END
