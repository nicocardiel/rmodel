C------------------------------------------------------------------------------
C Version 22-October-2008                         File: include_external_data.f
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
C IMODE=0: pide nombre de fichero y carga nuevos datos
C IMODE=1: utiliza el fichero anterior
        SUBROUTINE INCLUDE_EXTERNAL_DATA(IDLOG,IMODE)
        IMPLICIT NONE
        INTEGER IDLOG
        INTEGER IMODE
        INCLUDE 'dimensions.inc'
C
        INTEGER TRUEBEG,TRUELEN
        CHARACTER*255 READC
        CHARACTER*255 CEXTRAE
C
        INTEGER K
        INTEGER L1,L2
        INTEGER OLD_CI
        INTEGER ISTATUS
        INTEGER ISYMBOL,ICOLOR
        INTEGER NOBJPLOT
        REAL OLD_CH
        REAL XOBJ(NMAX_OBJECTS),EXOBJ(NMAX_OBJECTS)
        REAL YOBJ(NMAX_OBJECTS),EYOBJ(NMAX_OBJECTS)
        REAL FSIZE1,FSIZE2
        REAL OFFX,OFFY,ANGLE,FJUST
        CHARACTER*255 CDUMMY
        CHARACTER*255 CLINEA,CBACKUP
        CHARACTER*255 OBJNAME(NMAX_OBJECTS)
        CHARACTER*255 OBJ_INFILE
        LOGICAL LOGFILE
C
        COMMON/BLKOBJPLOT1/NOBJPLOT
        COMMON/BLKOBJPLOT2/XOBJ,EXOBJ,YOBJ,EYOBJ
        COMMON/BLKOBJPLOT3/OBJNAME
        COMMON/BLKOBJPLOT4/OBJ_INFILE
C------------------------------------------------------------------------------
        IF(IMODE.EQ.0)THEN
          LOGFILE=.FALSE.
          DO WHILE(.NOT.LOGFILE)
            WRITE(*,101) 'The external ASCII file name must contain'//
     +       ' at least the first 4 columns:'
            WRITE(*,101) 'column #01: X-axis index'
            WRITE(*,101) 'column #02: error in previous number'
            WRITE(*,101) 'column #03: Y-axis index'
            WRITE(*,101) 'column #04: error in previous number'
            WRITE(*,101) 'column #05: PGPLOT symbol code'
            WRITE(*,101) 'column #06: PGPLOT color code'
            WRITE(*,101) 'column #07: PGPLOT font size for symbol'
            WRITE(*,101) 'column #08: X-axis offset for label'
            WRITE(*,101) 'column #09: Y-axis offset for label'
            WRITE(*,101) 'column #10: ANGLE for label'
            WRITE(*,101) 'column #11: FJUST for label'
            WRITE(*,101) 'column #12: PGPLOT font size for label'
            WRITE(*,101) 'column #13: object label'//
     +       ' (without white spaces)'
            OBJ_INFILE=READC(IDLOG,'File name (NONE=ignore)',
     +       'NONE','@')
            L1=TRUEBEG(OBJ_INFILE)
            L2=TRUELEN(OBJ_INFILE)
            IF(OBJ_INFILE(L1:L2).EQ.'NONE')THEN
              CALL TOLOG77_STRING(OBJ_INFILE(L1:L2),
     +         'external ASCII file with data')
              RETURN
            END IF
            INQUIRE(FILE=OBJ_INFILE(L1:L2),EXIST=LOGFILE)
            IF(.NOT.LOGFILE)THEN
              WRITE(*,101) 'ERROR: this file does not exist. Try again.'
            END IF
          END DO
          CALL TOLOG77_STRING(OBJ_INFILE(L1:L2),
     +     'external ASCII file with data')
        END IF
C------------------------------------------------------------------------------
        K=0
        CALL PGQCH(OLD_CH)
        CALL PGQCI(OLD_CI)
        L1=TRUEBEG(OBJ_INFILE)
        L2=TRUELEN(OBJ_INFILE)
        OPEN(22,FILE=OBJ_INFILE(L1:L2),STATUS='OLD',FORM='FORMATTED')
10      READ(22,101,END=20) CLINEA
        K=K+1
        !valores por defecto
        ISYMBOL=17
        ICOLOR=5
        FSIZE1=2.0
        FSIZE2=1.0
        OFFX=0.0
        OFFY=0.0
        OBJNAME(K)=''
        CDUMMY=CEXTRAE(CLINEA,1,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) XOBJ(K)
        CDUMMY=CEXTRAE(CLINEA,2,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) EXOBJ(K)
        CDUMMY=CEXTRAE(CLINEA,3,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) YOBJ(K)
        CDUMMY=CEXTRAE(CLINEA,4,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) EYOBJ(K)
        CDUMMY=CEXTRAE(CLINEA,5,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) ISYMBOL
        CDUMMY=CEXTRAE(CLINEA,6,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) ICOLOR
        CDUMMY=CEXTRAE(CLINEA,7,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) FSIZE1
        CDUMMY=CEXTRAE(CLINEA,8,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) OFFX
        CDUMMY=CEXTRAE(CLINEA,9,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) OFFY
        CDUMMY=CEXTRAE(CLINEA,10,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) ANGLE
        CDUMMY=CEXTRAE(CLINEA,11,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) FJUST
        CDUMMY=CEXTRAE(CLINEA,12,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) READ(CDUMMY,*) FSIZE2
        CDUMMY=CEXTRAE(CLINEA,13,ISTATUS,CBACKUP,.FALSE.)
        IF(ISTATUS.NE.0) OBJNAME(K)=CDUMMY
        CALL PGSCH(FSIZE1)
        CALL PGSCI(ICOLOR)
        CALL PGERR1(1,XOBJ(K),YOBJ(K),EXOBJ(K),1.0)
        CALL PGERR1(2,XOBJ(K),YOBJ(K),EYOBJ(K),1.0)
        CALL PGERR1(3,XOBJ(K),YOBJ(K),EXOBJ(K),1.0)
        CALL PGERR1(4,XOBJ(K),YOBJ(K),EYOBJ(K),1.0)
        CALL PGPOINT(1,XOBJ(K),YOBJ(K),ISYMBOL)
        L2=TRUELEN(OBJNAME(K))
        IF(L2.GT.0)THEN
          L1=TRUEBEG(OBJNAME(K))
          CALL PGSCH(FSIZE2)
          CALL PGPTXT(XOBJ(K)+OFFX,YOBJ(K)+OFFY,
     +     ANGLE,FJUST,OBJNAME(K)(L1:L2))
        END IF
        GOTO 10
20      CLOSE(22)
        CALL PGSCH(OLD_CH)
        CALL PGSCI(OLD_CI)
        NOBJPLOT=K
C------------------------------------------------------------------------------
101     FORMAT(A)
        END
