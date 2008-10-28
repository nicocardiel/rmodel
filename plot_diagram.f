C------------------------------------------------------------------------------
C Version 22-October-2008                                  File: plot_diagram.f
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
        SUBROUTINE PLOT_DIAGRAM(IDLOG)
        IMPLICIT NONE
        INTEGER IDLOG
C
        INCLUDE 'dimensions.inc'
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        REAL READF
C
        INTEGER IPAR1,IPAR2
        INTEGER LEN_PARAMETER(NMAX_PARAMETERS)
        INTEGER NPARAM(2)
        INTEGER NINDEX(2)
        INTEGER NP
        INTEGER L1,L2,LL1,LL2
        REAL XMIN,XMAX,YMIN,YMAX,DX,DY
        REAL ARRAY1(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL ARRAY2(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL XP(NMAX_LEN_PARAMETER),YP(NMAX_LEN_PARAMETER)
        REAL XPARAMETER(NMAX_LEN_PARAMETER,NMAX_PARAMETERS)
        REAL OLD_CH
        REAL XSEP,YSEP
        CHARACTER*10 CUNITS(2)
        CHARACTER*50 CDUMMY
        CHARACTER*255 CMODELNAME
        CHARACTER*255 CINDEX(NMAX_INDICES)
        CHARACTER*255 CLABEL(2),CXLABEL,CYLABEL
        LOGICAL LREV(2)
        LOGICAL LABPAR(2)
        LOGICAL LIAUTO,LIMIASK
C
        COMMON/BLKCMODELNAME/CMODELNAME
        COMMON/BLKARRAYS/ARRAY1,ARRAY2
        COMMON/BLKNPARAM/NPARAM
        COMMON/BLKNINDEX/NINDEX
        COMMON/BLKLEN_PARAMETER/LEN_PARAMETER
        COMMON/BLKCINDEX/CINDEX
        COMMON/BLKXPARAMETER/XPARAMETER
        COMMON/BLKCUNITS/CUNITS
        COMMON/BLKCLABEL/CLABEL
        COMMON/BLKLREV/LREV
        COMMON/BLKLABPAR/LABPAR
        COMMON/BLKLIAUTO/LIAUTO,LIMIASK
        COMMON/BLKXYMINMAX/XMIN,XMAX,YMIN,YMAX
C------------------------------------------------------------------------------
C Calculamos limites del diagrama
        IF(.NOT.LIAUTO)THEN
          IF(.NOT.LIMIASK)THEN
            DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
              DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
                IF((IPAR1.EQ.1).AND.(IPAR2.EQ.1))THEN
                  XMIN=ARRAY1(IPAR1,IPAR2)
                  XMAX=XMIN
                  YMIN=ARRAY2(IPAR1,IPAR2)
                  YMAX=YMIN
                ELSE
                  XMIN=AMIN1(XMIN,ARRAY1(IPAR1,IPAR2))
                  XMAX=AMAX1(XMAX,ARRAY1(IPAR1,IPAR2))
                  YMIN=AMIN1(YMIN,ARRAY2(IPAR1,IPAR2))
                  YMAX=AMAX1(YMAX,ARRAY2(IPAR1,IPAR2))
                END IF
              END DO
            END DO
            DX=XMAX-XMIN
            IF(DX.EQ.0.0) DX=1.0
            XMIN=XMIN-DX/15.0
            XMAX=XMAX+DX/7.0
            DY=YMAX-YMIN
            IF(DY.EQ.0.0) DY=1.0
            YMIN=YMIN-DY/10.0
            YMAX=YMAX+DY/10.0
          ELSE
            WRITE(CDUMMY,*) XMIN
            XMIN=READF(IDLOG,'Xmin',CDUMMY)
            WRITE(77,*) XMIN,'# Xmin'
            WRITE(CDUMMY,*) XMAX
            XMAX=READF(IDLOG,'Xmax',CDUMMY)
            WRITE(77,*) XMAX,'# Xmax'
            WRITE(CDUMMY,*) YMIN
            YMIN=READF(IDLOG,'Ymin',CDUMMY)
            WRITE(77,*) YMIN,'# Ymin'
            WRITE(CDUMMY,*) YMAX
            YMAX=READF(IDLOG,'Ymax',CDUMMY)
            WRITE(77,*) YMAX,'# Ymax'
          END IF
        END IF
C------------------------------------------------------------------------------
C Dibujamos diagrama
        CALL PGENV(0.,1.,0.,1.,1,-2)
        IF(LREV(1).AND.LREV(2))THEN
          CALL PGSWIN(XMAX,XMIN,YMAX,YMIN)
        ELSEIF(LREV(1))THEN
          CALL PGSWIN(XMAX,XMIN,YMIN,YMAX)
        ELSEIF(LREV(2))THEN
          CALL PGSWIN(XMIN,XMAX,YMAX,YMIN)
        ELSE
          CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
        END IF
        CALL PGBOX('BCNTS',0.0,0,'BCNTS',0.0,0)
        L1=TRUEBEG(CLABEL(1))
        L2=TRUELEN(CLABEL(1))
        LL1=TRUEBEG(CUNITS(1))
        LL2=TRUELEN(CUNITS(1))
        CXLABEL=CLABEL(1)(L1:L2)//' ['//
     +   CUNITS(1)(LL1:LL2)//']'
        L1=TRUEBEG(CLABEL(2))
        L2=TRUELEN(CLABEL(2))
        LL1=TRUEBEG(CUNITS(2))
        LL2=TRUELEN(CUNITS(2))
        CYLABEL=CLABEL(2)(L1:L2)//' ['//
     +   CUNITS(2)(LL1:LL2)//']'
        CALL PGLABEL(CXLABEL,CYLABEL,CMODELNAME)
C Lineas de parametro 1 constante
        DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
          DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
            XP(IPAR2)=ARRAY1(IPAR1,IPAR2)
            YP(IPAR2)=ARRAY2(IPAR1,IPAR2)
          END DO
          NP=LEN_PARAMETER(NPARAM(2))
          CALL PGSLS(2)
          CALL PGSCI(14)
          CALL PGLINE(NP,XP,YP)
          CALL PGPOINT(NP,XP,YP,17)
          CALL PGSCI(1)
          CALL PGSLS(1)
          IF(LABPAR(1))THEN
            IF((XP(NP).GE.XMIN).AND.(XP(NP).LE.XMAX).AND.
     +         (YP(NP).GE.YMIN).AND.(YP(NP).LE.YMAX))THEN
              CALL PGQCH(OLD_CH)
              CALL PGSCH(0.8)
              WRITE(CDUMMY,'(F5.2)') XPARAMETER(IPAR1,NPARAM(1))
              L1=TRUEBEG(CDUMMY)
              L2=TRUELEN(CDUMMY)
              CALL PGSCI(2)
              CALL XYSEPAR(XMIN,XMAX,YMIN,YMAX,
     +         XP(NP-1),YP(NP-1),XP(NP),YP(NP),
     +         XSEP,YSEP)
              CALL PGPTXT(XP(NP)+XSEP,YP(NP)+YSEP,0.0,0.5,CDUMMY(L1:L2))
              CALL PGSCI(1)
              CALL PGSCH(OLD_CH)
            END IF
          END IF
        END DO
C Lineas de parametro 2 constante
        DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
          DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
            XP(IPAR1)=ARRAY1(IPAR1,IPAR2)
            YP(IPAR1)=ARRAY2(IPAR1,IPAR2)
          END DO
          NP=LEN_PARAMETER(NPARAM(1))
          CALL PGSLS(4)
          CALL PGSCI(14)
          CALL PGLINE(NP,XP,YP)
          CALL PGSCI(1)
          CALL PGSLS(1)
          IF(LABPAR(2))THEN
            IF((XP(NP).GE.XMIN).AND.(XP(NP).LE.XMAX).AND.
     +         (YP(NP).GE.YMIN).AND.(YP(NP).LE.YMAX))THEN
              CALL PGQCH(OLD_CH)
              CALL PGSCH(0.8)
              WRITE(CDUMMY,'(F5.2)') XPARAMETER(IPAR2,NPARAM(2))
              L1=TRUEBEG(CDUMMY)
              L2=TRUELEN(CDUMMY)
              CALL PGSCI(3)
              CALL XYSEPAR(XMIN,XMAX,YMIN,YMAX,
     +         XP(NP-1),YP(NP-1),XP(NP),YP(NP),
     +         XSEP,YSEP)
              CALL PGPTXT(XP(NP)+XSEP,YP(NP)+YSEP,0.0,0.5,CDUMMY(L1:L2))
              CALL PGSCI(1)
              CALL PGSCH(OLD_CH)
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
        END
C
C******************************************************************************
C
        SUBROUTINE XYSEPAR(XMIN,XMAX,YMIN,YMAX,X1,Y1,X2,Y2,XSEP,YSEP)
        IMPLICIT NONE
        REAL XMIN,XMAX,YMIN,YMAX
        REAL X1,Y1,X2,Y2
        REAL XSEP,YSEP
C
        REAL XV1,XV2,YV1,YV2
        REAL XX1,XX2,YY1,YY2
        REAL DIST
        REAL NEWXX2,NEWYY2
        REAL XXX2,YYY2
C------------------------------------------------------------------------------
        CALL PGQVP(1,XV1,XV2,YV1,YV2)
        XX1=XV1+(X1-XMIN)/(XMAX-XMIN)*(XV2-XV1)
        XX2=XV1+(X2-XMIN)/(XMAX-XMIN)*(XV2-XV1)
        YY1=YV1+(Y1-YMIN)/(YMAX-YMIN)*(YV2-YV1)
        YY2=YV1+(Y2-YMIN)/(YMAX-YMIN)*(YV2-YV1)
        DIST=SQRT((XX2-XX1)*(XX2-XX1)+(YY2-YY1)*(YY2-YY1))
        NEWXX2=XX2+(XX2-XX1)*0.05*(XV2-XV1)/DIST
        NEWYY2=YY2+(YY2-YY1)*0.05*(YV2-YV1)/DIST
        XXX2=XMIN+(NEWXX2-XV1)/(XV2-XV1)*(XMAX-XMIN)
        YYY2=YMIN+(NEWYY2-YV1)/(YV2-YV1)*(YMAX-YMIN)
        XSEP=XXX2-X2
        YSEP=YYY2-Y2
C------------------------------------------------------------------------------
        END
