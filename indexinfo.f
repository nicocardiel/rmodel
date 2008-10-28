C------------------------------------------------------------------------------
C Version 22-October-2008                                     File: indexinfo.f
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
C Para el indice denifido por la cadena CINDEX retorna parametros asociados
        SUBROUTINE INDEXINFO(IDLOG,CINDEX,
     +   CLABEL,ITI,FERR,DWC,CUNITS,LLOG,LREV)
        IMPLICIT NONE
C
        INTEGER IDLOG
        CHARACTER*(*) CINDEX
        CHARACTER*(*) CLABEL
        INTEGER ITI
        REAL FERR
        REAL DWC
        CHARACTER*(*) CUNITS
        LOGICAL LLOG
        LOGICAL LREV
C
        INCLUDE 'rmodel.inc'
C
        INTEGER NWVMAX
        PARAMETER (NWVMAX=396)
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        CHARACTER*255 READC
C
        INTEGER I,II
        INTEGER L1,L2
        INTEGER LD1,LD2
        INTEGER LI1,LI2
        INTEGER NCONTI,NABSOR
        REAL FWIDTH1,FWIDTH2
        REAL FXI1,FXI2
        REAL DWB,DWR
        REAL WB,WC,WR
        REAL WV(NWVMAX),FWV(NWVMAX/4)
        CHARACTER*1 CLLOG,CLREV
        CHARACTER*8 CNAME
        CHARACTER*60 CLDOS
        CHARACTER*255 RMODEL_DIR
C------------------------------------------------------------------------------
        RMODEL_DIR=RMODEL_DIR_
        LD1=TRUEBEG(RMODEL_DIR)
        LD2=TRUELEN(RMODEL_DIR)
C
        LI1=TRUEBEG(CINDEX)
        LI2=TRUELEN(CINDEX)
C------------------------------------------------------------------------------
C las etiquetas no cambian salvo en casos particulares
        CLABEL=CINDEX(LI1:LI2)
        IF(CINDEX(LI1:LI2).EQ.'Hbeta')THEN
          CLABEL='H\gb'
        ELSEIF(CINDEX(LI1:LI2).EQ.'<Fe>*')THEN
          CLABEL='<Fe>'//CHAR(39)
        ELSEIF(CINDEX(LI1:LI2).EQ.'[MgFe]*')THEN
          CLABEL='[MgFe]'//CHAR(39)
        END IF
C
        OPEN(33,FILE=RMODEL_DIR(LD1:LD2)//'/myindex.rmodel',
     +   STATUS='OLD',FORM='FORMATTED')
        READ(33,*,END=900)
        READ(33,*,END=900)
10      READ(33,'(A8,1X,I4,1X,A60)',END=12,ERR=900)
     +   CNAME,ITI,CLDOS
        IF(ITI.LE.100)THEN
          IF(ITI.EQ.99)THEN !es un color
            READ(33,*) FWIDTH1,FXI1
            FXI1=FXI1/10000.0
            READ(33,*) FWIDTH2,FXI2
            FXI2=FXI2/10000.0
            FERR=2.5*ALOG10(EXP(1.0))*SQRT(FXI1+FXI2)
            DWC=-999
            CUNITS='mag'
          ELSEIF(ITI.EQ.98)THEN !indice estrecho de Vazdekis & Arimoto 99
            READ(33,*) DWC,FERR
            CUNITS='\A'
          ELSEIF(ITI.EQ.97)THEN !<Fe>
            READ(33,*) DWC,FERR
            CUNITS='\A'
          ELSEIF(ITI.EQ.96)THEN !<Fe>'
            READ(33,*) DWC,FERR
            CUNITS='\A'
          ELSEIF(ITI.EQ.95)THEN ![MgFe]
            READ(33,*) DWC,FERR
            CUNITS='\A'
          ELSEIF(ITI.EQ.94)THEN ![MgFe]`
            READ(33,*) DWC,FERR
            CUNITS='\A'
          ELSEIF(ITI.EQ.12)THEN !D_CO
            NCONTI=(ITI/10)                     !numero de regiones de continuo
            NABSOR=ITI-NCONTI*10            !numero de regiones con absorciones
            DO I=1,NCONTI
              II=(2*I-1)
              READ(33,*,ERR=900) WV(II),WV(II+1)
            END DO
            DO I=1,NABSOR
              II=(2*NCONTI)+(2*I-1)
              READ(33,*,ERR=900) WV(II),WV(II+1)
            END DO
            FERR=-999
            DWC=1.0 !OJO: esto esta inventado
            CUNITS=' '
          ELSE !es un indice molecular, atomico o break
            READ(CLDOS,'(6(1X,F9.3))') WV(1),WV(2),WV(3),WV(4),
     +       WV(5),WV(6)
            IF((ITI.EQ.1).OR.(ITI.EQ.2))THEN    !indices moleculares o atomicos
              DWB=WV(2)-WV(1)
              DWC=WV(4)-WV(3)
              DWR=WV(6)-WV(5)
              WB=0.5*(WV(1)+WV(2))
              WC=0.5*(WV(3)+WV(4))
              WR=0.5*(WV(5)+WV(6))
              FERR=2.5*ALOG10(EXP(1.))*SQRT(1./DWC+
     +         (WR-WC)*(WR-WC)/((WR-WB)*(WR-WB)*DWB)+
     +         (WC-WB)*(WC-WB)/((WR-WB)*(WR-WB)*DWR))
              IF(ITI.EQ.1)THEN
                CUNITS='mag'
              ELSE
                CUNITS='\A'
              END IF
            ELSEIF((ITI.EQ.3).OR.(ITI.EQ.4))THEN         !indices D4000 o B4000
              FERR=2.5*ALOG10(EXP(1.))*0.1
              DWC=-999.
              CUNITS=' '
            ELSEIF(ITI.EQ.5)THEN !CO_KH
              FERR=-999
              DWC=-999.
              CUNITS=' '
            ELSE
              WRITE(*,101) 'FATAL ERROR: unexpecte ITI value'
              WRITE(*,100) 'ITI='
              WRITE(*,*) ITI
              CLOSE(33)
              STOP
            END IF
          END IF
        ELSEIF(ITI.GT.100)THEN                           !es un indice generico
          NCONTI=(ITI/100)                      !numero de regiones de continuo
          NABSOR=ITI-NCONTI*100             !numero de regiones con absorciones
          DO I=1,NCONTI
            II=(2*I-1)
            READ(33,*,ERR=900) WV(II),WV(II+1)
          END DO
          DO I=1,NABSOR
            II=(2*NCONTI)+(2*I-1)
            READ(33,*,ERR=900) WV(II),WV(II+1),FWV(I)
          END DO
          READ(33,*) DWC,FERR
          CUNITS='\A'
        ELSE
          WRITE(*,101) 'FATAL ERROR: invalid ITI number'
          WRITE(*,100) 'ITI='
          WRITE(*,*) ITI
          CLOSE(10)
          STOP
        END IF
C..............................................................................
        L1=TRUEBEG(CNAME)
        L2=TRUELEN(CNAME)
        IF(CNAME(L1:L2).EQ.CINDEX(LI1:LI2))THEN
          CLOSE(33) !ya hemos encontrado el indice buscado
          IF((ITI.EQ.02).OR. !indices atomicos
     +       (ITI.EQ.03).OR. !D4000
     +       (ITI.EQ.04).OR. !B4000
     +       (ITI.EQ.94).OR. ![MgFe]'
     +       (ITI.EQ.95).OR. ![MgFe]
     +       (ITI.EQ.96).OR. !<Fe>'
     +       (ITI.EQ.97))THEN !<Fe>
            !Nota: si se incluyen mas indices aqui, introducir las
            !transformaciones correspondientes en la subrutina RETURN_ARRAYS
            IF(LLOG)THEN
              CLLOG='y'
            ELSE
              CLLOG='n'
            END IF
            CLLOG(1:1)=READC(IDLOG,'Force logarithmic units...(y/n)',
     +       CLLOG,'yn')
            WRITE(77,112) CLLOG,'# force logarithmic units?'
            LLOG=(CLLOG.EQ.'y')
            IF(LLOG)THEN
              CUNITS='mag'
            END IF
          ELSE
            LLOG=.FALSE.
          END IF
          IF(LREV)THEN
            CLREV='y'
          ELSE
            CLREV='n'
          END IF
          CLREV(1:1)=READC(IDLOG,'Reverse axis in plots.....(y/n)',
     +     CLREV,'yn')
          WRITE(77,112) CLREV,'# reverse axis in plots?'
          LREV=(CLREV.EQ.'y')
          RETURN
        END IF
        GOTO 10
C------------------------------------------------------------------------------
12      CLOSE(33)
        WRITE(*,101) 'FATAL ERROR: index not found'
        WRITE(*,100) '==> index name: '
        WRITE(*,101) CINDEX(LI1:LI2)
        STOP
C------------------------------------------------------------------------------
900     CONTINUE
        WRITE(*,101) 'FATAL ERROR: while reading this file'
        WRITE(*,101) '==> myindex.rmodel'
        STOP
C
100     FORMAT(A,$)
101     FORMAT(A)
112     FORMAT(11X,A1,1X,A)
        END
