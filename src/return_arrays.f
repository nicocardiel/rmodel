C------------------------------------------------------------------------------
C Version 22-October-2008                                 File: return_arrays.f
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
C Devuelve en las matrices ARRAY1 y ARRAY2 los indices para la seleccion de 
C parametros elegida.
        SUBROUTINE RETURN_ARRAYS(NPARAMETERS,
!    +   NPARAMETERS_EXT,
     +   EXTMODELFILE,NLINES_TO_BE_READ)
        IMPLICIT NONE
        INTEGER NLINES_TO_BE_READ
        INTEGER NPARAMETERS
        CHARACTER*(*) EXTMODELFILE
!       INTEGER NPARAMETERS_EXT
C
        INCLUDE 'rmodel.inc'
        INCLUDE 'dimensions.inc'
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        INTEGER IEXTRAE
        REAL FEXTRAE
        CHARACTER*255 CEXTRAE
C
        INTEGER I,J
        INTEGER JJ,JJFOUND
        INTEGER L1,L2,LL1,LL2,LI1,LI2,LF1,LF2,LD1,LD2
        INTEGER IPAR1,IPAR2
        INTEGER NPARAM(2)
        INTEGER NINDEX(2)
        INTEGER NFIXED(NMAX_PARAMETERS)
        INTEGER LEN_PARAMETER(NMAX_PARAMETERS)
        INTEGER KEXPECTED(NMAX_PARAMETERS)
        INTEGER NUMTEMP
        INTEGER ITI(2)
        INTEGER ISTATUS
        INTEGER NCOLUMN_INDEX(NMAX_LEN_PARAMETER,2)
        INTEGER NCOLUMN_FIXEDPARAM(NMAX_PARAMETERS)
        INTEGER NSKIP
        REAL XPARAMETER(NMAX_LEN_PARAMETER,NMAX_PARAMETERS)
        REAL XINDICES(NMAX_LEN_BUFFER,NMAX_INDICES)
        REAL ARRAY1(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL ARRAY2(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL ARRAY1_EXT(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL ARRAY2_EXT(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL DWC(2)
        REAL FPARAM_INDEX(NMAX_LEN_PARAMETER,2)
        REAL FPARAM_TMP,FPARAM_EXP
        REAL FINDEX_TMP
        CHARACTER*255 RMODEL_DIR
        CHARACTER*255 CDUMMY,CBACKUP
        CHARACTER*255 CLAST_INDEX,CNEW_INDEX
        CHARACTER*255 CINDEX(NMAX_INDICES)
        CHARACTER*255 CPARAMETER(NMAX_PARAMETERS)
        CHARACTER*255 CFILEEXT,CEXTNAME
        CHARACTER*1000 CLONGLINE,CBACKUP_LONGLINE
        LOGICAL LPAR_OK(NMAX_PARAMETERS)
        LOGICAL LANY_FAIL
        LOGICAL LLOG(2)
        LOGICAL LEXTENSIONS
        LOGICAL LPAREXT_FOUND
        LOGICAL LINDEX_FOUND(2)
        LOGICAL LREFILL(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
C
        COMMON/BLKXINDICES/XINDICES
        COMMON/BLKARRAYS/ARRAY1,ARRAY2
        COMMON/BLKNPARAM/NPARAM
        COMMON/BLKNINDEX/NINDEX
        COMMON/BLKNFIXED/NFIXED
        COMMON/BLKLEN_PARAMETER/LEN_PARAMETER
        COMMON/BLKXPARAMETER/XPARAMETER
        COMMON/BLKLLOG/LLOG
        COMMON/BLKITI/ITI
        COMMON/BLKDWC/DWC
        COMMON/BLKCINDEX/CINDEX
        COMMON/BLKCPARAMETER/CPARAMETER
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
        NUMTEMP=0 !evita un warning de compilacion
C------------------------------------------------------------------------------
C Rellenamos las matrices para el caso normal, sin extensiones
        IPAR1=0
        IPAR2=0
        DO I=1,NLINES_TO_BE_READ
          DO J=1,NPARAMETERS
            IF(J.EQ.1)THEN
              NUMTEMP=1
            ELSE
              NUMTEMP=NUMTEMP*LEN_PARAMETER(J-1)
            END IF
            !calculamos el numero de entrada esperado para el parametro J
            KEXPECTED(J)=MOD((I-1)/NUMTEMP,LEN_PARAMETER(J))+1
            LPAR_OK(J)=.FALSE.
            IF(J.EQ.NPARAM(1))THEN
              LPAR_OK(J)=.TRUE.
            ELSEIF(J.EQ.NPARAM(2))THEN
              LPAR_OK(J)=.TRUE.
            ELSE
              IF(KEXPECTED(J).EQ.NFIXED(J))THEN
                LPAR_OK(J)=.TRUE.
              END IF
            END IF
          END DO
          LANY_FAIL=.FALSE.
          DO J=1,NPARAMETERS
            IF(.NOT.LPAR_OK(J)) LANY_FAIL=.TRUE.
          END DO
          IF(.NOT.LANY_FAIL)THEN
            IF(IPAR1.EQ.0) IPAR2=1
            IPAR1=IPAR1+1
            IF(IPAR1.GT.LEN_PARAMETER(NPARAM(1)))THEN
              IPAR1=1
              IPAR2=IPAR2+1
              IF(IPAR2.GT.LEN_PARAMETER(NPARAM(2))) IPAR2=1
            END IF
            ARRAY1(IPAR1,IPAR2)=XINDICES(I,NINDEX(1))
            ARRAY2(IPAR1,IPAR2)=XINDICES(I,NINDEX(2))
!           print*,ipar1,ipar2,array1(ipar1,ipar2),array2(ipar1,ipar2)
          END IF
        END DO
C------------------------------------------------------------------------------
C Si el segundo parametro es una extension, terminamos de rellenar las 
C matrices
        LEXTENSIONS=.FALSE.
        IF(NPARAM(2).GT.NPARAMETERS)THEN !es una extension
          L1=TRUEBEG(CPARAMETER(NPARAM(2)))
          L2=TRUELEN(CPARAMETER(NPARAM(2)))
          OPEN(25,FILE=EXTMODELFILE,STATUS='OLD',FORM='FORMATTED')
          READ(25,*) !salta las dos primeras lineas
          READ(25,*)
          !buscamos el parametro correspondiente a la extension
          LPAREXT_FOUND=.FALSE.
10        READ(25,101,END=12) CFILEEXT
          READ(25,101) CEXTNAME
          IF((CFILEEXT(1:7).NE.'# file:').OR.
     +     (CEXTNAME(1:12).NE.'# parameter:'))THEN
            WRITE(*,101) 'FATAL ERROR: invalid format in file'
            LL1=TRUEBEG(EXTMODELFILE)
            LL2=TRUELEN(EXTMODELFILE)
            WRITE(*,101) EXTMODELFILE(LL1:LL2)
            CLOSE(25)
            STOP
          END IF
          LL2=TRUELEN(CEXTNAME)
          IF(CPARAMETER(NPARAM(2))(L1:L2).EQ.CEXTNAME(14:LL2))THEN
            LPAREXT_FOUND=.TRUE.
!           print*,'parameter ',cextname(14:ll2),' found'
            LEXTENSIONS=.TRUE.
          END IF
          IF(LPAREXT_FOUND)THEN !hemos encontrado el parametro
            DO I=1,1+LEN_PARAMETER(NPARAM(2))
              READ(25,*) !salta informacion ya leida
            END DO
            DO I=1,NPARAMETERS
              READ(25,101) CDUMMY
              NCOLUMN_FIXEDPARAM(I)=IEXTRAE(CDUMMY,3,ISTATUS,CBACKUP)
              IF(ISTATUS.LE.0)THEN
                WRITE(*,100) 'FATAL ERROR: ISTATUS='
                WRITE(*,*) ISTATUS
                WRITE(*,101) 'while reading NCOLUMN_FIXEDPARAM(I)'
                STOP
              END IF
            END DO
            !leemos los indices disponibles
            LINDEX_FOUND(1)=.FALSE. !de momento no sabemos si esta este indice
            LINDEX_FOUND(2)=.FALSE. !de momento no sabemos si esta este indice
            CDUMMY='  '
            CLAST_INDEX=' '
            DO WHILE(CDUMMY(1:2).NE.'#-')
              READ(25,101) CDUMMY
              IF(CDUMMY(1:2).NE.'#-')THEN
                LI1=TRUEBEG(CLAST_INDEX)
                LI2=TRUELEN(CLAST_INDEX)
                CNEW_INDEX=CEXTRAE(CDUMMY,1,ISTATUS,CBACKUP,.TRUE.)
                LL1=TRUEBEG(CNEW_INDEX)
                LL2=TRUELEN(CNEW_INDEX)
                IF(CLAST_INDEX(LI1:LI2).NE.CNEW_INDEX(LL1:LL2))THEN
                  !es un indice nuevo
                  CLAST_INDEX=CNEW_INDEX
                  J=1
                  DO I=1,2 !miramos si es uno de los indices que buscamos
                    LI1=TRUEBEG(CINDEX(NINDEX(I)))
                    LI2=TRUELEN(CINDEX(NINDEX(I)))
                    IF(CINDEX(NINDEX(I))(LI1:LI2).EQ.
     +               CNEW_INDEX(LL1:LL2))THEN
                      IF(LINDEX_FOUND(I))THEN
                        WRITE(*,100) 'FATAL ERROR: the index '
                        WRITE(*,100) CNEW_INDEX(LL1:LL2)
                        WRITE(*,101) ' was found before in this file'
                        L1=TRUEBEG(EXTMODELFILE)
                        L2=TRUELEN(EXTMODELFILE)
                        WRITE(*,101) EXTMODELFILE(L1:L2)
                        CLOSE(25)
                        STOP
                      END IF
                      LINDEX_FOUND(I)=.TRUE.
                      NCOLUMN_INDEX(J,I)=IEXTRAE(CDUMMY,2,
     +                 ISTATUS,CBACKUP)
                      IF(ISTATUS.LE.0)THEN
                        WRITE(*,100) 'FATAL ERROR: ISTATUS='
                        WRITE(*,*) ISTATUS
                        WRITE(*,101) 'while reading NCOLUMN_INDEX(J,I)'
     +                   //' in file'
                        L1=TRUEBEG(EXTMODELFILE)
                        L2=TRUELEN(EXTMODELFILE)
                        WRITE(*,101) EXTMODELFILE(L1:L2)
                        CLOSE(25)
                        WRITE(*,100) 'with (J,I)='
                        WRITE(*,*) J,I
                        STOP
                      END IF
                      FPARAM_INDEX(J,I)=FEXTRAE(CDUMMY,3,
     +                 ISTATUS,CBACKUP)
                      IF(ISTATUS.LE.0)THEN
                        WRITE(*,100) 'FATAL ERROR: ISTATUS='
                        WRITE(*,*) ISTATUS
                        WRITE(*,101) 'while reading FPARAM_INDEX(J,I)'
     +                   //' in file'
                        L1=TRUEBEG(EXTMODELFILE)
                        L2=TRUELEN(EXTMODELFILE)
                        WRITE(*,101) EXTMODELFILE(L1:L2)
                        CLOSE(25)
                        WRITE(*,100) 'with (J,I)='
                        WRITE(*,*) J,I
                        STOP
                      END IF
!                     print*,cdummy(ll1:ll2), ' found #',i
                    END IF
                  END DO
                ELSE
                  !estamos repitiendo indice
                  DO I=1,2
                    LI1=TRUEBEG(CINDEX(NINDEX(I)))
                    LI2=TRUELEN(CINDEX(NINDEX(I)))
                    IF(CINDEX(NINDEX(I))(LI1:LI2).EQ.
     +               CNEW_INDEX(LL1:LL2))THEN
                      J=J+1
                      NCOLUMN_INDEX(J,I)=IEXTRAE(CDUMMY,2,
     +                 ISTATUS,CBACKUP)
                      IF(ISTATUS.LE.0)THEN
                        WRITE(*,100) 'FATAL ERROR: ISTATUS='
                        WRITE(*,*) ISTATUS
                        WRITE(*,101) 'while reading NCOLUMN_INDEX(J,I)'
     +                   //' in file'
                        L1=TRUEBEG(EXTMODELFILE)
                        L2=TRUELEN(EXTMODELFILE)
                        WRITE(*,101) EXTMODELFILE(L1:L2)
                        CLOSE(25)
                        WRITE(*,100) 'with (J,I)='
                        WRITE(*,*) J,I
                        STOP
                      END IF
                      FPARAM_INDEX(J,I)=FEXTRAE(CDUMMY,3,
     +                 ISTATUS,CBACKUP)
                      IF(ISTATUS.LE.0)THEN
                        WRITE(*,100) 'FATAL ERROR: ISTATUS='
                        WRITE(*,*) ISTATUS
                        WRITE(*,101) 'while reading FPARAM_INDEX(J,I)'
     +                   //' in file'
                        L1=TRUEBEG(EXTMODELFILE)
                        L2=TRUELEN(EXTMODELFILE)
                        WRITE(*,101) EXTMODELFILE(L1:L2)
                        CLOSE(25)
                        WRITE(*,100) 'with (J,I)='
                        WRITE(*,*) J,I
                        STOP
                      END IF
                    END IF
                  END DO
                END IF
              END IF
            END DO
            GOTO 12
          ELSE 
            !este no es el parametro; saltamos toda la informacion
            !intermedia hasta llegar (si existe) al siguiente parametro
            CDUMMY='  '
            DO WHILE(CDUMMY(1:2).NE.'#-')
              READ(25,101) CDUMMY
            END DO
          END IF
          GOTO 10
12        CLOSE(25)
c..............................................................................
          LF1=TRUEBEG(CFILEEXT)
          LF2=TRUELEN(CFILEEXT)
!         print*,'fichero a leer ',cfileext(9:lf2)
          DO I=1,2 !numero de indice
            IF(LINDEX_FOUND(I))THEN
!             print*,'indice(',i,') encontrado'
              DO J=1,LEN_PARAMETER(NPARAM(2))
!               print*,'columnas, parametro: ',ncolumn_index(j,i),
!    +           fparam_index(j,i),xparameter(j,nparam(2))
                IF(FPARAM_INDEX(J,I).NE.XPARAMETER(J,NPARAM(2)))THEN
                  LI1=TRUEBEG(CINDEX(NINDEX(I)))
                  LI2=TRUELEN(CINDEX(NINDEX(I)))
                  WRITE(*,100) 'FATAL ERROR: while reading the index '
                  WRITE(*,100) CINDEX(NINDEX(I))(LI1:LI2)
                  WRITE(*,101) ' in the file'
                  L1=TRUEBEG(EXTMODELFILE)
                  L2=TRUELEN(EXTMODELFILE)
                  WRITE(*,101) EXTMODELFILE(L1:L2)
                  WRITE(*,101) 'Expected parameter: '
                  WRITE(*,*) XPARAMETER(J,NPARAM(2))
                  WRITE(*,101) 'Read parameter....: '
                  WRITE(*,*) FPARAM_INDEX(J,I)
                  STOP
                END IF
              END DO
            END IF
          END DO
c..............................................................................
c si no hemos encontrado ninguno de los dos indices utilizados en la extension,
c no tiene sentido seguir
          IF((.NOT.LINDEX_FOUND(1)).AND.(.NOT.LINDEX_FOUND(2)))THEN
            WRITE(*,100) 'FATAL ERROR: neither '
            L1=TRUEBEG(CINDEX(NINDEX(1)))
            L2=TRUELEN(CINDEX(NINDEX(1)))
            WRITE(*,100) CINDEX(NINDEX(1))(L1:L2)
            WRITE(*,100) ' or '
            L1=TRUEBEG(CINDEX(NINDEX(2)))
            L2=TRUELEN(CINDEX(NINDEX(2)))
            WRITE(*,100) CINDEX(NINDEX(2))(L1:L2)
            WRITE(*,101) ' have been found in the file '
            L1=TRUEBEG(EXTMODELFILE)
            L2=TRUELEN(EXTMODELFILE)
            WRITE(*,101) EXTMODELFILE(L1:L2)
            WRITE(*,100) 'for the parameter '
            L1=TRUEBEG(CPARAMETER(NPARAM(2)))
            L2=TRUELEN(CPARAMETER(NPARAM(2)))
            WRITE(*,101) CPARAMETER(NPARAM(2))(L1:L2)
            STOP
          END IF
c..............................................................................
c el segundo parametro es una extension
          RMODEL_DIR=RMODEL_DIR_
          LD1=TRUEBEG(RMODEL_DIR)
          LD2=TRUELEN(RMODEL_DIR)
C
!         do j=1,nparameters
!           l1=truebeg(cparameter(j))
!           l2=truelen(cparameter(j))
!           if(j.eq.nparam(1))then !parametro variable
!             print*,'parametro variable: ',cparameter(j)(l1:l2),
!    +         '  in col#',ncolumn_fixedparam(j)
!             do jj=1,len_parameter(j)
!               print*,jj,xparameter(jj,j)
!             end do
!           else !parametros fijos
!             print*,'parametro fijo: ',cparameter(j)(l1:l2),
!    +         xparameter(nfixed(j),j),'  in col#',ncolumn_fixedparam(j)
!           end if
!         end do
c
          DO I=1,2 !numero de indice
            IF(LINDEX_FOUND(I))THEN !............el indice esta en la extension
              !usamos una matriz auxiliar para estar seguros que encontramos
              !los indices para todos los valores de los dos parametros (el
              !parametro genuino y la extension)
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
                  LREFILL(IPAR1,IPAR2)=.FALSE.
                END DO
              END DO
              LF1=TRUEBEG(CFILEEXT)
              LF2=TRUELEN(CFILEEXT)
              OPEN(37,FILE=RMODEL_DIR(LD1:LD2)//'/models/'//
     +         CFILEEXT(9:LF2),STATUS='OLD',FORM='FORMATTED')
              CLONGLINE='#'
              NSKIP=0 !contamos lineas a saltar
              DO WHILE(CLONGLINE(1:1).EQ.'#')
                READ(37,101) CLONGLINE
                IF(CLONGLINE(1:1).EQ.'#') NSKIP=NSKIP+1
              END DO
              CLOSE(37)
              OPEN(38,FILE=RMODEL_DIR(LD1:LD2)//'/models/'//
     +         CFILEEXT(9:LF2),STATUS='OLD',FORM='FORMATTED')
              IF(NSKIP.GT.0)THEN
                DO J=1,NSKIP
                  READ(38,*)
                END DO
              END IF
20            READ(38,101,END=22) CLONGLINE
              LANY_FAIL=.FALSE.
              JJFOUND=0
              DO J=1,NPARAMETERS
                !leemos el parametro j-esimo en una variable temporal
                FPARAM_TMP=FEXTRAE(CLONGLINE,NCOLUMN_FIXEDPARAM(J),
     +           ISTATUS,CBACKUP_LONGLINE)
                IF(ISTATUS.LE.0)THEN
                  WRITE(*,100) 'FATAL ERROR: ISTATUS='
                  WRITE(*,*) ISTATUS
                  WRITE(*,101) 'while reading FPARAM_TMP'
                  STOP
                END IF
                !si es el parametro variable, miramos si es un valor cualquiera
                !de los esperados; en ese caso, guardamos el numero de orden
                IF(J.EQ.NPARAM(1))THEN !parametro variable
                  DO JJ=1,LEN_PARAMETER(J)
                    FPARAM_EXP=XPARAMETER(JJ,J)
                    IF(FPARAM_TMP.EQ.FPARAM_EXP)THEN
                      JJFOUND=JJ
                    END IF
                  END DO
                  IF(JJFOUND.EQ.0)THEN
                    LANY_FAIL=.TRUE.
                  END IF
                ELSE !parametro(s) fijo(s)
                  !comparamos el parametro leido con el esperado
                  FPARAM_EXP=XPARAMETER(NFIXED(J),J)
                  IF(FPARAM_TMP.NE.FPARAM_EXP)THEN
                    LANY_FAIL=.TRUE.
                  END IF
                END IF
              END DO
              IF(.NOT.LANY_FAIL)THEN
!               print*,clongline(1:truelen(clongline))
                IPAR1=JJFOUND
                DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                  IF(NCOLUMN_INDEX(IPAR2,I).EQ.0)THEN
                    IF(I.EQ.1)THEN
                      FINDEX_TMP=ARRAY1(IPAR1,1)
                    ELSE
                      FINDEX_TMP=ARRAY2(IPAR1,1)
                    END IF
                  ELSE
                    FINDEX_TMP=FEXTRAE(CLONGLINE,NCOLUMN_INDEX(IPAR2,I),
     +               ISTATUS,CBACKUP_LONGLINE)
                    IF(ISTATUS.LE.0)THEN
                      WRITE(*,100) 'FATAL ERROR: ISTATUS='
                      WRITE(*,*) ISTATUS
                      WRITE(*,101) 'while reading FINDEX_TMP'
                      STOP
                    END IF
                  END IF
                  IF(I.EQ.1)THEN
                    ARRAY1_EXT(IPAR1,IPAR2)=FINDEX_TMP
                  ELSE
                    ARRAY2_EXT(IPAR1,IPAR2)=FINDEX_TMP
                  END IF
                  LREFILL(IPAR1,IPAR2)=.TRUE.
                END DO
              END IF
              GOTO 20
22            CLOSE(38)
              LANY_FAIL=.FALSE.
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
                  IF(.NOT.LREFILL(IPAR1,IPAR2))THEN
                    LANY_FAIL=.TRUE.
                  END IF
                END DO
              END DO
              IF(LANY_FAIL)THEN
                WRITE(*,101) 'FATAL ERROR: while reading the file'
                LF1=TRUEBEG(CFILEEXT)
                LF2=TRUELEN(CFILEEXT)
                WRITE(*,101) CFILEEXT(9:LF2)
                WRITE(*,101) '>>> missing combinations of parameters'
              END IF
            ELSE !............................el indice no esta en la extension
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
                  IF(I.EQ.1)THEN
                    ARRAY1_EXT(IPAR1,IPAR2)=ARRAY1(IPAR1,1)
                  ELSE
                    ARRAY2_EXT(IPAR1,IPAR2)=ARRAY2(IPAR1,1)
                  END IF
                END DO
              END DO
            END IF
          END DO
c..............................................................................
        END IF
C si ha habido extensiones, damos el cambiazo a las matrices
        IF(LEXTENSIONS)THEN
          DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
            DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
              ARRAY1(IPAR1,IPAR2)=ARRAY1_EXT(IPAR1,IPAR2)
              ARRAY2(IPAR1,IPAR2)=ARRAY2_EXT(IPAR1,IPAR2)
            END DO
          END DO
        END IF
C------------------------------------------------------------------------------
C en caso necesario, tomamos logaritmos
        DO I=1,2
          IF(LLOG(I))THEN
            IF((ITI(I).EQ.02).OR.   !indices atomicos
     +         (ITI(I).EQ.94).OR.   ![MgFe]'
     +         (ITI(I).EQ.95).OR.   ![MgFe]
     +         (ITI(I).EQ.96).OR.   !<Fe>'
     +         (ITI(I).EQ.97))THEN  !<Fe>
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
                  IF(I.EQ.1)THEN
                    ARRAY1(IPAR1,IPAR2)=
     +               -2.5*ALOG10(1.-ARRAY1(IPAR1,IPAR2)/DWC(I))
                  ELSE
                    ARRAY2(IPAR1,IPAR2)=
     +               -2.5*ALOG10(1.-ARRAY2(IPAR1,IPAR2)/DWC(I))
                  END IF
                END DO
              END DO
            ELSEIF((ITI(I).EQ.3).OR.(ITI(I).EQ.4))THEN !D4000, B4000
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
                  IF(I.EQ.1)THEN
                    ARRAY1(IPAR1,IPAR2)=
     +               2.5*ALOG10(ARRAY1(IPAR1,IPAR2))
                  ELSE
                    ARRAY2(IPAR1,IPAR2)=
     +               2.5*ALOG10(ARRAY2(IPAR1,IPAR2))
                  END IF
                END DO
              END DO
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
