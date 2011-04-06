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
C Programa rmodel
C------------------------------------------------------------------------------
C
C Para incluir un nuevo modelo basta con generar, dentro del subdirectorio
C "models", los ficheros *.dat, *.def (y si hace falta *.ext) ---ver
C explicacion de estos ficheros a continuacion---. Hay que aumentar en una
C unidad el valor del parametro NMAX_MODELS en el fichero "dimensions.inc",
C e incluir en "rmodel.f" la raiz del nombre de los modelos (ir a la linea
C inicial del codigo en la que se define el DATA(MODELFILE(I)...).
C
C En una versión más reciente, también se ha incluido NMAX_MODELS_MILES
C para poder incluir modelos calculados con los modelos de MILES con una
C numeración diferente.
C
C------------------------------------------------------------------------------
C Para economizar memoria, se asume que los modelos estan en una tabla
C bidimensional dividida en dos partes. En las primeras columnas van variando
C los parametros (edad, metalicidad, [alpha/Fe],...), de forma que las primeras
C columnas son las más rápidas en variar. A continuación vienen los índices
C medidos para cada configuración de parámetros. De esta forma, toda la tabla
C de índices puede almacenarse en arrays unidimensionales para cada índice,
C estando la dimensión máxima de estos arrays determinada por el parámetro
C MAX_LEN_BUFFER.
C
C Para insertar un nuevo modelo, basta con generar un fichero *.dat con los
C datos en columnas (tal y como se describe arriba), y un fichero *.def que
C contenga la informacion sobre los parametros del modelo considerado y el
C listado de los indices predichos. Si, ademas, existen predicciones para otros
C parametros (que llamaremos extensiones) que solo estan disponibles para
C algunos indices, resulta poco "rentable" incluir dichos parametros en el
C mismo tipo de tabla general (podemos tener una tabla muy grande llena de
C ceros). Por ello se puede especificar dichos parametros en otro fichero,
C *.ext, en el que se indican todos los parametros adicionales, su recorrido,
C los ficheros externos en los que se encuentran las predicciones, y las
C columnas en las que podemos encontrar tanto los parametros como los indices.
C Como ejemplo ver el fichero model_thomas03.ext.
C
C Para introducir nuevos indices, hay que modificar las subrutinas
C INDEXINFO y RETURN_ARRAYS.
C
C El programa puede calcular, en caso necesario, indices compuestos como 
C <Fe>, [MgFe], etc. (NOTA: esto no es posible con indices en extensiones; 
C en este caso lo mas facil es generar, con un programa externo, las columnas
C correspondientes a estos indices compuestos e insertarlos en el fichero
C *.dat general). Para calcular los indices compuestos asociamos a cada indice 
C una variable auxiliar llamada COMP_INDEX(0:NBUFF_COMP_INDEX,NMAX_INDICES). 
C Es una matriz bidimensional en la que a cada indice se le asocia una matriz 
C unidimensional de 0 a NBUFF_COMP_INDEX. El primer numero indica el tipo de 
C indice compuesto:
C    0: no es indice compuesto
C    1: <Fe>=0.5*(Fe52+Fe53)
C    2: <Fe>*=0.72*Fe52+0.28*Fe53
C    3: [MgFe]=sqrt(Mgb*<Fe>)
C    4: [MgFe]*=sqrt(Mgb*<Fe>')
C Los siguientes NBUFF_COMP_INDEX numeros sirven como buffer para los posibles 
C indices que pueden llegar a ser necesarios (en el caso <Fe> solo hacen falta 
C 2, mientras que para el [MgFe] hacen falta 3; dimensiono hasta 
C NBUFF_COMP_INDEX para posibles futuros indices mas complejos). En el fichero 
C de definicion de indices, estos indices compuestos tienen que ir precedidos 
C por el simbolo "@", para indicar asi que hay que calcularlos a partir de los 
C indices individuales. Estos indices compuestos pueden aparecer en cualquier 
C lugar en dicho fichero, porque primero se leen los indices que no son 
C compuestos. Esto permite insertarlos aproximadamente en el orden esperado 
C por las longitudes de onda involucradas.
C------------------------------------------------------------------------------
        PROGRAM RMODEL
        IMPLICIT NONE
C
        INCLUDE 'rmodel.inc'
        INCLUDE 'dimensions.inc'
        INCLUDE 'version.inc'
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER READI
        INTEGER READILIM
        INTEGER PGOPEN
        INTEGER SYSTEMFUNCTION
        INTEGER FINDINDEX
        REAL FEXTRAE
        REAL READF
        REAL FMEAN0
        REAL FPERCENT
        REAL FCHISQR
        CHARACTER*255 READC
        LOGICAL INSIDE_ROMBO
C
        REAL RANRED
        EXTERNAL RANRED
C
        INTEGER I,J,II,JJ
        INTEGER I0,J0
        INTEGER K,NSEED
        INTEGER L,L1,L2
        INTEGER LD1,LD2
        INTEGER N1,N2
        INTEGER IDPLOT
        INTEGER IDLOG
        INTEGER ISYSTEM
        INTEGER NMODEL,NMODEL_OLD
        INTEGER NPARAMETERS,NPARAMETERS_EXT
        INTEGER NPARAM(2),NPARAM_TEMP
        INTEGER LEN_PARAMETER(NMAX_PARAMETERS)
        INTEGER LEN_PARAMETER_SKIPPED(NMAX_PARAMETERS)
        INTEGER NINDICES
        INTEGER NINDEX(2)
        INTEGER ITI(2)
        INTEGER COMP_INDEX(0:10,NMAX_INDICES) !ver explicacion mas arriba
        INTEGER NSKIP
        INTEGER NLINES_TO_BE_READ
        INTEGER ISTATUS
        INTEGER KEXPECTED(NMAX_PARAMETERS)
        INTEGER NUMTEMP
        INTEGER NCOLUMN
        INTEGER NFIXED(NMAX_PARAMETERS)
        INTEGER IPAR1,IPAR2,IPAR1_MIN,IPAR2_MIN
        INTEGER IBEST
        INTEGER NSIMUL,NSIGMA
        INTEGER ISIGMA
        INTEGER NP_IN_ELIPSE(3)
        INTEGER NFIT
        INTEGER NOBJPLOT,NCURRENT_OBJECT
        INTEGER OBJSTATUS
        REAL XPARAMETER(NMAX_LEN_PARAMETER,NMAX_PARAMETERS)
        REAL XPARAM_MIN,XPARAM_MAX
        REAL FDUM_XPARAMETER(NMAX_PARAMETERS)
        REAL XINDICES(NMAX_LEN_BUFFER,NMAX_INDICES)
        REAL MGB,FE5270,FE5335
        REAL ARRAY1(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL ARRAY2(NMAX_LEN_PARAMETER,NMAX_LEN_PARAMETER)
        REAL DWC(2),DWCX,DWCY
        REAL FERR(2),FERRX,FERRY
        REAL INDEX_X,EINDEX_X,INDEX_Y,EINDEX_Y
        REAL INDX0,INDY0
        REAL DIST_MIN,DIST_TMP
        REAL FPAR1_MIN,FPAR2_MIN
        REAL FPAR1_FIT,FPAR2_FIT
        REAL X1(NMAX_LEN_PARAMETER),X2(NMAX_LEN_PARAMETER)
        REAL Y1(NMAX_LEN_PARAMETER),Y2(NMAX_LEN_PARAMETER)
        REAL W1(NMAX_LEN_PARAMETER),W2(NMAX_LEN_PARAMETER)
        REAL A11,A12,A21,A22 !A matrix
        REAL B11,B12,B21,B22 !B matrix
        REAL A11_LINEAR,A12_LINEAR,A21_LINEAR,A22_LINEAR
        REAL A11_1,A11_2,A12_1,A12_2,A21_1,A21_2,A22_1,A22_2
        REAL X0,Y0,XX,YY
        REAL RHO_X,RHO_Y
        REAL DETA_LINEAR
        REAL SIX,SIY
        REAL DETA,DETB
        REAL FDEGE,FDEGEX,FDEGEY
        REAL FAREA
        REAL XFIT(9),YFIT(9),UFIT(9),VFIT(9)
        REAL UU,VV
        REAL AIJ(6),BIJ(6)
        REAL AIJ_(6),BIJ_(6)
        REAL DETER
        REAL XP(NPMAX),YP(NPMAX)
        REAL ERRIX,ERRIY
        REAL DELTA_PAR1_,DELTA_PAR2_
        REAL DELTA_PAR1(NSIMULMAX),DELTA_PAR2(NSIMULMAX)
        REAL DELTA_IX(NSIMULMAX),DELTA_IY(NSIMULMAX)
        REAL AA1,BB1,AA2,BB2,AA3,BB3
        REAL FPAR1_SIMUL(NSIMULMAX),FPAR2_SIMUL(NSIMULMAX)
        REAL FPAR1_SIMUL1S(0:360),FPAR2_SIMUL1S(0:360)
        REAL FPAR1_SIMUL2S(0:360),FPAR2_SIMUL2S(0:360)
        REAL FPAR1_SIMUL3S(0:360),FPAR2_SIMUL3S(0:360)
        REAL FPAR1_P,FPAR2_P
        REAL SNRATX,SNRATY,R1,R2
        REAL MEANERR_PAR1,MEANERR_PAR2
        REAL SIGMAERR_PAR1,SIGMAERR_PAR2
        REAL MEDIAN_PAR1,MEDIAN_PAR2
        REAL PERC1_PAR1,PERC2_PAR1
        REAL PERC1_PAR2,PERC2_PAR2
        REAL T,AREA_II(3),AREA_AZ(3)
        REAL XMIN_,XMAX_,YMIN_,YMAX_,DX_,DY_
        REAL XEL(NPOINTSELLIPSE),YEL(NPOINTSELLIPSE)
        REAL XOBJ(NMAX_OBJECTS),EXOBJ(NMAX_OBJECTS)
        REAL YOBJ(NMAX_OBJECTS),EYOBJ(NMAX_OBJECTS)
        CHARACTER*1 CSAVE
        CHARACTER*1 CCHAR
        CHARACTER*1 CFPOINT,CCHECK
        CHARACTER*1 CALLOBJ
        CHARACTER*1 CPAUSE
        CHARACTER*1 CCONT
        CHARACTER*5 COPC
        CHARACTER*10 CUNITS(2)
        CHARACTER*79 CSEPARATOR
        CHARACTER*255 CMODELNAME
        CHARACTER*255 CLABEL(2)
        CHARACTER*255 CDUMMY
        CHARACTER*255 MODELFILE(0:NMAX_MODELS+NMAX_MODELS_MILES)
        CHARACTER*255 DEFMODELFILE(0:NMAX_MODELS+NMAX_MODELS_MILES)
        CHARACTER*255 DATMODELFILE(0:NMAX_MODELS+NMAX_MODELS_MILES)
        CHARACTER*255 EXTMODELFILE(0:NMAX_MODELS+NMAX_MODELS_MILES)
        CHARACTER*255 CMODEL(0:NMAX_MODELS+NMAX_MODELS_MILES)
        CHARACTER*255 TTER
        CHARACTER*255 BATCHFILE
        CHARACTER*255 FILELOG
        CHARACTER*255 RMODEL_DIR
        CHARACTER*255 CPARAMETER(NMAX_PARAMETERS)
        CHARACTER*255 CINDEX(NMAX_INDICES)
        CHARACTER*255 COUTFILE
        CHARACTER*255 CRESULTSFILE
        CHARACTER*255 OBJNAME(NMAX_OBJECTS)
        CHARACTER*255 OBJ_INFILE
        CHARACTER*255 CLINEA39
        CHARACTER*255 CLINEOUT
        CHARACTER*10000 CLONGLINE,CBACKUP_LONGLINE
        LOGICAL LECHO
        LOGICAL LOGFILE
        LOGICAL LMODEL_USER
        LOGICAL LLOG(2),LXLOG,LYLOG
        LOGICAL LREV(2)
        LOGICAL LABPAR(2)
        LOGICAL LIAUTO,LIAUTO_OLD,LIMIASK
        LOGICAL LRENORM
        LOGICAL LINSIDE_ROMBO
        LOGICAL LNOT_ENOUGH
        LOGICAL LEXTERNAL
        LOGICAL L14,L39
C
        COMMON/BLKNSEED/NSEED
        COMMON/BLKLECHO/LECHO
        COMMON/BLKCMODELNAME/CMODELNAME
        COMMON/BLKCINDEX/CINDEX
        COMMON/BLKXINDICES/XINDICES
        COMMON/BLKARRAYS/ARRAY1,ARRAY2
        COMMON/BLKNPARAM/NPARAM
        COMMON/BLKNINDEX/NINDEX
        COMMON/BLKNFIXED/NFIXED
        COMMON/BLKLEN_PARAMETER/LEN_PARAMETER
        COMMON/BLKXPARAMETER/XPARAMETER
        COMMON/BLKLLOG/LLOG
        COMMON/BLKLREV/LREV
        COMMON/BLKCUNITS/CUNITS
        COMMON/BLKCLABEL/CLABEL
        COMMON/BLKITI/ITI
        COMMON/BLKDWC/DWC
        COMMON/BLKLABPAR/LABPAR
        COMMON/BLKLIAUTO/LIAUTO,LIMIASK
        COMMON/BLKCPARAMETER/CPARAMETER
        COMMON/BLKOBJPLOT1/NOBJPLOT
        COMMON/BLKOBJPLOT2/XOBJ,EXOBJ,YOBJ,EYOBJ
        COMMON/BLKOBJPLOT3/OBJNAME
        COMMON/BLKOBJPLOT4/OBJ_INFILE
C------------------------------------------------------------------------------
C Welcome message
        WRITE(*,203)
        WRITE(*,101) '                       Welcome to Rmodel '//
     +   '(version '//VERSION(1:TRUELEN(VERSION))//')'
        WRITE(*,202)
        WRITE(*,100) '   For more information visit:'
        WRITE(*,101) ' http://www.ucm.es/info/Astrof/software/rmodel'
        WRITE(*,203)
        WRITE(*,*)
C definimos los ficheros con las especificaciones de cada modelo
C
C NOTA: si se modifica incluyendo nuevos modelos, cambiar tambien la dimension
C       de NMAX_MODELS o de NMAX_MODELS_MILES en el fichero dimensions.inc
C
        DATA(MODELFILE(I),I=0,NMAX_MODELS)
     +  /
     +   'model_user',
     +   'models/model_bc01',
     +   'models/model_bc03',
     +   'models/model_lw05',
     +   'models/model_thomas03',
     +   'models/model_vazdekis06'
     +  /
        DATA(MODELFILE(I),I=NMAX_MODELS+1,NMAX_MODELS+NMAX_MODELS_MILES)
     +  /
     +   'models/MILES_KU_LIS-14.0'
     +  /
        DO I=1,79
          CSEPARATOR(I:I)='#'
        END DO
C parametros iniciales
        LMODEL_USER=.FALSE.
        NSEED=-1
        NMODEL_OLD=-1 !todavia ninguno
        CFPOINT='3'
        CPAUSE='y'
        L14=.TRUE.
        L39=.FALSE.
C definimos el directorio de instalacion del programa
        RMODEL_DIR=RMODEL_DIR_
        LD1=TRUEBEG(RMODEL_DIR)
        LD2=TRUELEN(RMODEL_DIR)
C------------------------------------------------------------------------------
        NUMTEMP=0 !evita un warning de compilacion
C------------------------------------------------------------------------------
C chequeamos que tenemos los ficheros con las especificaciones de cada modelo
        DO I=0,NMAX_MODELS+NMAX_MODELS_MILES
          L1=TRUEBEG(MODELFILE(I))
          L2=TRUELEN(MODELFILE(I))
          IF(I.EQ.0)THEN
            DEFMODELFILE(I)=MODELFILE(I)(L1:L2)//'.def'
            DATMODELFILE(I)=MODELFILE(I)(L1:L2)//'.dat'
            EXTMODELFILE(I)=MODELFILE(I)(L1:L2)//'.ext'
          ELSE
            DEFMODELFILE(I)=RMODEL_DIR(LD1:LD2)//'/'//
     +       MODELFILE(I)(L1:L2)//'.def'
            DATMODELFILE(I)=RMODEL_DIR(LD1:LD2)//'/'//
     +       MODELFILE(I)(L1:L2)//'.dat'
            EXTMODELFILE(I)=RMODEL_DIR(LD1:LD2)//'/'//
     +       MODELFILE(I)(L1:L2)//'.ext'
          END IF
          INQUIRE(FILE=DEFMODELFILE(I),EXIST=LOGFILE)
          IF(I.EQ.0) LMODEL_USER=LOGFILE
          IF(.NOT.LOGFILE)THEN
            IF(I.EQ.0)THEN
              CMODEL(I)='NONE (file model_user.def not found)'
            ELSE
              WRITE(*,101) 'FATAL ERROR: the following file does'//
     +         ' not exist:'
              WRITE(*,101) RMODEL_DIR(LD1:LD2)//'/'//
     +         MODELFILE(I)(L1:L2)//'.def'
              STOP
            END IF
          ELSE !leemos la primera linea con descripcion del modelo
            OPEN(10,FILE=DEFMODELFILE(I),STATUS='OLD',FORM='FORMATTED')
            READ(10,101) CMODEL(I)
            CMODEL(I)=CMODEL(I)(3:)
            CLOSE(10)
          END IF
        END DO
C------------------------------------------------------------------------------
C Abrimos la salida grafica
        WRITE(*,100) 'Graphics device/type '//
     +   '(? to see list, default /XServe): '
        READ(*,101) TTER
        IF(TRUELEN(TTER).EQ.0) TTER='/XServe'
        IF(TTER(1:1).EQ.'@')THEN
          WRITE(*,101) TTER(1:TRUELEN(TTER))
          TTER=TTER(2:)
          LECHO=.TRUE.
        ELSE
          LECHO=.FALSE.
        END IF
        IDPLOT=PGOPEN(TTER)
        IF(IDPLOT.LE.0) STOP 'FATAL ERROR in PGOPEN'
        CALL PGASK(.FALSE.)
        CALL PGSLW(2)
        CALL PGSCF(2)
        CALL PGSCH(1.5)
        CALL SET_MY_COLORS
C------------------------------------------------------------------------------
C Abrimos el fichero log de esta sesion
        OPEN(77,FILE='rmodel.log',STATUS='UNKNOWN',FORM='FORMATTED')
        WRITE(77,101) '# rmodel version: '//VERSION
        WRITE(77,101) CSEPARATOR
C------------------------------------------------------------------------------
C Indicamos si usamos un fichero log ya existente
        WRITE(*,100) 'Previous log file (<CR>=none)? '
        READ(*,101) BATCHFILE
        IF(LECHO) WRITE(*,101) BATCHFILE(1:TRUELEN(BATCHFILE))
        IF(TRUELEN(BATCHFILE).GT.0)THEN
          LOGFILE=.FALSE.
          DO WHILE(.NOT.LOGFILE)
            INQUIRE(FILE=BATCHFILE,EXIST=LOGFILE)
            IF(.NOT.LOGFILE)THEN
              WRITE(*,100) 'ERROR: this file does not exist. '
              WRITE(*,101) 'Try again.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
              WRITE(*,100) 'Previous log file (<CR>=none)? '
              READ(*,101) BATCHFILE
              IF(TRUELEN(BATCHFILE).EQ.0)THEN
                LOGFILE=.TRUE.
              ELSE
                IF(LECHO) WRITE(*,101) BATCHFILE(1:TRUELEN(BATCHFILE))
              END IF
            END IF
          END DO
          IF(TRUELEN(BATCHFILE).EQ.0)THEN
            IDLOG=0
          ELSE
            IDLOG=78
            OPEN(IDLOG,FILE=BATCHFILE,STATUS='OLD',FORM='FORMATTED')
          END IF
        ELSE
          IDLOG=0
        END IF
C------------------------------------------------------------------------------
C Mostramos y elegimos modelo
10      WRITE(*,*)
C------------------------------------------------------------------------------
        WRITE(*,201)
        DO I=0,NMAX_MODELS+NMAX_MODELS_MILES
          IF(I.LE.NMAX_MODELS)THEN
            II=I
          ELSE
            II=I-NMAX_MODELS+100
          END IF
          IF(I.EQ.NMAX_MODELS+1) WRITE(*,202)
          L1=TRUEBEG(CMODEL(I))
          L2=TRUELEN(CMODEL(I))
          WRITE(*,'(A6,I3.3,A1,1X,A)') 'Model#',II,':',CMODEL(I)(L1:L2)
        END DO
        IF(NMODEL_OLD.EQ.-1)THEN
          CDUMMY='@'
        ELSE
          IF(NMODEL_OLD.GT.NMAX_MODELS)
     +     NMODEL_OLD=NMODEL_OLD-NMAX_MODELS+100
          WRITE(CDUMMY,*) NMODEL_OLD
        END IF
        WRITE(*,201)
        COPC(1:5)=READC(IDLOG,'Model number (q=QUIT)',
     +   CDUMMY,'qQ0123456789')
C..............................................................................
        IF((COPC.EQ.'q').OR.(COPC.EQ.'Q')) GOTO 998
        IF((INDEX(COPC,'q').EQ.0).AND.(INDEX(COPC,'Q').EQ.0))THEN
          READ(COPC,*) NMODEL
          IF(NMODEL.LT.0)THEN
            WRITE(*,100) '**ERROR** Press <CR> to continue...'
            READ(*,*)
            GOTO 10
          END IF
          IF(NMODEL.GT.NMAX_MODELS)THEN
            IF((NMODEL.LE.100).OR.(NMODEL.GT.100+NMAX_MODELS_MILES))THEN
              WRITE(*,100) '**ERROR** Press <CR> to continue...'
              READ(*,*)
              GOTO 10
            END IF
          END IF
          IF((NMODEL.EQ.0).AND.(.NOT.LMODEL_USER))THEN
            MODELFILE(0)=READC(IDLOG,
     +       'Filename of model data (without extension)','@','@')
            L1=TRUEBEG(MODELFILE(0))
            L2=TRUELEN(MODELFILE(0))
            DEFMODELFILE(0)=MODELFILE(0)(L1:L2)//'.def'
            DATMODELFILE(0)=MODELFILE(0)(L1:L2)//'.dat'
            EXTMODELFILE(0)=MODELFILE(0)(L1:L2)//'.ext'
            INQUIRE(FILE=DEFMODELFILE(0),EXIST=LOGFILE)
            IF(LOGFILE)THEN
              OPEN(10,FILE=DEFMODELFILE(0),STATUS='OLD',
     +         FORM='FORMATTED')
              READ(10,101) CMODEL(0)
              CMODEL(0)=CMODEL(0)(3:)
              CLOSE(10)
              WRITE(*,101)
              WRITE(*,100) '>>> Model description: '
              L1=TRUEBEG(CMODEL(0))
              L2=TRUELEN(CMODEL(0))
              WRITE(*,101) CMODEL(0)(L1:L2)
            ELSE
              WRITE(*,101) 'ERROR: the following file does not exist:'
              L1=TRUEBEG(DEFMODELFILE(0))
              L2=TRUELEN(DEFMODELFILE(0))
              WRITE(*,101) DEFMODELFILE(0)(L1:L2)
              READ(*,*)
              GOTO 10
            END IF
          END IF
        ELSE
          WRITE(*,100) '**ERROR** Press <CR> to continue...'
          READ(*,*)
          GOTO 10
        END IF
C
        L=TRUELEN(COPC)
        WRITE(77,100) '       '
        IF(L.LT.5)THEN
          DO I=1,5-L
            WRITE(77,100) ' '
          END DO
        END IF
        WRITE(77,101) COPC(1:L)//' # Model number'
        IF((NMODEL.EQ.0).AND.(.NOT.LMODEL_USER))THEN
          L1=TRUEBEG(MODELFILE(0))
          L2=TRUELEN(MODELFILE(0))
          WRITE(77,101) MODELFILE(0)(L1:L2)//' # Filename of model data'
        END IF
        IF(NMODEL.GT.100) NMODEL=NMODEL-100+NMAX_MODELS
C------------------------------------------------------------------------------
C Valores por defecto
        LABPAR(1)=.TRUE.
        LABPAR(2)=.TRUE.
        LIAUTO=.FALSE.
        LIMIASK=.FALSE.
C
        IF(NMODEL.NE.NMODEL_OLD)THEN
          NSIMUL=1000
          SNRATX=50.0
          SNRATY=50.0
C
          NPARAM(1)=0
          NPARAM(2)=0
C
          DO I=1,NMAX_PARAMETERS
            NFIXED(I)=0
          END DO
C
          NINDEX(1)=0
          NINDEX(2)=0
C
          IPAR1_MIN=0
          IPAR2_MIN=0
C
          LLOG(1)=.FALSE.
          LLOG(2)=.FALSE.
          LREV(1)=.FALSE.
          LREV(2)=.FALSE.
C
          NMODEL_OLD=NMODEL
        END IF
C------------------------------------------------------------------------------
C Una vez elegido el modelo, leemos parametros e indices disponibles
        OPEN(10,FILE=DEFMODELFILE(NMODEL),STATUS='OLD',FORM='FORMATTED')
        READ(10,101) CMODELNAME !descripcion del modelo
        CMODELNAME=CMODELNAME(3:)
        READ(10,101) CDUMMY !saltamos linea
        READ(10,101) CDUMMY !leemos numero de parametros
        READ(CDUMMY(25:),*) NPARAMETERS
        IF(NPARAMETERS.GT.NMAX_PARAMETERS)THEN
          WRITE(*,101) 'FATAL ERROR: NPARAMETERS.GT.NMAX_PARAMETERS'
          WRITE(*,100) 'NPARAMETERS....: '
          WRITE(*,*) NPARAMETERS
          WRITE(*,100) 'NMAX_PARAMETERS: '
          WRITE(*,*) NMAX_PARAMETERS
          CLOSE(10)
          STOP
        END IF
        READ(10,*) CDUMMY !saltamos linea
        WRITE(*,*)
        WRITE(*,100) '>>> Total number of parameters: '
        WRITE(*,*) NPARAMETERS
        DO I=1,NPARAMETERS
          READ(10,101) CDUMMY
          CPARAMETER(I)=CDUMMY(3:)
          L1=TRUEBEG(CPARAMETER(I))
          L2=TRUELEN(CPARAMETER(I))
          WRITE(*,'(A6,I3.3,A2,$)') 'Param#',I,'> '
          WRITE(*,101) CPARAMETER(I)(L1:L2)
          LEN_PARAMETER(I)=0
          LEN_PARAMETER_SKIPPED(I)=0
          CDUMMY(1:1)=' '
          DO WHILE(CDUMMY(1:1).NE.'#')
            READ(10,101) CDUMMY
            IF((CDUMMY(1:1).NE.'#').AND.(CDUMMY(1:1).NE.'!'))THEN
              LEN_PARAMETER(I)=LEN_PARAMETER(I)+1
              IF(LEN_PARAMETER(I).GT.NMAX_LEN_PARAMETER)THEN
                WRITE(*,*)
                WRITE(*,101) 'FATAL ERROR: LEN_PARAMETER(I).GT.'//
     +           'MAX_LEN_PARAMETER'
                WRITE(*,100) 'LEN_PARAMETER(I).: '
                WRITE(*,*) LEN_PARAMETER(I)
                WRITE(*,100) 'NMAX_LEN_PARAMETER: '
                WRITE(*,*) NMAX_LEN_PARAMETER
                CLOSE(10)
                STOP
              END IF
              READ(CDUMMY,*) XPARAMETER(LEN_PARAMETER(I),I)
            ELSEIF(CDUMMY(1:1).EQ.'!')THEN
              LEN_PARAMETER_SKIPPED(I)=LEN_PARAMETER_SKIPPED(I)+1
            END IF
          END DO
          WRITE(*,'(A11,I3.3,A,$)') '---------> ',LEN_PARAMETER(I),
     +     ' values read'
          WRITE(*,'(A,I3.3,A)') ' (',LEN_PARAMETER_SKIPPED(I),
     +     ' values skipped)'
        END DO
        !Buscamos parametros extra
        INQUIRE(FILE=EXTMODELFILE(NMODEL),EXIST=LOGFILE)
        NPARAMETERS_EXT=0
        IF(LOGFILE)THEN
          OPEN(11,FILE=EXTMODELFILE(NMODEL),STATUS='OLD',
     +     FORM='FORMATTED')
          READ(11,*) !saltamos dos primeras lineas
          READ(11,*)
15        READ(11,101,END=16) CDUMMY
          READ(11,101) CDUMMY
          NPARAMETERS_EXT=NPARAMETERS_EXT+1
          IF(NPARAMETERS+NPARAMETERS_EXT.GT.NMAX_PARAMETERS)THEN
            WRITE(*,101) 'FATAL ERROR: (NPARAMETERS+NPARAMETERS_EXT)'//
     +       '.GT.NMAX_PARAMETERS'
            WRITE(*,100) 'NPARAMETERS....: '
            WRITE(*,*) NPARAMETERS
            WRITE(*,100) 'NPARAMETERS_EXT: '
            WRITE(*,*) NPARAMETERS_EXT
            WRITE(*,100) 'NMAX_PARAMETERS: '
            WRITE(*,*) NMAX_PARAMETERS
            CLOSE(11)
            CLOSE(10)
            STOP
          END IF
          L1=INDEX(CDUMMY,':')
          L2=TRUELEN(CDUMMY)
          WRITE(*,'(A6,I3.3,A2,$)') 'Param#',
     +     NPARAMETERS+NPARAMETERS_EXT,'> '
          WRITE(*,100) CDUMMY(L1+2:L2)
          CPARAMETER(NPARAMETERS+NPARAMETERS_EXT)=CDUMMY(L1+2:L2)
          IF(L2-L1-2.LT.30)THEN
            DO L=1,30-(L2-L1-2)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) ': '
          READ(11,*) LEN_PARAMETER(NPARAMETERS+NPARAMETERS_EXT)
          WRITE(*,'(I3.3,A)') LEN_PARAMETER(NPARAMETERS+NPARAMETERS_EXT)
     +     ,' values read (EXTENSION)'
          DO I=1,LEN_PARAMETER(NPARAMETERS+NPARAMETERS_EXT)
            READ(11,*) XPARAMETER(I,NPARAMETERS+NPARAMETERS_EXT)
          END DO
          DO I=1,NPARAMETERS
            READ(11,101) CDUMMY
          END DO
          CDUMMY='  '
          DO WHILE(CDUMMY(1:2).NE.'#-')
            READ(11,101) CDUMMY
          END DO
          GOTO 15
16        CLOSE(11)
        END IF
        !Ahora hay que leer los indices
        READ(10,101) CDUMMY
        IF(CDUMMY(1:9).NE.'# indices')THEN
          WRITE(*,101) 'FATAL ERROR: missing "# indices" keyword'
          CLOSE(10)
          STOP
        END IF
        NINDICES=0
        CDUMMY(1:1)=' '
        DO WHILE(CDUMMY(1:1).NE.'#')
          READ(10,101) CDUMMY
          IF(CDUMMY(1:1).NE.'#')THEN
            NINDICES=NINDICES+1
            IF(NINDICES.GT.NMAX_INDICES)THEN
              WRITE(*,101) 'FATAL ERROR: NINDICES.GT.NMAX_INDICES'
              WRITE(*,100) 'NINDICES....: '
              WRITE(*,*) NINDICES
              WRITE(*,100) 'NMAX_INDICES: '
              WRITE(*,*) NMAX_INDICES
              CLOSE(10)
              STOP
            END IF
            CINDEX(NINDICES)=CDUMMY
          END IF
        END DO
        CLOSE(10)
        !Una vez leidos todos los indices, comprobamos si tendremos que
        !calcular indices compuestos; si es asi, determinamos que indices
        !no compuestos habra que emplear en cada caso
        DO I=1,NINDICES
          DO J=0,NBUFF_COMP_INDEX
            COMP_INDEX(J,I)=0
          END DO
          IF(CINDEX(I)(1:1).EQ.'@')THEN
            L1=TRUEBEG(CINDEX(I))
            L2=TRUELEN(CINDEX(I))
            IF(CINDEX(I)(L1:L2).EQ.'@<Fe>')THEN
              COMP_INDEX(0,I)=1
              COMP_INDEX(1,I)=FINDINDEX('Fe5270',NINDICES)
              IF(COMP_INDEX(1,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5270 index'
                STOP
              END IF
              COMP_INDEX(2,I)=FINDINDEX('Fe5335',NINDICES)
              IF(COMP_INDEX(2,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5335 index'
                STOP
              END IF
              CINDEX(I)='<Fe>'
            ELSEIF(CINDEX(I)(L1:L2).EQ.'@<Fe>*')THEN
              COMP_INDEX(0,I)=2
              COMP_INDEX(1,I)=FINDINDEX('Fe5270',NINDICES)
              IF(COMP_INDEX(1,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5270 index'
                STOP
              END IF
              COMP_INDEX(2,I)=FINDINDEX('Fe5335',NINDICES)
              IF(COMP_INDEX(2,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5335 index'
                STOP
              END IF
              CINDEX(I)='<Fe>*'
            ELSEIF(CINDEX(I)(L1:L2).EQ.'@[MgFe]')THEN
              COMP_INDEX(0,I)=3
              COMP_INDEX(1,I)=FINDINDEX('Mgb5177',NINDICES)
              IF(COMP_INDEX(1,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Mgb5177 index'
                STOP
              END IF
              COMP_INDEX(2,I)=FINDINDEX('Fe5270',NINDICES)
              IF(COMP_INDEX(2,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5270 index'
                STOP
              END IF
              COMP_INDEX(3,I)=FINDINDEX('Fe5335',NINDICES)
              IF(COMP_INDEX(3,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5335 index'
                STOP
              END IF
              CINDEX(I)='[MgFe]'
            ELSEIF(CINDEX(I)(L1:L2).EQ.'@[MgFe]*')THEN
              COMP_INDEX(0,I)=4
              COMP_INDEX(1,I)=FINDINDEX('Mgb5177',NINDICES)
              IF(COMP_INDEX(1,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Mgb5177 index'
                STOP
              END IF
              COMP_INDEX(2,I)=FINDINDEX('Fe5270',NINDICES)
              IF(COMP_INDEX(2,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5270 index'
                STOP
              END IF
              COMP_INDEX(3,I)=FINDINDEX('Fe5335',NINDICES)
              IF(COMP_INDEX(3,I).EQ.0)THEN
                WRITE(*,101) 'FATAL ERROR: missing Fe5335 index'
                STOP
              END IF
              CINDEX(I)='[MgFe]*'
            ELSE
              WRITE(*,100) 'FATAL ERROR: unexpected '
              WRITE(*,100) CINDEX(I)(L1:L2)
              WRITE(*,101) ' index.'
              STOP
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
C leemos las predicciones del modelo para todos los parametros y todos los
C indices
        INQUIRE(FILE=DATMODELFILE(NMODEL),EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          L1=TRUEBEG(MODELFILE(NMODEL))
          L2=TRUELEN(MODELFILE(NMODEL))
          WRITE(*,100) 'FATAL ERROR: the file "'
          WRITE(*,100) MODELFILE(NMODEL)(L1:L2)//'.dat'
          WRITE(*,101) '" does not exist.'
          STOP
        END IF
        !contamos las lineas iniciales con comentarios
        NSKIP=0
        OPEN(10,FILE=DATMODELFILE(NMODEL),STATUS='OLD',FORM='FORMATTED')
        CDUMMY(1:1)='#'
        DO WHILE(CDUMMY(1:1).EQ.'#')
          READ(10,101) CDUMMY
          IF(CDUMMY(1:1).EQ.'#') NSKIP=NSKIP+1
        END DO
        CLOSE(10)
        !determinamos numero de lineas a leer con indices
        NLINES_TO_BE_READ=1
        DO I=1,NPARAMETERS
          NLINES_TO_BE_READ=NLINES_TO_BE_READ*LEN_PARAMETER(I)
        END DO
        IF(NLINES_TO_BE_READ.GT.NMAX_LEN_BUFFER)THEN
          WRITE(*,101) 'FATAL ERROR: NLINES_TO_BE_READ.GT.'//
     +     'NMAX_LEN_BUFFER'
          WRITE(*,100) 'NLINES_TO_BE_READ: '
          WRITE(*,*) NLINES_TO_BE_READ
          WRITE(*,100) 'NMAX_LEN_BOFFER..: '
          WRITE(*,*) NMAX_LEN_BUFFER
          CLOSE(10)
          STOP
        END IF
        !volvemos a abrir el fichero, saltando las lineas iniciales
        OPEN(10,FILE=DATMODELFILE(NMODEL),STATUS='OLD',FORM='FORMATTED')
        IF(NSKIP.GT.0)THEN
          DO I=1,NSKIP
            READ(10,*)
          END DO
        END IF
        !......................................................................
        I=0
20      I=I+1 !..........numero de líneas efectivas (con valores útiles) leidas
        !calculamos qué parametros esperamos
        DO J=1,NPARAMETERS
          IF(J.EQ.1)THEN
            NUMTEMP=1
          ELSE
            NUMTEMP=NUMTEMP*LEN_PARAMETER(J-1)
          END IF
          KEXPECTED(J)=MOD((I-1)/NUMTEMP,LEN_PARAMETER(J))+1
        END DO
22      READ(10,101,ERR=997,END=996) CLONGLINE
        !si la linea contiene tabuladores, los eliminamos
        IF(INDEX(CLONGLINE,CHAR(9)).NE.0) CALL CLEANTAB(CLONGLINE)
        !si alguno de los parametros leidos no coincide con lo esperado,
        !nos saltamos la linea leida y pasamos a la siguiente
        DO J=1,NPARAMETERS
          FDUM_XPARAMETER(J)=FEXTRAE(CLONGLINE,J,ISTATUS,
     +     CBACKUP_LONGLINE)
          IF(ISTATUS.NE.1)THEN
            L1=TRUEBEG(DATMODELFILE(NMODEL))
            L2=TRUELEN(DATMODELFILE(NMODEL))
            WRITE(*,100) 'FATAL ERROR while reading '
            WRITE(*,101) DATMODELFILE(NMODEL)(L1:L2)
            CLOSE(10)
            WRITE(*,101) 'ERROR: while calling FEXTRAE with'
            WRITE(*,100) 'J='
            WRITE(*,*) J
            WRITE(*,100) 'CLONGLINE='
            L1=TRUEBEG(CLONGLINE)
            L2=TRUELEN(CLONGLINE)
            WRITE(*,101) CLONGLINE(L1:L2)
            STOP
          END IF
          IF(FDUM_XPARAMETER(J)-XPARAMETER(KEXPECTED(J),J).NE.0.0)THEN
            GOTO 22
          END IF
        END DO
        !......................................................................
        !leemos los indices que no son compuestos
        NCOLUMN=NPARAMETERS
        DO J=1,NINDICES
          IF(COMP_INDEX(0,J).EQ.0)THEN
            NCOLUMN=NCOLUMN+1
            XINDICES(I,J)=FEXTRAE(CLONGLINE,NCOLUMN,ISTATUS,
     +       CBACKUP_LONGLINE)
          END IF
        END DO
        !ahora calculamos los indices que sean compuestos (ya hemos leido
        !todos los que no lo son, asi que no deberia faltarnos ninguno)
        DO J=1,NINDICES
          IF(COMP_INDEX(0,J).NE.0)THEN
            IF(COMP_INDEX(0,J).EQ.1)THEN !<Fe>
              FE5270=XINDICES(I,COMP_INDEX(1,J))
              FE5335=XINDICES(I,COMP_INDEX(2,J))
              XINDICES(I,J)=0.5*(FE5270+FE5335)
            ELSEIF(COMP_INDEX(0,J).EQ.2)THEN !<Fe>'
              FE5270=XINDICES(I,COMP_INDEX(1,J))
              FE5335=XINDICES(I,COMP_INDEX(2,J))
              XINDICES(I,J)=0.72*FE5270+0.28*FE5335
            ELSEIF(COMP_INDEX(0,J).EQ.3)THEN ![MgFe]
              MGB=XINDICES(I,COMP_INDEX(1,J))
              FE5270=XINDICES(I,COMP_INDEX(2,J))
              FE5335=XINDICES(I,COMP_INDEX(3,J))
              XINDICES(I,J)=SQRT(MGB*0.5*(FE5270+FE5335))
            ELSEIF(COMP_INDEX(0,J).EQ.4)THEN ![MgFe]'
              MGB=XINDICES(I,COMP_INDEX(1,J))
              FE5270=XINDICES(I,COMP_INDEX(2,J))
              FE5335=XINDICES(I,COMP_INDEX(3,J))
              XINDICES(I,J)=SQRT(MGB*(0.72*FE5270+0.28*FE5335))
            ELSE
              WRITE(*,101) 'FATAL ERROR: unexpected COMP_INDEX value'
              WRITE(*,100) 'COMP_INDEX='
              WRITE(*,*) COMP_INDEX
              CLOSE(10)
              STOP
            END IF
          END IF
        END DO
        IF(I.LT.NLINES_TO_BE_READ) GOTO 20
        CLOSE(10)
C------------------------------------------------------------------------------
C Elegimos parametros variables
30      DO I=1,2
          IF(NPARAM(I).EQ.0)THEN
            CDUMMY='@'
          ELSE
            WRITE(CDUMMY,*) NPARAM(I)
          END IF
          IF(I.EQ.1)THEN
            NPARAM(I)=READILIM(IDLOG,'Number of 1st free parameter',
     +       CDUMMY,1,NPARAMETERS)
            WRITE(77,111) NPARAM(I),'# number of 1st free parameter'
          ELSE
            NPARAM(I)=READILIM(IDLOG,'Number of 2nd free parameter',
     +       CDUMMY,1,NPARAMETERS+NPARAMETERS_EXT)
            WRITE(77,111) NPARAM(I),'# number of 2nd free parameter'
          END IF
        END DO
        IF(NPARAM(1).EQ.NPARAM(2))THEN
          WRITE(*,101) 'ERROR: the two parameters must be different.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(IDLOG.EQ.0)THEN
            READ(*,*)
          ELSE
            READ(IDLOG,*)
          END IF
          WRITE(77,*)
          GOTO 30
        END IF
        IF(NPARAM(1).GT.NPARAM(2))THEN !tienen que estar en orden para que
          NPARAM_TEMP=NPARAM(1)        !funcione RETURN_ARRAYS
          NPARAM(1)=NPARAM(2)
          NPARAM(2)=NPARAM_TEMP
        END IF
C------------------------------------------------------------------------------
C Elegimos parametros fijos
        IF(NPARAMETERS+NPARAMETERS_EXT.GT.2)THEN
          WRITE(*,*)
          DO I=1,NPARAMETERS
            IF((I.NE.NPARAM(1)).AND.(I.NE.NPARAM(2)))THEN
              L1=TRUEBEG(CPARAMETER(I))
              L2=TRUELEN(CPARAMETER(I))
              DO J=1,LEN_PARAMETER(I)
                WRITE(*,100) CPARAMETER(I)(L1:L2)
                WRITE(*,'(A2,I3.3,A2,$)') ' #',J,'> '
                WRITE(*,*) XPARAMETER(J,I)
              END DO
              IF(NFIXED(I).EQ.0)THEN
                CDUMMY='@'
              ELSE
                WRITE(CDUMMY,*) NFIXED(I)
              END IF
              IF(INDEX(CPARAMETER(I),':').NE.0)THEN
                L2=INDEX(CPARAMETER(I),':')-1
              END IF
              NFIXED(I)=READILIM(IDLOG,'Number of fixed value for '//
     +         CPARAMETER(I)(L1:L2),CDUMMY,1,LEN_PARAMETER(I))
              WRITE(77,111) NFIXED(I),'# number of fixed value for '//
     +         CPARAMETER(I)(L1:L2)
            ELSE
              NFIXED(I)=0
            END IF
          END DO
        END IF
C------------------------------------------------------------------------------
C Elegimos indices
        WRITE(*,*)
        DO I=1,NINDICES
          WRITE(*,'(A6,I3.3,A2,$)') 'Index#',I,'> '
          L1=TRUEBEG(CINDEX(I))
          L2=TRUELEN(CINDEX(I))
          IF((CINDEX(I)(L1:L1).EQ.'@').AND.(L2.GT.L1)) L1=L1+1
          WRITE(*,101) CINDEX(I)(L1:L2)
        END DO
50      DO I=1,2
          IF(NINDEX(I).EQ.0)THEN
            CDUMMY='@'
          ELSE
            WRITE(CDUMMY,*) NINDEX(I)
          END IF
52        IF(I.EQ.1)THEN
            NINDEX(I)=READILIM(IDLOG,'Number of 1st index',CDUMMY,
     +       1,NINDICES)
            WRITE(77,111) NINDEX(I),'# number of 1st index'
          ELSE
            NINDEX(I)=READILIM(IDLOG,'Number of 2nd index',CDUMMY,
     +       1,NINDICES)
            WRITE(77,111) NINDEX(I),'# number of 2nd index'
          END IF
          CALL INDEXINFO(IDLOG,CINDEX(NINDEX(I)),
     +     CLABEL(I),ITI(I),FERR(I),DWC(I),CUNITS(I),LLOG(I),LREV(I))
          IF((DWC(I).LE.0.0).AND.(.NOT.LLOG(I)))THEN
            WRITE(*,100) 'ERROR: DWC.LE.0.0 for index '
            L1=TRUEBEG(CINDEX(NINDEX(I)))
            L2=TRUELEN(CINDEX(NINDEX(I)))
            WRITE(*,100) CINDEX(NINDEX(I))(L1:L2)
            WRITE(*,101) ' and the scale is not logarithmic.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            GOTO 52
          END IF
        END DO
        IF(NINDEX(1).EQ.NINDEX(2))THEN
          WRITE(*,101) 'ERROR: the two indices must be different.'
          WRITE(*,100) 'Press <CR> to continue...'
          IF(IDLOG.EQ.0)THEN
            READ(*,*)
          ELSE
            READ(IDLOG,*)
          END IF
          WRITE(77,*)
          GOTO 50
        END IF
C------------------------------------------------------------------------------
        CALL RETURN_ARRAYS(NPARAMETERS,
!    +   NPARAMETERS_EXT,
     +   EXTMODELFILE(NMODEL),NLINES_TO_BE_READ)
        CALL PLOT_DIAGRAM(IDLOG)
        LIAUTO=.TRUE.
C------------------------------------------------------------------------------
90      WRITE(*,*)
        WRITE(*,101) '==========================================='
        L1=TRUEBEG(CMODELNAME)
        L2=TRUELEN(CMODELNAME)
        WRITE(*,101) '>>> '//CMODELNAME(L1:L2)
        WRITE(*,101) '-------------------------------------------'
        WRITE(*,101) '[P] Plot whole index-index diagram'
        WRITE(*,101) '[R] Plot partial index-index diagram'
        WRITE(*,100) '[1] Plot parameter#1 labels...'
        IF(LABPAR(1))THEN
          WRITE(*,101) ' ON'
        ELSE
          WRITE(*,101) ' OFF'
        END IF
        WRITE(*,100) '[2] Plot parameter#2 labels...'
        IF(LABPAR(2))THEN
          WRITE(*,101) ' ON'
        ELSE
          WRITE(*,101) ' OFF'
        END IF
        WRITE(*,100) '[L] Automatic limits..........'
        IF(LIAUTO)THEN
          WRITE(*,101) ' ON'
        ELSE
          WRITE(*,101) ' OFF'
        END IF
        WRITE(*,101) '-------------------------------------------'
        WRITE(*,101) '[S] Compute sensitivity parameters'
        WRITE(*,101) '[C] Estimate errors in parameters'
        WRITE(*,101) '[?] Derive parameters for 1 arbitrary point'
        WRITE(*,101) '[I] Include and plot data from ASCII file'
        WRITE(*,101) '-------------------------------------------'
        CCHAR(1:1)=READC(IDLOG,'Menu option (X=restart,Q=quit)','@',
     +   'pPrRiI12lLsScCxXqQ?')
        IF((CCHAR.NE.'q').AND.(CCHAR.NE.'Q'))THEN
          WRITE(77,112) CCHAR,'# Menu option'
        END IF
C..............................................................................
        IF((CCHAR.EQ.'x').OR.(CCHAR.EQ.'X'))THEN
          GOTO 10
C..............................................................................
        ELSEIF((CCHAR.EQ.'q').OR.(CCHAR.EQ.'Q'))THEN
          IF(IDLOG.NE.0)THEN
            CCONT(1:1)=READC(0, !ojo, aqui tiene que se "cero"
     +       'Do you want to continue executing rmodel (y/n)','y','yn')
            IF(CCONT.EQ.'y')THEN
              CLOSE(IDLOG)
              IDLOG=0
              GOTO 90
            ELSE
              WRITE(77,112) CCHAR,'# Menu option'
            END IF
          ELSE
            WRITE(77,112) CCHAR,'# Menu option'
          END IF
          GOTO 999
C..............................................................................
        ELSEIF((CCHAR.EQ.'p').OR.(CCHAR.EQ.'P'))THEN
          LIMIASK=.FALSE.
          LIAUTO_OLD=LIAUTO
          LIAUTO=.FALSE.
          CALL PLOT_DIAGRAM(IDLOG)
          LIAUTO=LIAUTO_OLD
C..............................................................................
        ELSEIF((CCHAR.EQ.'r').OR.(CCHAR.EQ.'R'))THEN
          LIMIASK=.TRUE.
          LIAUTO_OLD=LIAUTO
          LIAUTO=.FALSE.
          CALL PLOT_DIAGRAM(IDLOG)
          LIAUTO=LIAUTO_OLD
C..............................................................................
        ELSEIF((CCHAR.EQ.'i').OR.(CCHAR.EQ.'I'))THEN
!         CALL PLOT_DIAGRAM(IDLOG)
          CALL INCLUDE_EXTERNAL_DATA(IDLOG,0)
          IF(NOBJPLOT.EQ.0) GOTO 90
C..............................................................................
        ELSEIF(CCHAR.EQ.'1')THEN
          IF(LABPAR(1))THEN
            LABPAR(1)=.FALSE.
          ELSE
            LABPAR(1)=.TRUE.
          END IF
C..............................................................................
        ELSEIF(CCHAR.EQ.'2')THEN
          IF(LABPAR(2))THEN
            LABPAR(2)=.FALSE.
          ELSE
            LABPAR(2)=.TRUE.
          END IF
C..............................................................................
        ELSEIF((CCHAR.EQ.'l').OR.(CCHAR.EQ.'L'))THEN
          IF(LIAUTO)THEN
            LIAUTO=.FALSE.
          ELSE
            LIAUTO=.TRUE.
          END IF
        END IF
C..............................................................................
        IF(INDEX('iIsScC?',CCHAR).EQ.0) GOTO 90
        LNOT_ENOUGH=.FALSE.
        DO I=1,2
          IF(LEN_PARAMETER(NPARAM(I)).LT.2)THEN
            WRITE(*,*)
            WRITE(*,100) 'ERROR: insufficient range in '
            L1=TRUEBEG(CPARAMETER(NPARAM(I)))
            L2=TRUELEN(CPARAMETER(NPARAM(I)))
            WRITE(*,101) CPARAMETER(NPARAM(I))(L1:L2)
            LNOT_ENOUGH=.TRUE.
            WRITE(*,101) 'Press <CR> to continue...'
            IF(IDLOG.EQ.0)THEN
              READ(*,*)
            ELSE
              READ(IDLOG,*)
            END IF
            WRITE(77,101) ' '
          END IF
        END DO
        IF(LNOT_ENOUGH) GOTO 90
C..............................................................................
        IF((CCHAR.EQ.'c').OR.(CCHAR.EQ.'C').OR.(CCHAR.EQ.'?'))THEN
          CALL PGSUBP(2,1)
        END IF
C..............................................................................
C si estamos leyendo los puntos del diagrama de un fichero externo,
C vamos a simular que elegimos la opcion '?' para cada objeto
        NCURRENT_OBJECT=1
        IF((CCHAR.EQ.'i').OR.(CCHAR.EQ.'I'))THEN
          WRITE(*,100) '>> No. of objects plotted: '
          WRITE(*,*) NOBJPLOT
          CALLOBJ(1:1)=READC(IDLOG,'Do you want to estimate parameters'
     +     //' for all these objects (y/n) ','n','yn')
          WRITE(77,112) CALLOBJ,'# estimate parameters for all objects?'
          IF(CALLOBJ.EQ.'n') GOTO 90
          L1=TRUEBEG(OBJ_INFILE)
          L2=TRUELEN(OBJ_INFILE)
          CRESULTSFILE=READC(IDLOG,
     +     'Output ASCII file with results',
     +     OBJ_INFILE(L1:L2)//'_rmodel','@')
          CALL TOLOG77_STRING(CRESULTSFILE(1:TRUELEN(CRESULTSFILE)),
     +     'output ASCII file with results')
          OPEN(39,FILE=CRESULTSFILE,STATUS='UNKNOWN',FORM='FORMATTED')
          L39=.TRUE.
          WRITE(39,101) '#'//OBJ_INFILE(L1:L2)
          WRITE(39,100) '#object_name status '
          CALL RSPACE(CPARAMETER(NPARAM(1)),'closest point',CLINEOUT)
          WRITE(39,100) CLINEOUT(1:TRUELEN(CLINEOUT))//' '
          CALL RSPACE(CPARAMETER(NPARAM(2)),'closest point',CLINEOUT)
          WRITE(39,100) CLINEOUT(1:TRUELEN(CLINEOUT))//' '
          CALL RSPACE(CPARAMETER(NPARAM(1)),'linear fit',CLINEOUT)
          WRITE(39,100) CLINEOUT(1:TRUELEN(CLINEOUT))//' '
          CALL RSPACE(CPARAMETER(NPARAM(2)),'linear fit',CLINEOUT)
          WRITE(39,100) CLINEOUT(1:TRUELEN(CLINEOUT))//' '
          CALL RSPACE(CPARAMETER(NPARAM(1)),'bivar. fit',CLINEOUT)
          WRITE(39,100) CLINEOUT(1:TRUELEN(CLINEOUT))//' '
          CALL RSPACE(CPARAMETER(NPARAM(2)),'bivar. fit',CLINEOUT)
          WRITE(39,101) CLINEOUT(1:TRUELEN(CLINEOUT))
          DO I=1,255
            CLINEA39(I:I)=' ' !linea en la que almacenamos los resultados
          END DO
          CPAUSE(1:1)=READC(IDLOG,
     +     'Pause between plots (y/n/k=skip plots) ',CPAUSE,'ynk')
          WRITE(77,112) CPAUSE,'# pause between plots?'
          LEXTERNAL=.TRUE.
          CCHAR='?'
          IF(CPAUSE.NE.'k') CALL PGSUBP(2,1)
        ELSE
          LEXTERNAL=.FALSE.
        END IF
C..............................................................................
C si es un punto, lo introducimos y buscamos lineas isoparametricas para el
C mejor ajuste
91      IF(CPAUSE.NE.'k') CALL PLOT_DIAGRAM(IDLOG)
        IF(CCHAR.EQ.'?')THEN
          OBJSTATUS=0 !salvo que se demuestre lo contrario
          !pedimos el punto con su correspondiente error
          IF(LEXTERNAL)THEN
            INDEX_X=XOBJ(NCURRENT_OBJECT)
            EINDEX_X=EXOBJ(NCURRENT_OBJECT)
            INDEX_Y=YOBJ(NCURRENT_OBJECT)
            EINDEX_Y=EYOBJ(NCURRENT_OBJECT)
          ELSE
            L1=TRUEBEG(CINDEX(NINDEX(1)))
            L2=TRUELEN(CINDEX(NINDEX(1)))
            INDEX_X=READF(IDLOG,CINDEX(NINDEX(1))(L1:L2)//' value','@')
            WRITE(77,*) INDEX_X,'# X index value'
            EINDEX_X=READF(IDLOG,CINDEX(NINDEX(1))(L1:L2)//' error','@')
            WRITE(77,*) EINDEX_X,'# error in X index'
            L1=TRUEBEG(CINDEX(NINDEX(2)))
            L2=TRUELEN(CINDEX(NINDEX(2)))
            INDEX_Y=READF(IDLOG,CINDEX(NINDEX(2))(L1:L2)//' value','@')
            WRITE(77,*) INDEX_Y,'# Y index value'
            EINDEX_Y=READF(IDLOG,CINDEX(NINDEX(2))(L1:L2)//' error','@')
            WRITE(77,*) EINDEX_Y,'# error in Y index'
          END IF
          !dibujamos el punto
          IF(CPAUSE.NE.'k')THEN
            CALL PGSCI(5)
            CALL PGPOINT(1,INDEX_X,INDEX_Y,17)
            CALL PGERR1(1,INDEX_X,INDEX_Y,EINDEX_X,1.0)
            CALL PGERR1(2,INDEX_X,INDEX_Y,EINDEX_Y,1.0)
            CALL PGERR1(3,INDEX_X,INDEX_Y,EINDEX_X,1.0)
            CALL PGERR1(4,INDEX_X,INDEX_Y,EINDEX_Y,1.0)
            CALL PGSCI(1)
          END IF
          !determinamos metodo para buscar punto mas proximo en los modelos
          IF(NCURRENT_OBJECT.EQ.1)THEN
            WRITE(*,101) '(1) use closest point'
            WRITE(*,101) '(2) determine best polygon and find closest'//
     +       ' point'
            WRITE(*,101) '(3) determine best polygon and find closest'//
     +       ' point (renorm. X and Y ranges)'
            WRITE(*,101) 'Note: if point is outside model grid, '//
     +       'option (1) is employed'
            CFPOINT(1:1)=READC(IDLOG,
     +       'Method to find best polygon (1/2/3) ',CFPOINT,'123')
            WRITE(77,112) CFPOINT,'# method to find best polygon'
          END IF
          IF(CFPOINT.EQ.'3')THEN
            LRENORM=.TRUE.
          ELSE
            LRENORM=.FALSE.
          END IF
          IPAR1_MIN=0
          IPAR2_MIN=0
          IF((CFPOINT.EQ.'2').OR.(CFPOINT.EQ.'3'))THEN
            !recorremos todos los "rombos" de los modelos
            DO IPAR1=1,LEN_PARAMETER(NPARAM(1))-1
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))-1
                LINSIDE_ROMBO=INSIDE_ROMBO(INDEX_X,INDEX_Y,
     +           ARRAY1(IPAR1,IPAR2),ARRAY2(IPAR1,IPAR2),
     +           ARRAY1(IPAR1,IPAR2+1),ARRAY2(IPAR1,IPAR2+1),
     +           ARRAY1(IPAR1+1,IPAR2+1),ARRAY2(IPAR1+1,IPAR2+1),
     +           ARRAY1(IPAR1+1,IPAR2),ARRAY2(IPAR1+1,IPAR2),
     +           LRENORM,IBEST)
                IF(LINSIDE_ROMBO)THEN
                  IF(IBEST.EQ.1)THEN
                    IPAR1_MIN=IPAR1
                    IPAR2_MIN=IPAR2
                  ELSEIF(IBEST.EQ.2)THEN
                    IPAR1_MIN=IPAR1
                    IPAR2_MIN=IPAR2+1
                  ELSEIF(IBEST.EQ.3)THEN
                    IPAR1_MIN=IPAR1+1
                    IPAR2_MIN=IPAR2+1
                  ELSEIF(IBEST.EQ.4)THEN
                    IPAR1_MIN=IPAR1+1
                    IPAR2_MIN=IPAR2
                  ELSE
                    WRITE(*,101) 'FATAL ERROR: invalid IBEST value'
                    WRITE(*,100) 'IBEST='
                    WRITE(*,*) IBEST
                    STOP
                  END IF
                  IF(CPAUSE.NE.'k') CALL PGSCI(3)
                ELSE
                  IF(CPAUSE.NE.'k') CALL PGSCI(4)
                END IF
                IF(CPAUSE.NE.'k')THEN
                  CALL PGMOVE(ARRAY1(IPAR1,IPAR2),ARRAY2(IPAR1,IPAR2))
                  CALL PGDRAW(ARRAY1(IPAR1,IPAR2+1),
     +             ARRAY2(IPAR1,IPAR2+1))
                  CALL PGDRAW(ARRAY1(IPAR1+1,IPAR2+1),
     +             ARRAY2(IPAR1+1,IPAR2+1))
                  CALL PGDRAW(ARRAY1(IPAR1+1,IPAR2),
     +             ARRAY2(IPAR1+1,IPAR2))
                  CALL PGDRAW(ARRAY1(IPAR1,IPAR2),ARRAY2(IPAR1,IPAR2))
                END IF
              END DO
            END DO
          END IF
          IF((IPAR1_MIN.EQ.0).OR.(IPAR2_MIN.EQ.0))THEN
            IF((CFPOINT.EQ.'2').OR.(CFPOINT.EQ.'3'))THEN
              WRITE(*,*)
              WRITE(*,101) 'WARNING: point outside model!'
              WRITE(*,101) '>>> finding closest point in any polygon'
              OBJSTATUS=1
            END IF
            !buscamos el punto del modelo mas proximo al introducido (sin
            !tener en cuenta si los recorridos en X e Y son diferentes!)
            DIST_MIN=0.0 !evita un warning de compilacion
            DO IPAR1=1,LEN_PARAMETER(NPARAM(1))
              DO IPAR2=1,LEN_PARAMETER(NPARAM(2))
                IF((IPAR1.EQ.1).AND.(IPAR2.EQ.1))THEN
                  DIST_MIN=(ARRAY1(IPAR1,IPAR2)-INDEX_X)**2+
     +                     (ARRAY2(IPAR1,IPAR2)-INDEX_Y)**2
                  IPAR1_MIN=1
                  IPAR2_MIN=1
                ELSE
                  DIST_TMP=(ARRAY1(IPAR1,IPAR2)-INDEX_X)**2+
     +                     (ARRAY2(IPAR1,IPAR2)-INDEX_Y)**2
                  IF(DIST_TMP.LT.DIST_MIN)THEN
                    DIST_MIN=DIST_TMP
                    IPAR1_MIN=IPAR1
                    IPAR2_MIN=IPAR2
                  END IF
                END IF
              END DO
            END DO
          END IF
        ELSE
          !select fixed 1st parameter
          IF(IPAR1_MIN.EQ.0)THEN
            CDUMMY='@'
          ELSE
            WRITE(CDUMMY,*) IPAR1_MIN
          END IF
          IPAR1_MIN=READILIM(IDLOG,'Number of 1st parameter line',
     +     CDUMMY,1,LEN_PARAMETER(NPARAM(1)))
          WRITE(77,111) IPAR1_MIN,'# number of 1st parameter line'
          !select fixed 2nd parameter
          IF(IPAR2_MIN.EQ.0)THEN
            CDUMMY='@'
          ELSE
            WRITE(CDUMMY,*) IPAR2_MIN
          END IF
          IPAR2_MIN=READILIM(IDLOG,'Number of 2nd parameter line',
     +     CDUMMY,1,LEN_PARAMETER(NPARAM(2)))
          WRITE(77,111) IPAR2_MIN,'# Number of 2nd parameter line'
        END IF
        FPAR1_MIN=XPARAMETER(IPAR1_MIN,NPARAM(1))
        FPAR2_MIN=XPARAMETER(IPAR2_MIN,NPARAM(2))
        !determina el punto de cruce
        INDX0=ARRAY1(IPAR1_MIN,IPAR2_MIN)
        INDY0=ARRAY2(IPAR1_MIN,IPAR2_MIN)
        IF(CPAUSE.NE.'k')THEN
          CALL PGSCI(5)
          CALL PGPOINT(1,INDX0,INDY0,17)
          CALL PGSCI(1)
        END IF
        IF(CCHAR.NE.'?')THEN
          INDEX_X=INDX0
          INDEX_Y=INDY0
        END IF
C..............................................................................
C compute local linear transformation
        !hacemos un cambio de variables para reutilizar el codigo del antiguo
        !programa rmodel.......................................................
        N1=LEN_PARAMETER(NPARAM(2)) !si, aqui los indices 1 y 2 estan cambiados
        DO I=1,N1
          W1(I)=XPARAMETER(I,NPARAM(2))
          X1(I)=ARRAY1(IPAR1_MIN,I)
          Y1(I)=ARRAY2(IPAR1_MIN,I)
        END DO
        N2=LEN_PARAMETER(NPARAM(1)) !si, aqui los indices 2 y 1 estan cambiados
        DO I=1,N2
          W2(I)=XPARAMETER(I,NPARAM(1))
          X2(I)=ARRAY1(I,IPAR2_MIN)
          Y2(I)=ARRAY2(I,IPAR2_MIN)
        END DO
        II=IPAR2_MIN
        JJ=IPAR1_MIN
        LXLOG=LLOG(1)
        LYLOG=LLOG(2)
        DWCX=DWC(1)
        DWCY=DWC(2)
        FERRX=FERR(1)
        FERRY=FERR(2)
        !......................................................................
        IF(II.EQ.1)THEN !solo un punto (el posterior)
          A11=(X1(II+1)-X1(II))/(W1(II+1)-W1(II))
          A21=(Y1(II+1)-Y1(II))/(W1(II+1)-W1(II))
        ELSEIF(II.EQ.N1)THEN !solo un punto (el anterior)
          A11=(X1(II-1)-X1(II))/(W1(II-1)-W1(II))
          A21=(Y1(II-1)-Y1(II))/(W1(II-1)-W1(II))
        ELSE !dos puntos (anterior y posterior)
          A11_1=(X1(II+1)-X1(II))/(W1(II+1)-W1(II))
          A11_2=(X1(II-1)-X1(II))/(W1(II-1)-W1(II))
          A11=(A11_1+A11_2)/2.
          A21_1=(Y1(II+1)-Y1(II))/(W1(II+1)-W1(II))
          A21_2=(Y1(II-1)-Y1(II))/(W1(II-1)-W1(II))
          A21=(A21_1+A21_2)/2.
        END IF
        IF(JJ.EQ.1)THEN !solo un punto (el posterior)
          A12=(X2(JJ+1)-X2(JJ))/(W2(JJ+1)-W2(JJ))
          A22=(Y2(JJ+1)-Y2(JJ))/(W2(JJ+1)-W2(JJ))
        ELSEIF(JJ.EQ.N2)THEN !solo un punto (el anterior)
          A12=(X2(JJ-1)-X2(JJ))/(W2(JJ-1)-W2(JJ))
          A22=(Y2(JJ-1)-Y2(JJ))/(W2(JJ-1)-W2(JJ))
        ELSE !dos puntos (anterior y posterior)
          A12_1=(X2(JJ+1)-X2(JJ))/(W2(JJ+1)-W2(JJ))
          A12_2=(X2(JJ-1)-X2(JJ))/(W2(JJ-1)-W2(JJ))
          A12=(A12_1+A12_2)/2.
          A22_1=(Y2(JJ+1)-Y2(JJ))/(W2(JJ+1)-W2(JJ))
          A22_2=(Y2(JJ-1)-Y2(JJ))/(W2(JJ-1)-W2(JJ))
          A22=(A22_1+A22_2)/2.
        END IF
        X0=X1(II)-A11*W1(II)-A12*W2(JJ)
        Y0=Y1(II)-A21*W1(II)-A22*W2(JJ)
c plot local linear approximation (solo si no estamos en opcion 'C')
        IF(((CCHAR.EQ.'S').OR.(CCHAR.EQ.'s').OR.(CCHAR.EQ.'?')).AND.
     +   (CPAUSE.NE.'k'))THEN
          CALL PGSCI(7)
          IF((II.LT.N1).AND.(JJ.LT.N2))THEN
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGMOVE(XX,YY)
            XX=A11*W1(II+1)+A12*W2(JJ)+X0
            YY=A21*W1(II+1)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II+1)+A12*W2(JJ+1)+X0
            YY=A21*W1(II+1)+A22*W2(JJ+1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ+1)+X0
            YY=A21*W1(II)+A22*W2(JJ+1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
          END IF
          IF((II.GT.1).AND.(JJ.LT.N2))THEN
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGMOVE(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ+1)+X0
            YY=A21*W1(II)+A22*W2(JJ+1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II-1)+A12*W2(JJ+1)+X0
            YY=A21*W1(II-1)+A22*W2(JJ+1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II-1)+A12*W2(JJ)+X0
            YY=A21*W1(II-1)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
          END IF
          IF((II.GT.1).AND.(JJ.GT.1))THEN
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGMOVE(XX,YY)
            XX=A11*W1(II-1)+A12*W2(JJ)+X0
            YY=A21*W1(II-1)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II-1)+A12*W2(JJ-1)+X0
            YY=A21*W1(II-1)+A22*W2(JJ-1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ-1)+X0
            YY=A21*W1(II)+A22*W2(JJ-1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
          END IF
          IF((II.LT.N1).AND.(JJ.GT.1))THEN
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGMOVE(XX,YY)
            XX=A11*W1(II+1)+A12*W2(JJ)+X0
            YY=A21*W1(II+1)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II+1)+A12*W2(JJ-1)+X0
            YY=A21*W1(II+1)+A22*W2(JJ-1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ-1)+X0
            YY=A21*W1(II)+A22*W2(JJ-1)+Y0
            CALL PGDRAW(XX,YY)
            XX=A11*W1(II)+A12*W2(JJ)+X0
            YY=A21*W1(II)+A22*W2(JJ)+Y0
            CALL PGDRAW(XX,YY)
          END IF
          CALL PGSCI(1)
        END IF
C display fitted parameters
        IF(LXLOG)THEN
          RHO_X=1.0
        ELSE !coeficiente cuando la escala no es logaritmica
          RHO_X=(DWCX-INDX0)/(2.5*ALOG10(EXP(1.0)))
        END IF
        IF(LYLOG)THEN
          RHO_Y=1.0
        ELSE !coeficiente cuando la escala no es logaritmica
          RHO_Y=(DWCY-INDY0)/(2.5*ALOG10(EXP(1.0)))
        END IF
        !OJO: a partir de aqui modificamos A11,A12,A21 y A22, por lo que los
        !coeficientes NO SIRVEN para dibujar (salvo que los indices esten
        !en escala logaritmica en los dos ejes, es decir, RHO_X=RHO_Y=1.0).
        !De todas formas los copiamos en A??_LINEAR para ser usados mas
        !abajo.
        A11_LINEAR=A11
        A12_LINEAR=A12
        A21_LINEAR=A21
        A22_LINEAR=A22
        DETA_LINEAR=A11*A22-A12*A21
        A11=A11/RHO_X
        A12=A12/RHO_X
        A21=A21/RHO_Y
        A22=A22/RHO_Y
        SIX=A11/A12 !esto no depende de RHO
        SIY=A21/A22 !esto no depende de RHO
        DETA=A11*A22-A12*A21
        FDEGE=FERRX*FERRX*A22*A22*(1.+SIY*SIY)+
     +        FERRY*FERRY*A12*A12*(SIX*SIX+1.)
        FDEGE=SQRT(FDEGE/(DETA*DETA)) !dejar asi para que sea positivo
        FDEGEX=FERRX*FERRX*A22*A22*(1.+SIY*SIY)/(DETA*DETA)
        FDEGEX=SQRT(FDEGEX)
        FDEGEY=FERRY*FERRY*A12*A12*(SIX*SIX+1.)/(DETA*DETA)
        FDEGEY=SQRT(FDEGEY)
        WRITE(*,*)
        L1=TRUEBEG(CINDEX(NINDEX(1)))
        L2=TRUELEN(CINDEX(NINDEX(1)))
        WRITE(*,101) '>>> X-axis index: '//CINDEX(NINDEX(1))(L1:L2)
        L1=TRUEBEG(CINDEX(NINDEX(2)))
        L2=TRUELEN(CINDEX(NINDEX(2)))
        WRITE(*,101) '>>> Y-axis index: '//CINDEX(NINDEX(2))(L1:L2)
        WRITE(*,*)
        WRITE(*,100) '>>> C_err_x, Dlambda_c....:'
        WRITE(*,*) FERRX,DWCX
        WRITE(*,100) '>>> C_err_y, Dlambda_c....:'
        WRITE(*,*) FERRY,DWCY
        WRITE(*,*)
        WRITE(*,100) '>>> X-coordinate of central grid point: '
        WRITE(*,*) INDX0
        WRITE(*,100) '>>> Y-coordinate of central grid point: '
        WRITE(*,*) INDY0
        WRITE(*,*)
        WRITE(*,101) '* Coefficients of linear approximation'
        WRITE(*,101) '======================================'
        WRITE(*,101) '(NOTE: computed at the closest grid point '//
     +   'in a log-log scale)'
        WRITE(*,100) 'a11,a12,a21,a22:'
        WRITE(*,*) A11,A12,A21,A22
        B11=A22/DETA
        B12=-A12/DETA
        B21=-A21/DETA
        B22=A11/DETA
        DETB=B11*B22-B12*B21
        FAREA=FERRX*FERRY*ABS(DETB)
        WRITE(*,100) 'b11,b12,b21,b22:'
        WRITE(*,*) B11,B12,B21,B22
        WRITE(*,100) 'det(A),det(B),(=1/det(A)): '
        WRITE(*,*) DETA,DETB,1.0/DETA
        WRITE(*,100) 'Sx,Sy:'
        WRITE(*,*) SIX,SIY
        WRITE(*,100) 's_par1^2+s_par2^2 (total, X, Y):'
        WRITE(*,*) FDEGE,FDEGEX,FDEGEY
        WRITE(*,100) 'C_err_x*C_err_y, |det(B)|: '
        WRITE(*,*) FERRX*FERRY,ABS(DETB)
        WRITE(*,100) 'Suitability index kappa, log10(kappa): '
        WRITE(*,*) FAREA,ALOG10(FAREA)
C..............................................................................
C
        I0=IPAR1_MIN
        J0=IPAR2_MIN
        !solo en el caso de que tengamos 3 valores de cada parametro, forzamos
        !que en los bordes el centro del grid se desplace hacia el interior
        !de la malla
        IF(LEN_PARAMETER(NPARAM(1)).GT.2)THEN
          IF(I0.EQ.1) I0=2
          IF(I0.EQ.LEN_PARAMETER(NPARAM(1))) 
     +     I0=LEN_PARAMETER(NPARAM(1))-1
        END IF
        IF(LEN_PARAMETER(NPARAM(2)).GT.2)THEN
          IF(J0.EQ.1) J0=2
          IF(J0.EQ.LEN_PARAMETER(NPARAM(2))) 
     +     J0=LEN_PARAMETER(NPARAM(2))-1
        END IF
        NFIT=0
        !punto#1...............................................................
        IF((I0-1.GE.1).AND.(J0-1.GE.1))THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0-1,J0-1)
          YFIT(NFIT)=ARRAY2(I0-1,J0-1)
          UFIT(NFIT)=XPARAMETER(J0-1,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0-1,NPARAM(1))
        END IF 
        !punto#2...............................................................
        IF(I0-1.GE.1)THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0-1,J0)
          YFIT(NFIT)=ARRAY2(I0-1,J0)
          UFIT(NFIT)=XPARAMETER(J0,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0-1,NPARAM(1))
        END IF
        !punto#3...............................................................
        IF((I0-1.GE.1).AND.(J0+1.LE.LEN_PARAMETER(NPARAM(2))))THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0-1,J0+1)
          YFIT(NFIT)=ARRAY2(I0-1,J0+1)
          UFIT(NFIT)=XPARAMETER(J0+1,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0-1,NPARAM(1))
        END IF
        !punto#4...............................................................
        IF(J0-1.GE.1)THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0,J0-1)
          YFIT(NFIT)=ARRAY2(I0,J0-1)
          UFIT(NFIT)=XPARAMETER(J0-1,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0,NPARAM(1))
        END IF
        !punto#5...............................................................
        NFIT=NFIT+1
        XFIT(NFIT)=ARRAY1(I0,J0)
        YFIT(NFIT)=ARRAY2(I0,J0)
        UFIT(NFIT)=XPARAMETER(J0,NPARAM(2))
        VFIT(NFIT)=XPARAMETER(I0,NPARAM(1))
        !punto#6...............................................................
        IF(J0+1.LE.LEN_PARAMETER(NPARAM(2)))THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0,J0+1)
          YFIT(NFIT)=ARRAY2(I0,J0+1)
          UFIT(NFIT)=XPARAMETER(J0+1,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0,NPARAM(1))
        END IF
        !punto#7...............................................................
        IF((I0+1.LE.LEN_PARAMETER(NPARAM(1))).AND.(J0-1.GE.1))THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0+1,J0-1)
          YFIT(NFIT)=ARRAY2(I0+1,J0-1)
          UFIT(NFIT)=XPARAMETER(J0-1,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0+1,NPARAM(1))
        END IF
        !punto#8...............................................................
        IF(I0+1.LE.LEN_PARAMETER(NPARAM(1)))THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0+1,J0)
          YFIT(NFIT)=ARRAY2(I0+1,J0)
          UFIT(NFIT)=XPARAMETER(J0,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0+1,NPARAM(1))
        END IF
        !punto#9...............................................................
        IF((I0+1.LE.LEN_PARAMETER(NPARAM(1))).AND.
     +     (J0+1.LE.LEN_PARAMETER(NPARAM(2))))THEN
          NFIT=NFIT+1
          XFIT(NFIT)=ARRAY1(I0+1,J0+1)
          YFIT(NFIT)=ARRAY2(I0+1,J0+1)
          UFIT(NFIT)=XPARAMETER(J0+1,NPARAM(2))
          VFIT(NFIT)=XPARAMETER(I0+1,NPARAM(1))
        END IF
        !Si solo hay 6 puntos, mantenemos la interpolacion cuadratica y para
        !ello nos inventamos 3 nuevos puntos extrapolando la informacion
        !conocida
        IF(NFIT.EQ.6)THEN
          IF(LEN_PARAMETER(NPARAM(1)).EQ.2)THEN
            IF(I0.EQ.1)THEN
              DO I=1,3
                XFIT(6+I)=XFIT(I)+(XFIT(I)-XFIT(3+I))
                YFIT(6+I)=YFIT(I)+(YFIT(I)-YFIT(3+I))
                UFIT(6+I)=UFIT(I)+(UFIT(I)-UFIT(3+I))
                VFIT(6+I)=VFIT(I)+(VFIT(I)-VFIT(3+I))
              END DO
            ELSE
              DO I=1,3
                XFIT(6+I)=XFIT(3+I)-(XFIT(I)-XFIT(3+I))
                YFIT(6+I)=YFIT(3+I)-(YFIT(I)-YFIT(3+I))
                UFIT(6+I)=UFIT(3+I)-(UFIT(I)-UFIT(3+I))
                VFIT(6+I)=VFIT(3+I)-(VFIT(I)-VFIT(3+I))
              END DO
            END IF
          ELSE
            IF(J0.EQ.1)THEN
              DO I=1,3
                XFIT(6+I)=XFIT(2*I-1)+(XFIT(2*I-1)-XFIT(2*I))
                YFIT(6+I)=YFIT(2*I-1)+(YFIT(2*I-1)-YFIT(2*I))
                UFIT(6+I)=UFIT(2*I-1)+(UFIT(2*I-1)-UFIT(2*I))
                VFIT(6+I)=VFIT(2*I-1)+(VFIT(2*I-1)-VFIT(2*I))
              END DO
            ELSE
              DO I=1,3
                XFIT(6+I)=XFIT(2*I)-(XFIT(2*I-1)-XFIT(2*I))
                YFIT(6+I)=YFIT(2*I)-(YFIT(2*I-1)-YFIT(2*I))
                UFIT(6+I)=UFIT(2*I)-(UFIT(2*I-1)-UFIT(2*I))
                VFIT(6+I)=VFIT(2*I)-(VFIT(2*I-1)-VFIT(2*I))
              END DO
            END IF
          END IF
          NFIT=9
        END IF
C calculamos la transformacion directa e inversa
        IF(NFIT.EQ.9)THEN !ajusta por minimos cuadrados
          CALL TRANSFORM(9,UFIT,VFIT,XFIT,YFIT,2,AIJ_,BIJ_)
          CALL TRANSFORM(9,XFIT,YFIT,UFIT,VFIT,2,AIJ,BIJ)
        ELSEIF(NFIT.EQ.4)THEN !tomamos la aproximacion lineal
          !transformacion directa
          AIJ_(2)=A12_LINEAR
          AIJ_(3)=0.0
          AIJ_(4)=A11_LINEAR
          AIJ_(5)=0.0
          AIJ_(6)=0.0
          AIJ_(1)=ARRAY1(I0,J0)-AIJ_(4)*XPARAMETER(J0,NPARAM(2))
     +                         -AIJ_(2)*XPARAMETER(I0,NPARAM(1))
          BIJ_(2)=A22_LINEAR
          BIJ_(3)=0.0
          BIJ_(4)=A21_LINEAR
          BIJ_(5)=0.0
          BIJ_(6)=0.0
          BIJ_(1)=ARRAY2(I0,J0)-BIJ_(4)*XPARAMETER(J0,NPARAM(2))
     +                         -BIJ_(2)*XPARAMETER(I0,NPARAM(1))
          !transformacion inversa
          DETER=AIJ_(4)*BIJ_(2)-AIJ_(2)*BIJ_(4)
          AIJ(1)=(-AIJ_(1)*BIJ_(2)+BIJ_(1)*AIJ_(2))/DETER
          AIJ(2)=-AIJ_(2)/DETER
          AIJ(3)=0.0
          AIJ(4)=BIJ_(2)/DETER
          AIJ(5)=0.0
          AIJ(6)=0.0
          BIJ(1)=(-AIJ_(4)*BIJ_(1)+BIJ_(4)*AIJ_(1))/DETER
          BIJ(2)=AIJ_(4)/DETER
          BIJ(3)=0.0
          BIJ(4)=-BIJ_(4)/DETER
          BIJ(5)=0.0
          BIJ(6)=0.0
        ELSE
          WRITE(*,100) 'FATAL ERROR: NFIT='
          WRITE(*,*) NFIT
          STOP
        END IF
        A11=AIJ_(4)+AIJ_(5)*FPAR1_MIN+2.*AIJ_(6)*FPAR2_MIN
        A12=AIJ_(2)+AIJ_(5)*FPAR2_MIN+2.*AIJ_(3)*FPAR1_MIN
        A21=BIJ_(4)+BIJ_(5)*FPAR1_MIN+2.*BIJ_(6)*FPAR2_MIN
        A22=BIJ_(2)+BIJ_(5)*FPAR2_MIN+2.*BIJ_(3)*FPAR1_MIN
        IF(LXLOG)THEN
          RHO_X=1.0
        ELSE !coeficiente cuando la escala no es logaritmica
          RHO_X=(DWCX-INDX0)/(2.5*ALOG10(EXP(1.0)))
        END IF
        IF(LYLOG)THEN
          RHO_Y=1.0
        ELSE !coeficiente cuando la escala no es logaritmica
          RHO_Y=(DWCY-INDY0)/(2.5*ALOG10(EXP(1.0)))
        END IF
        !OJO: a partir de aqui modificamos A11,A12,A21 y A22, por lo que los
        !coeficientes NO SIRVEN para dibujar (salvo que los indices esten
        !en escala logaritmica en los dos ejes, es decir, RHO_X=RHO_Y=1.0)
        A11=A11/RHO_X
        A12=A12/RHO_X
        A21=A21/RHO_Y
        A22=A22/RHO_Y
        SIX=A11/A12
        SIY=A21/A22
        DETA=A11*A22-A12*A21
        FDEGE=FERRX*FERRX*A22*A22*(1.+SIY*SIY)+
     +        FERRY*FERRY*A12*A12*(SIX*SIX+1.)
        FDEGE=SQRT(FDEGE/(DETA*DETA))
        FDEGEX=FERRX*FERRX*A22*A22*(1.+SIY*SIY)/(DETA*DETA)
        FDEGEX=SQRT(FDEGEX)
        FDEGEY=FERRY*FERRY*A12*A12*(SIX*SIX+1.)/(DETA*DETA)
        FDEGEY=SQRT(FDEGEY)
        WRITE(*,*)
        WRITE(*,101) '* Coefficients of bivariate polynomial fit'
        WRITE(*,101) '=========================================='
        WRITE(*,101) '(NOTE: computed at the closest grid point '//
     +   'in a log-log scale)'
        WRITE(*,100) 'a11,a12,a21,a22:'
        WRITE(*,*) A11,A12,A21,A22
        B11=A22/DETA
        B12=-A12/DETA
        B21=-A21/DETA
        B22=A11/DETA
        DETB=B11*B22-B12*B21
        FAREA=FERRX*FERRY*ABS(DETB)
        WRITE(*,100) 'b11,b12,b21,b22:'
        WRITE(*,*) B11,B12,B21,B22
        WRITE(*,100) 'det(A), det(B), (=1/det(A)): '
        WRITE(*,*) DETA,DETB,1.0/DETA
        WRITE(*,100) 'Sx,Sy:'
        WRITE(*,*) SIX,SIY
        WRITE(*,100) 's_par1^2+s_par2^2 (total, X, Y):'
        WRITE(*,*) FDEGE,FDEGEX,FDEGEY
        WRITE(*,100) 'C_err_x*C_err_y, |det(B)|: '
        WRITE(*,*) FERRX*FERRY,ABS(DETB)
        WRITE(*,100) 'Suitability index kappa, log10(kappa): '
        WRITE(*,*) FAREA,ALOG10(FAREA)
C idem pero usando la transformacion evaluada en el punto considerado (y no en
C el punto del grid del modelo mas proximo)
        CALL FMAP(2,AIJ,BIJ,INDEX_X,INDEX_Y,FPAR2_FIT,FPAR1_FIT)
        A11=AIJ_(4)+AIJ_(5)*FPAR1_FIT+2.*AIJ_(6)*FPAR2_FIT
        A12=AIJ_(2)+AIJ_(5)*FPAR2_FIT+2.*AIJ_(3)*FPAR1_FIT
        A21=BIJ_(4)+BIJ_(5)*FPAR1_FIT+2.*BIJ_(6)*FPAR2_FIT
        A22=BIJ_(2)+BIJ_(5)*FPAR2_FIT+2.*BIJ_(3)*FPAR1_FIT
        IF(LXLOG)THEN
          RHO_X=1.0
        ELSE !coeficiente cuando la escala no es logaritmica
          RHO_X=(DWCX-INDX0)/(2.5*ALOG10(EXP(1.0)))
        END IF
        IF(LYLOG)THEN
          RHO_Y=1.0
        ELSE !coeficiente cuando la escala no es logaritmica
          RHO_Y=(DWCY-INDY0)/(2.5*ALOG10(EXP(1.0)))
        END IF
        !OJO: a partir de aqui modificamos A11,A12,A21 y A22, por lo que los
        !coeficientes NO SIRVEN para dibujar (salvo que los indices esten
        !en escala logaritmica en los dos ejes, es decir, RHO_X=RHO_Y=1.0)
        A11=A11/RHO_X
        A12=A12/RHO_X
        A21=A21/RHO_Y
        A22=A22/RHO_Y
        SIX=A11/A12 !esto no depende de RHO
        SIY=A21/A22 !esto no depende de RHO
        DETA=A11*A22-A12*A21
        FDEGE=FERRX*FERRX*A22*A22*(1.+SIY*SIY)+
     +        FERRY*FERRY*A12*A12*(SIX*SIX+1.)
        FDEGE=SQRT(FDEGE/(DETA*DETA)) !dejar asi para que sea positivo
        FDEGEX=FERRX*FERRX*A22*A22*(1.+SIY*SIY)/(DETA*DETA)
        FDEGEX=SQRT(FDEGEX)
        FDEGEY=FERRY*FERRY*A12*A12*(SIX*SIX+1.)/(DETA*DETA)
        FDEGEY=SQRT(FDEGEY)
        WRITE(*,*)
        WRITE(*,101) '* Coefficients of bivariate polynomial fit'
        WRITE(*,101) '=========================================='
        WRITE(*,101) '(NOTE: computed at the index-index point '//
     +   'in a log-log scale)'
        WRITE(*,100) 'a11,a12,a21,a22:'
        WRITE(*,*) A11,A12,A21,A22
        B11=A22/DETA
        B12=-A12/DETA
        B21=-A21/DETA
        B22=A11/DETA
        DETB=B11*B22-B12*B21
        FAREA=FERRX*FERRY*ABS(DETB)
        WRITE(*,100) 'b11,b12,b21,b22:'
        WRITE(*,*) B11,B12,B21,B22
        WRITE(*,100) 'det(A), det(B), (=1/det(A)): '
        WRITE(*,*) DETA,DETB,1.0/DETA
        WRITE(*,100) 'Sx,Sy:'
        WRITE(*,*) SIX,SIY
        WRITE(*,100) 's_par1^2+s_par2^2 (total, X, Y):'
        WRITE(*,*) FDEGE,FDEGEX,FDEGEY
        WRITE(*,100) 'C_err_x*C_err_y, |det(B)|: '
        WRITE(*,*) FERRX*FERRY,ABS(DETB)
        WRITE(*,100) 'Suitability index kappa, log10(kappa): '
        WRITE(*,*) FAREA,ALOG10(FAREA)
C dibujamos el resultado (solo si no hacemos estamos en la opcion 'C')
        IF(((CCHAR.EQ.'S').OR.(CCHAR.EQ.'s').OR.(CCHAR.EQ.'?')).AND.
     +   (CPAUSE.NE.'k'))THEN
          CALL PGSCI(6)
          CALL PGSLW(3)
          !---
          IF(J0-1.GE.1)THEN
            XPARAM_MIN=XPARAMETER(J0-1,NPARAM(2))
          ELSE
            XPARAM_MIN=XPARAMETER(J0,NPARAM(2))
          END IF
          IF(J0+1.LE.LEN_PARAMETER(NPARAM(2)))THEN
            XPARAM_MAX=XPARAMETER(J0+1,NPARAM(2))
          ELSE
            XPARAM_MAX=XPARAMETER(J0,NPARAM(2))
          END IF
          IF(I0-1.GE.1)THEN
            DO I=1,NPMAX
              UU=XPARAM_MIN+REAL(I-1)/REAL(NPMAX-1)*
     +         (XPARAM_MAX-XPARAM_MIN)
              VV=XPARAMETER(I0-1,NPARAM(1))
              CALL FMAP(2,AIJ_,BIJ_,UU,VV,XP(I),YP(I))
            END DO
            CALL PGLINE(NPMAX,XP,YP)
          END IF
          !---
          DO I=1,NPMAX
            UU=XPARAM_MIN+REAL(I-1)/REAL(NPMAX-1)*
     +       (XPARAM_MAX-XPARAM_MIN)
            VV=XPARAMETER(I0,NPARAM(1))
            CALL FMAP(2,AIJ_,BIJ_,UU,VV,XP(I),YP(I))
          END DO
          CALL PGLINE(NPMAX,XP,YP)
          !---
          IF(I0+1.LE.LEN_PARAMETER(NPARAM(1)))THEN
            DO I=1,NPMAX
              UU=XPARAM_MIN+REAL(I-1)/REAL(NPMAX-1)*
     +         (XPARAM_MAX-XPARAM_MIN)
              VV=XPARAMETER(I0+1,NPARAM(1))
              CALL FMAP(2,AIJ_,BIJ_,UU,VV,XP(I),YP(I))
            END DO
            CALL PGLINE(NPMAX,XP,YP)
          END IF
          !---
          IF(I0-1.GE.1)THEN
            XPARAM_MIN=XPARAMETER(I0-1,NPARAM(1))
          ELSE
            XPARAM_MIN=XPARAMETER(I0,NPARAM(1))
          END IF
          IF(I0+1.LE.LEN_PARAMETER(NPARAM(1)))THEN
            XPARAM_MAX=XPARAMETER(I0+1,NPARAM(1))
          ELSE
            XPARAM_MAX=XPARAMETER(I0,NPARAM(1))
          END IF
          IF(J0-1.GE.1)THEN
            DO I=1,NPMAX
              UU=XPARAMETER(J0-1,NPARAM(2))
              VV=XPARAM_MIN+REAL(I-1)/REAL(NPMAX-1)*
     +         (XPARAM_MAX-XPARAM_MIN)
              CALL FMAP(2,AIJ_,BIJ_,UU,VV,XP(I),YP(I))
            END DO
            CALL PGLINE(NPMAX,XP,YP)
          END IF
          !---
          DO I=1,NPMAX
            UU=XPARAMETER(J0,NPARAM(2))
            VV=XPARAM_MIN+REAL(I-1)/REAL(NPMAX-1)*
     +       (XPARAM_MAX-XPARAM_MIN)
            CALL FMAP(2,AIJ_,BIJ_,UU,VV,XP(I),YP(I))
          END DO
          CALL PGLINE(NPMAX,XP,YP)
          !---
          IF(J0+1.LE.LEN_PARAMETER(NPARAM(2)))THEN
            DO I=1,NPMAX
              UU=XPARAMETER(J0+1,NPARAM(2))
              VV=XPARAM_MIN+REAL(I-1)/REAL(NPMAX-1)*
     +         (XPARAM_MAX-XPARAM_MIN)
              CALL FMAP(2,AIJ_,BIJ_,UU,VV,XP(I),YP(I))
            END DO
            CALL PGLINE(NPMAX,XP,YP)
          END IF
          !---
          CALL PGSLW(2)
          CALL PGSCI(1)
        END IF
!       write(55,*) idx,idy,SIX,SIY,FERRX,FERRY,FDEGE
!       write(55,*) idx,idy,SIX,SIY,FERRX,FERRY,FAREA
!       write(56,*) INDX0,INDY0,FAREA
C..............................................................................
C Con la transformacion calculada, determinamos el valor de edad y metalicidad
C para el punto introducido por el usuario. 
ccc92      IF(CCHAR.EQ.'?')THEN
        IF(CCHAR.EQ.'?')THEN
          !------------------------------------------------------------------
          WRITE(*,*)
          IF(LEXTERNAL)THEN
            WRITE(CDUMMY,*) NCURRENT_OBJECT
            CALL RMBLANK(CDUMMY,CDUMMY,L1)
            WRITE(*,100) '* OBJECT #'//CDUMMY(1:L1)
            L2=TRUELEN(OBJNAME(NCURRENT_OBJECT))
            IF(L2.GT.0)THEN
              L1=TRUEBEG(OBJNAME(NCURRENT_OBJECT))
              WRITE(*,101) ' <'//OBJNAME(NCURRENT_OBJECT)(L1:L2)//'>'
              IF(L39) CLINEA39(1:50)=OBJNAME(NCURRENT_OBJECT)(L1:L2)
            ELSE
              WRITE(*,101) ' <UNKNOWN>'
              IF(L39) CLINEA39(1:50)='<UNKNOWN>'
            END IF
            IF(L39) WRITE(CLINEA39(52:53),'(I2.2)') OBJSTATUS
          END IF
          WRITE(*,101) '* Summary of linear and bivariate fits'
          WRITE(*,101) '======================================'
          !
          L1=TRUEBEG(CPARAMETER(NPARAM(1)))
          L2=TRUELEN(CPARAMETER(NPARAM(1)))
          WRITE(*,100) '>> '
          WRITE(*,100) CPARAMETER(NPARAM(1))(L1:L2)
          IF(L2-L1.LT.30)THEN
            DO I=1,30-(L2-L1)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) '(closest point)= '
          WRITE(*,*) FPAR1_MIN
          IF(L39) WRITE(CLINEA39(060:080),*) FPAR1_MIN
          !
          L1=TRUEBEG(CPARAMETER(NPARAM(2)))
          L2=TRUELEN(CPARAMETER(NPARAM(2)))
          WRITE(*,100) '>> '
          WRITE(*,100) CPARAMETER(NPARAM(2))(L1:L2)
          IF(L2-L1.LT.30)THEN
            DO I=1,30-(L2-L1)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) '(closest point)= '
          WRITE(*,*) FPAR2_MIN
          IF(L39) WRITE(CLINEA39(090:110),*) FPAR2_MIN
          !
          ERRIX=INDEX_X-INDX0
          ERRIY=INDEX_Y-INDY0
          DELTA_PAR2_=(A22_LINEAR*ERRIX-A12_LINEAR*ERRIY)/
     +     (DETA_LINEAR)
          DELTA_PAR1_=(-A21_LINEAR*ERRIX+A11_LINEAR*ERRIY)/
     +     (DETA_LINEAR)
          !
          WRITE(*,100) '>> '
          L1=TRUEBEG(CPARAMETER(NPARAM(1)))
          L2=TRUELEN(CPARAMETER(NPARAM(1)))
          WRITE(*,100) CPARAMETER(NPARAM(1))(L1:L2)
          IF(L2-L1.LT.33)THEN
            DO I=1,33-(L2-L1)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) '(linear fit)= '
          WRITE(*,*) FPAR1_MIN+DELTA_PAR1_
          IF(L39) WRITE(CLINEA39(120:140),*) FPAR1_MIN+DELTA_PAR1_
          !
          WRITE(*,100) '>> '
          L1=TRUEBEG(CPARAMETER(NPARAM(2)))
          L2=TRUELEN(CPARAMETER(NPARAM(2)))
          WRITE(*,100) CPARAMETER(NPARAM(2))(L1:L2)
          IF(L2-L1.LT.33)THEN
            DO I=1,33-(L2-L1)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) '(linear fit)= '
          WRITE(*,*) FPAR2_MIN+DELTA_PAR2_
          IF(L39) WRITE(CLINEA39(150:170),*) FPAR2_MIN+DELTA_PAR2_
          !
          WRITE(*,100) '>> '
          L1=TRUEBEG(CPARAMETER(NPARAM(1)))
          L2=TRUELEN(CPARAMETER(NPARAM(1)))
          WRITE(*,100) CPARAMETER(NPARAM(1))(L1:L2)
          IF(L2-L1.LT.33)THEN
            DO I=1,33-(L2-L1)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) '(bivar. fit)= '
          WRITE(*,*) FPAR1_FIT
          IF(L39) WRITE(CLINEA39(180:200),*) FPAR1_FIT
          !
          WRITE(*,100) '>> '
          L1=TRUEBEG(CPARAMETER(NPARAM(2)))
          L2=TRUELEN(CPARAMETER(NPARAM(2)))
          WRITE(*,100) CPARAMETER(NPARAM(2))(L1:L2)
          IF(L2-L1.LT.33)THEN
            DO I=1,33-(L2-L1)
              WRITE(*,100) '.'
            END DO
          END IF
          WRITE(*,100) '(bivar. fit)= '
          WRITE(*,*) FPAR2_FIT
          IF(L39) WRITE(CLINEA39(210:230),*) FPAR2_FIT
          !
          WRITE(*,*)
          IF(L39) WRITE(39,101) CLINEA39
          !------------------------------------------------------------------
          IF(NCURRENT_OBJECT.EQ.1)THEN
            WRITE(CDUMMY,*) NSIMUL
            NSIMUL=READILIM(IDLOG,'No. of simulations, 0=none',CDUMMY,
     +       0,NSIMULMAX)
            WRITE(77,111) NSIMUL,'# no. of simulations'
            IF(NSIMUL.EQ.0)THEN
              GOTO 95
            END IF
            IF(NSEED.EQ.-1)THEN
              NSEED=READI(IDLOG,'NSEED for random numbers','-1')
              WRITE(77,111) NSEED,'# NSEED for random numbers'
            END IF
            WRITE(*,*)
            WRITE(*,101) 'Note: the results can be stored in an '//
     +       'ASCII file which will contain'
            WRITE(*,101) '      [Fe/H] and log10[Age(yr)] for the '//
     +       '(index1,index2) previously indicated,'
            WRITE(*,101) '      as well as the corresponding '//
     +       'pseudo-ellipses (1sigma, 2sigma and 3sigma)'
            WRITE(*,101) '      of the errors (each ellipse formed '//
     +       'by 361 pairs of points, from angle 0'
            WRITE(*,101) '      to 360---). All these data points'//
     +       ' are contiguous in the same output file.'
            COUTFILE=READC(IDLOG,'Output file name (NONE=no output)',
     +       'rmodel_simul.out','@')
            L1=TRUEBEG(COUTFILE)
            L2=TRUELEN(COUTFILE)
            CALL TOLOG77_STRING(COUTFILE(L1:L2),
     +       'output file for simulations')
            IF(COUTFILE.EQ.'NONE')THEN
              L14=.FALSE.
            ELSE
              L14=.TRUE.
            END IF
          ELSE
            IF(NSIMUL.EQ.0)THEN
              GOTO 95
            END IF
          END IF
C
          IF(L14)THEN
            IF(LEXTERNAL)THEN
              WRITE(CDUMMY(1:8),'(A4,I4.4)') '_obj',NCURRENT_OBJECT
              L1=TRUEBEG(COUTFILE)
              L2=TRUELEN(COUTFILE)
              OPEN(14,FILE=COUTFILE(L1:L2)//CDUMMY(1:8),
     +         STATUS='UNKNOWN',FORM='FORMATTED')
            ELSE
              OPEN(14,FILE=COUTFILE,STATUS='UNKNOWN',FORM='FORMATTED')
            END IF
            WRITE(14,100) '# Object: '
            IF(LEXTERNAL)THEN
              L2=TRUELEN(OBJNAME(NCURRENT_OBJECT))
              IF(L2.GT.0)THEN
                L1=TRUEBEG(OBJNAME(NCURRENT_OBJECT))
                WRITE(14,101) '<'//OBJNAME(NCURRENT_OBJECT)(L1:L2)//'>'
              ELSE
                WRITE(14,101) '<UNKNOWN>'
              END IF
            ELSE
              WRITE(14,101) '<UNKNOWN>'
            END IF
            WRITE(14,100) '# Model: '
            L1=TRUEBEG(CMODELNAME)
            L2=TRUELEN(CMODELNAME)
            WRITE(14,101) CMODELNAME(L1:L2)
            WRITE(14,100) '# X-axis: '
            L1=TRUEBEG(CLABEL(1))
            L2=TRUELEN(CLABEL(1))
            WRITE(14,101) CLABEL(1)(L1:L2)
            WRITE(14,100) '# Y-axis: '
            L1=TRUEBEG(CLABEL(2))
            L2=TRUELEN(CLABEL(2))
            WRITE(14,101) CLABEL(2)(L1:L2)
            WRITE(14,100) '# Param1: '
            L1=TRUEBEG(CPARAMETER(NPARAM(1)))
            L2=TRUELEN(CPARAMETER(NPARAM(1)))
            WRITE(14,101) CPARAMETER(NPARAM(1))(L1:L2)
            WRITE(14,100) '# Param2: '
            L1=TRUEBEG(CPARAMETER(NPARAM(2)))
            L2=TRUELEN(CPARAMETER(NPARAM(2)))
            WRITE(14,101) CPARAMETER(NPARAM(2))(L1:L2)
            WRITE(14,101) '#Param2        Param1'
            WRITE(14,*) FPAR2_FIT,FPAR1_FIT
          END IF
          IF(CPAUSE.NE.'k') CALL PGSCI(14)
          DO K=1,NSIMUL
            R1=RANRED(NSEED)
            R2=RANRED(NSEED)*6.283185307
            ERRIX=1.4142136*EINDEX_X*SQRT(-ALOG(1.-R1))*COS(R2)
            R1=RANRED(NSEED)
            R2=RANRED(NSEED)*6.283185307
            ERRIY=1.4142136*EINDEX_Y*SQRT(-ALOG(1.-R1))*COS(R2)
            IF(CPAUSE.NE.'k')THEN
              CALL PGPOINT(1,INDEX_X+ERRIX,INDEX_Y+ERRIY,1)
            END IF
            !calculamos edad y metalicidad del nuevo punto usando la
            !transformacion cuadratica
            CALL FMAP(2,AIJ,BIJ,INDEX_X+ERRIX,INDEX_Y+ERRIY,
     +       FPAR2_SIMUL(K),FPAR1_SIMUL(K))
          END DO
          IF(CPAUSE.NE.'k')THEN
            CALL PGSCI(5)
            CALL PGPOINT(1,INDEX_X,INDEX_Y,17)
            CALL PGERR1(1,INDEX_X,INDEX_Y,EINDEX_X,1.0)
            CALL PGERR1(2,INDEX_X,INDEX_Y,EINDEX_Y,1.0)
            CALL PGERR1(3,INDEX_X,INDEX_Y,EINDEX_X,1.0)
            CALL PGERR1(4,INDEX_X,INDEX_Y,EINDEX_Y,1.0)
          END IF
          !recorremos las elipses de 1, 2 y 3 sigma
          IF(CPAUSE.NE.'k') CALL PGSCI(4)
          DO ISIGMA=1,3
            DO K=0,360 !grados
              ERRIX=REAL(ISIGMA)*EINDEX_X*COS(REAL(K)*1.74532925E-2)
              ERRIY=REAL(ISIGMA)*EINDEX_Y*SIN(REAL(K)*1.74532925E-2)
              IF(ISIGMA.EQ.1)THEN
                CALL FMAP(2,AIJ,BIJ,INDEX_X+ERRIX,INDEX_Y+ERRIY,
     +           FPAR2_SIMUL1S(K),FPAR1_SIMUL1S(K))
                IF(L14) WRITE(14,*) FPAR2_SIMUL1S(K),FPAR1_SIMUL1S(K)
              ELSEIF(ISIGMA.EQ.2)THEN
                CALL FMAP(2,AIJ,BIJ,INDEX_X+ERRIX,INDEX_Y+ERRIY,
     +           FPAR2_SIMUL2S(K),FPAR1_SIMUL2S(K))
                IF(L14) WRITE(14,*) FPAR2_SIMUL2S(K),FPAR1_SIMUL2S(K)
              ELSE
                CALL FMAP(2,AIJ,BIJ,INDEX_X+ERRIX,INDEX_Y+ERRIY,
     +           FPAR2_SIMUL3S(K),FPAR1_SIMUL3S(K))
                IF(L14) WRITE(14,*) FPAR2_SIMUL3S(K),FPAR1_SIMUL3S(K)
              END IF
              IF(CPAUSE.NE.'k')THEN
                IF(K.EQ.0)THEN
                  CALL PGMOVE(INDEX_X+ERRIX,INDEX_Y+ERRIY)
                ELSE
                  CALL PGDRAW(INDEX_X+ERRIX,INDEX_Y+ERRIY)
                END IF
              END IF
            END DO
          END DO
          IF(CPAUSE.NE.'k') CALL PGSCI(1)
          IF(L14) CLOSE(14)
          !plot param1 vs. param2
          IF(CPAUSE.NE.'k')THEN
            XMIN_=FPAR2_SIMUL(1)
            XMAX_=XMIN_
            YMIN_=FPAR1_SIMUL(1)
            YMAX_=YMIN_
            DO K=1,NSIMUL
              XMIN_=AMIN1(XMIN_,FPAR2_SIMUL(K))
              XMAX_=AMAX1(XMAX_,FPAR2_SIMUL(K))
              YMIN_=AMIN1(YMIN_,FPAR1_SIMUL(K))
              YMAX_=AMAX1(YMAX_,FPAR1_SIMUL(K))
            END DO
            DX_=XMAX_-XMIN_
            IF(DX_.GT.0.0)THEN
              XMIN_=XMIN_-DX_/20.
              XMAX_=XMAX_+DX_/20.
            ELSE
              XMIN_=FPAR2_SIMUL(1)-0.5
              XMAX_=FPAR2_SIMUL(1)+0.5
            END IF
            DY_=YMAX_-YMIN_
            IF(DY_.GT.0.0)THEN
              YMIN_=YMIN_-DY_/20.
              YMAX_=YMAX_+DY_/20.
            ELSE
              YMIN_=FPAR1_SIMUL(1)-0.5
              YMAX_=FPAR1_SIMUL(1)+0.5
            END IF
            CALL PGENV(XMIN_,XMAX_,YMIN_,YMAX_,0,0)
            CALL PGSCI(14)
            CALL PGPOINT(NSIMUL,FPAR2_SIMUL,FPAR1_SIMUL,1)
            CALL PGSCI(5)
            CALL PGPOINT(1,FPAR2_FIT,FPAR1_FIT,17)
            DO I=1,NPMAX
              XX=INDEX_X+REAL(I-1)/REAL(NPMAX-1)*EINDEX_X
              CALL FMAP(2,AIJ,BIJ,XX,INDEX_Y,FPAR2_P,FPAR1_P)
              IF(I.EQ.1)THEN
                CALL PGMOVE(FPAR2_P,FPAR1_P)
              ELSE
                CALL PGDRAW(FPAR2_P,FPAR1_P)
              END IF
            END DO
            DO I=1,NPMAX
              XX=INDEX_X-REAL(I-1)/REAL(NPMAX-1)*EINDEX_X
              CALL FMAP(2,AIJ,BIJ,XX,INDEX_Y,FPAR2_P,FPAR1_P)
              IF(I.EQ.1)THEN
                CALL PGMOVE(FPAR2_P,FPAR1_P)
              ELSE
                CALL PGDRAW(FPAR2_P,FPAR1_P)
              END IF
            END DO
            DO I=1,NPMAX
              YY=INDEX_Y+REAL(I-1)/REAL(NPMAX-1)*EINDEX_Y
              CALL FMAP(2,AIJ,BIJ,INDEX_X,YY,FPAR2_P,FPAR1_P)
              IF(I.EQ.1)THEN
                CALL PGMOVE(FPAR2_P,FPAR1_P)
              ELSE
                CALL PGDRAW(FPAR2_P,FPAR1_P)
              END IF
            END DO
            DO I=1,NPMAX
              YY=INDEX_Y-REAL(I-1)/REAL(NPMAX-1)*EINDEX_Y
              CALL FMAP(2,AIJ,BIJ,INDEX_X,YY,FPAR2_P,FPAR1_P)
              IF(I.EQ.1)THEN
                CALL PGMOVE(FPAR2_P,FPAR1_P)
              ELSE
                CALL PGDRAW(FPAR2_P,FPAR1_P)
              END IF
            END DO
            CALL PGSCI(4)
            CALL PGLINE(361,FPAR2_SIMUL1S(0),FPAR1_SIMUL1S(0))
            CALL PGLINE(361,FPAR2_SIMUL2S(0),FPAR1_SIMUL2S(0))
            CALL PGLINE(361,FPAR2_SIMUL3S(0),FPAR1_SIMUL3S(0))
            CALL PGSCI(1)
            CALL PGLABEL(CPARAMETER(NPARAM(2)),CPARAMETER(NPARAM(1)),
     +       CMODELNAME)
c lineas del ajuste bivariado (rectas en esta representacion!)
            CALL PGSCI(6)
            IF(J0-1.GE.1)THEN
              XPARAM_MIN=XPARAMETER(J0-1,NPARAM(2))
            ELSE
              XPARAM_MIN=XPARAMETER(J0,NPARAM(2))
            END IF
            IF(J0+1.LE.LEN_PARAMETER(NPARAM(2)))THEN
              XPARAM_MAX=XPARAMETER(J0+1,NPARAM(2))
            ELSE
              XPARAM_MAX=XPARAMETER(J0,NPARAM(2))
            END IF
            IF(I0-1.GE.1)THEN
              VV=XPARAMETER(I0-1,NPARAM(1))
              CALL PGMOVE(XPARAM_MIN,VV)
              CALL PGDRAW(XPARAM_MAX,VV)
            END IF
            !---
            VV=XPARAMETER(I0,NPARAM(1))
            CALL PGMOVE(XPARAM_MIN,VV)
            CALL PGDRAW(XPARAM_MAX,VV)
            !---
            IF(I0+1.LE.LEN_PARAMETER(NPARAM(1)))THEN
              VV=XPARAMETER(I0+1,NPARAM(1))
              CALL PGMOVE(XPARAM_MIN,VV)
              CALL PGDRAW(XPARAM_MAX,VV)
            END IF
            !---
            IF(I0-1.GE.1)THEN
              XPARAM_MIN=XPARAMETER(I0-1,NPARAM(1))
            ELSE
              XPARAM_MIN=XPARAMETER(I0,NPARAM(1))
            END IF
            IF(I0+1.LE.LEN_PARAMETER(NPARAM(1)))THEN
              XPARAM_MAX=XPARAMETER(I0+1,NPARAM(1))
            ELSE
              XPARAM_MAX=XPARAMETER(I0,NPARAM(1))
            END IF
            IF(J0-1.GE.1)THEN
              UU=XPARAMETER(J0-1,NPARAM(2))
              CALL PGMOVE(UU,XPARAM_MIN)
              CALL PGDRAW(UU,XPARAM_MAX)
            END IF
            !---
            UU=XPARAMETER(J0,NPARAM(2))
            CALL PGMOVE(UU,XPARAM_MIN)
            CALL PGDRAW(UU,XPARAM_MAX)
            !---
            IF(J0+1.LE.LEN_PARAMETER(NPARAM(2)))THEN
              UU=XPARAMETER(J0+1,NPARAM(2))
              CALL PGMOVE(UU,XPARAM_MIN)
              CALL PGDRAW(UU,XPARAM_MAX)
            END IF
            CALL PGSCI(1)
C
            IF(.NOT.LEXTERNAL) CALL PGSUBP(1,1)
          END IF
        END IF
C..............................................................................
95      IF(LEXTERNAL)THEN
          IF(CPAUSE.EQ.'y')THEN
            WRITE(*,100) 'Press <RETURN> to continue...'
            IF(IDLOG.EQ.0)THEN
              READ(*,*)
            ELSE
              READ(IDLOG,*)
              WRITE(*,*)
            END IF
            WRITE(77,*)
          END IF
          NCURRENT_OBJECT=NCURRENT_OBJECT+1
          IF(NCURRENT_OBJECT.GT.NOBJPLOT)THEN
            IF(L39)THEN
              CLOSE(39)
              L39=.FALSE.
            END IF
            L1=TRUEBEG(CRESULTSFILE)
            L2=TRUELEN(CRESULTSFILE)
            ISYSTEM=SYSTEMFUNCTION('mv -f '//CRESULTSFILE(L1:L2)//' '//
     +       CRESULTSFILE(L1:L2)//'_xxx')
            ISYSTEM=SYSTEMFUNCTION('cat '//CRESULTSFILE(L1:L2)//
     +       '_xxx | column -t > '//CRESULTSFILE(L1:L2))
            ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +       CRESULTSFILE(L1:L2)//'_xxx')
            IF(CPAUSE.NE.'k')THEN
              CALL PGSUBP(1,1)
              CALL PLOT_DIAGRAM(IDLOG)
            END IF
            CALL INCLUDE_EXTERNAL_DATA(IDLOG,1)
            IF(CPAUSE.EQ.'k') CPAUSE='y'
            GOTO 90
          ELSE
            GOTO 91
          END IF
        END IF
C..............................................................................
C Podemos usar las simulaciones de Monte Carlo para probar que todo funciona
C correctamente
        IF((CCHAR.EQ.'C').OR.(CCHAR.EQ.'c'))THEN
c number of simulations and S/N ratio
          WRITE(CDUMMY,*) NSIMUL
          WRITE(*,*)
          NSIMUL=READILIM(IDLOG,'No. of simulations',CDUMMY,
     +     10,NSIMULMAX)
          WRITE(77,111) NSIMUL,'# no. of simulations'
          IF(NSEED.EQ.-1)THEN
            NSEED=READI(IDLOG,'NSEED for random numbers','-1')
            WRITE(77,111) NSEED,'# NSEED for random numbers'
          END IF
          WRITE(CDUMMY,*) SNRATX
          SNRATX=READF(IDLOG,'S/N per A in X axis',CDUMMY)
          WRITE(77,*) SNRATX,'# S/N per A in X axis'
          WRITE(CDUMMY,*) SNRATY
          SNRATY=READF(IDLOG,'S/N per A in Y axis',CDUMMY)
          WRITE(77,*) SNRATY,'# S/N per A in Y axis'
c simulations: escalamos con RHO_X y RHO_Y para que tambien funcione con
C              indices no medidos en magnitudes
          CALL PGSCI(14)
          DO K=1,NSIMUL
            R1=RANRED(NSEED)
            R2=RANRED(NSEED)*6.283185307
            ERRIX=
     +       1.4142136*RHO_X*FERRX/SNRATX*SQRT(-ALOG(1.-R1))*COS(R2)
            R1=RANRED(NSEED)
            R2=RANRED(NSEED)*6.283185307
            ERRIY=
     +       1.4142136*RHO_Y*FERRY/SNRATY*SQRT(-ALOG(1.-R1))*COS(R2)
            CALL PGPOINT(1,INDX0+ERRIX,INDY0+ERRIY,1)
            DELTA_IX(K)=ERRIX
            DELTA_IY(K)=ERRIY
            DELTA_PAR2(K)=(A22*ERRIX/RHO_X-A12*ERRIY/RHO_Y)/DETA
            DELTA_PAR1(K)=(-A21*ERRIX/RHO_X+A11*ERRIY/RHO_Y)/DETA
          END DO
          CALL PGSCI(1)
          !calculamos fracción de puntos que caen dentro de cada elipse
          NP_IN_ELIPSE(1)=0
          NP_IN_ELIPSE(2)=0
          NP_IN_ELIPSE(3)=0
          AA1=RHO_X*RHO_X*FERRX*FERRX/SNRATX/SNRATX
          BB1=RHO_Y*RHO_Y*FERRY*FERRY/SNRATY/SNRATY
          AA2=AA1*4.0
          BB2=BB1*4.0
          AA3=AA1*9.0
          BB3=BB1*9.0
          DO K=1,NSIMUL
            XX=DELTA_IX(K)*DELTA_IX(K)
            YY=DELTA_IY(K)*DELTA_IY(K)
            IF(XX/AA1+YY/BB1.LE.1.0)THEN
              NP_IN_ELIPSE(1)=NP_IN_ELIPSE(1)+1
              NP_IN_ELIPSE(2)=NP_IN_ELIPSE(2)+1
              NP_IN_ELIPSE(3)=NP_IN_ELIPSE(3)+1
            ELSEIF(XX/AA2+YY/BB2.LE.1.0)THEN
              NP_IN_ELIPSE(2)=NP_IN_ELIPSE(2)+1
              NP_IN_ELIPSE(3)=NP_IN_ELIPSE(3)+1
            ELSEIF(XX/AA3+YY/BB3.LE.1.0)THEN
              NP_IN_ELIPSE(3)=NP_IN_ELIPSE(3)+1
            END IF
          END DO
          DO NSIGMA=1,3
             WRITE(*,'(A,I1,A,F7.5,2X,F7.5)') 
     +        '1-fraction of points_',nsigma,
     +        'sigma, expected (1-alpha): ',
     +       1.0-REAL(NP_IN_ELIPSE(NSIGMA))/REAL(NSIMUL),
     +       FCHISQR(2,REAL(NSIGMA*NSIGMA))
          END DO
          !dibujamos elipses a 1, 2 y 3 sigma
          DO NSIGMA=1,3
            AREA_II(NSIGMA)=3.141592654*(RHO_X*FERRX/SNRATX)*
     +       (RHO_Y*FERRY/SNRATY)*REAL(NSIGMA)*REAL(NSIGMA)
            WRITE(*,'(A,I1,A,$)') 'area_ii_',NSIGMA,'sigma: '
            WRITE(*,*) AREA_II(NSIGMA)
            DO K=1,NPOINTSELLIPSE
              T=REAL(K-1)/REAL(NPOINTSELLIPSE-1)*6.283185307
              XX=REAL(NSIGMA)*RHO_X*FERRX/SNRATX*COS(T)
              YY=REAL(NSIGMA)*RHO_Y*FERRY/SNRATY*SIN(T)
              XEL(K)=INDX0+XX
              YEL(K)=INDY0+YY
            END DO
            CALL PGSLW(3)
            CALL PGSCI(0)
            CALL PGLINE(NPOINTSELLIPSE,XEL,YEL)
            CALL PGSLW(1)
            CALL PGSCI(4)
            CALL PGLINE(NPOINTSELLIPSE,XEL,YEL)
            CALL PGSCI(1)
          END DO
          CALL PGSCI(3)
          CALL PGSLW(3)
          CALL PGERR1(1,INDX0,INDY0,RHO_X*FERRX/SNRATX,1.0)
          CALL PGERR1(2,INDX0,INDY0,RHO_Y*FERRY/SNRATY,1.0)
          CALL PGERR1(3,INDX0,INDY0,RHO_X*FERRX/SNRATX,1.0)
          CALL PGERR1(4,INDX0,INDY0,RHO_Y*FERRY/SNRATY,1.0)
          CALL PGSLW(1)
          CALL PGSCI(2)
          CALL PGPOINT(1,INDX0,INDY0,17)
          CALL PGSCI(1)
          CALL PGSLW(2)
c calculamos media y dispersión
          MEANERR_PAR1=FMEAN0(NSIMUL,DELTA_PAR1,SIGMAERR_PAR1)
          MEANERR_PAR2=FMEAN0(NSIMUL,DELTA_PAR2,SIGMAERR_PAR2)
          PERC1_PAR1=FPERCENT(NSIMUL,DELTA_PAR1,15.87)
          MEDIAN_PAR1=FPERCENT(NSIMUL,DELTA_PAR1,50.00)
          PERC2_PAR1=FPERCENT(NSIMUL,DELTA_PAR1,84.13)
          PERC1_PAR2=FPERCENT(NSIMUL,DELTA_PAR2,15.87)
          MEDIAN_PAR2=FPERCENT(NSIMUL,DELTA_PAR2,50.00)
          PERC2_PAR2=FPERCENT(NSIMUL,DELTA_PAR2,84.13)
c display results
          WRITE(*,101) '* Simulations:'
          WRITE(*,100) '>>>delta(parameter1): mean, sigma..: '
          WRITE(*,*) MEANERR_PAR1,SIGMAERR_PAR1
          WRITE(*,100) '>>>median..........................: '
          WRITE(*,*) MEDIAN_PAR1
          WRITE(*,100) '>>>percentiles (approx. +/-1 sigma): '
          WRITE(*,*) PERC1_PAR1,PERC2_PAR1
          WRITE(*,100) '>>>delta(parameter2): mean, sigma..: '
          WRITE(*,*) MEANERR_PAR2,SIGMAERR_PAR2
          WRITE(*,100) '>>>median..........................: '
          WRITE(*,*) MEDIAN_PAR2
          WRITE(*,100) '>>>percentiles (approx. +/-1 sigma): '
          WRITE(*,*) PERC1_PAR2,PERC2_PAR2
          WRITE(*,100) 'Suitability check: '
          WRITE(*,*) SIGMAERR_PAR1*SIGMAERR_PAR1+
     +     SIGMAERR_PAR2*SIGMAERR_PAR2,
     +     FDEGEX*FDEGEX/SNRATX/SNRATX+FDEGEY*FDEGEY/SNRATY/SNRATY
c plot delta(parameter1) vs. delta(parameter2)
          XMIN_=DELTA_PAR2(1)
          XMAX_=XMIN_
          YMIN_=DELTA_PAR1(1)
          YMAX_=YMIN_
          DO K=1,NSIMUL
            XMIN_=AMIN1(XMIN_,DELTA_PAR2(K))
            XMAX_=AMAX1(XMAX_,DELTA_PAR2(K))
            YMIN_=AMIN1(YMIN_,DELTA_PAR1(K))
            YMAX_=AMAX1(YMAX_,DELTA_PAR1(K))
          END DO
          DX_=XMAX_-XMIN_
          DY_=YMAX_-YMIN_
          XMIN_=XMIN_-DX_/20.
          XMAX_=XMAX_+DX_/20.
          YMIN_=YMIN_-DY_/20.
          YMAX_=YMAX_+DY_/20.
!         CALL PGENV(AMIN1(XMIN_,YMIN_),AMAX1(XMAX_,YMAX_),
!    +     AMIN1(XMIN_,YMIN_),AMAX1(XMAX_,YMAX_),0,0)
          CALL PGENV(XMIN_,XMAX_,YMIN_,YMAX_,0,0)
          CALL PGSCI(14)
          CALL PGPOINT(NSIMUL,DELTA_PAR2,DELTA_PAR1,1)
          CALL PGSCI(3)
          CALL PGSLW(3)
          CALL PGERR1(1,MEANERR_PAR2,MEANERR_PAR1,SIGMAERR_PAR2,1.0)
          CALL PGERR1(2,MEANERR_PAR2,MEANERR_PAR1,SIGMAERR_PAR1,1.0)
          CALL PGERR1(3,MEANERR_PAR2,MEANERR_PAR1,SIGMAERR_PAR2,1.0)
          CALL PGERR1(4,MEANERR_PAR2,MEANERR_PAR1,SIGMAERR_PAR1,1.0)
          CALL PGSLW(1)
          CALL PGSCI(2)
          CALL PGPOINT(1,MEANERR_PAR2,MEANERR_PAR1,17)
          CALL PGSCI(1)
          CALL PGSLW(2)
          CALL PGLABEL('\gD'//CPARAMETER(NPARAM(2)),
     +     '\gD'//CPARAMETER(NPARAM(1)),
     +     CMODELNAME)
          !dibujamos elipses a 1, 2 y 3 sigma (aqui no hace falta utilizar
          !los factores RHO_X y RHO_Y multiplicando a FERRX y FERRY porque 
          !los coeficientes B ya han sido reescalados; sin embargo si hace
          !falta incluirlos en el calculo de area)
          DETB=B11*B22-B12*B21
          DO NSIGMA=1,3
            AREA_AZ(NSIGMA)=AREA_II(NSIGMA)*ABS(DETB/(RHO_X*RHO_Y))
            WRITE(*,'(A,I1,A,$)') 'area_az_',NSIGMA,'sigma: '
            WRITE(*,*) AREA_AZ(NSIGMA)
            CALL PGSCI(NSIGMA+1)
            DO K=1,NPOINTSELLIPSE
              T=REAL(K-1)/REAL(NPOINTSELLIPSE-1)*6.283185307
              XX=REAL(NSIGMA)*FERRX/SNRATX*COS(T)
              YY=REAL(NSIGMA)*FERRY/SNRATY*SIN(T)
              XEL(K)=B11*XX+B12*YY
              YEL(K)=B21*XX+B22*YY
            END DO
            CALL PGSLW(3)
            CALL PGSCI(0)
            CALL PGLINE(NPOINTSELLIPSE,XEL,YEL)
            CALL PGSLW(1)
            CALL PGSCI(4)
            CALL PGLINE(NPOINTSELLIPSE,XEL,YEL)
          END DO
          CALL PGSLW(2)
          CALL PGSCI(1)
          CCHECK(1:1)=READC(IDLOG,'Check area estimate (y/n)','n','yn')
          WRITE(77,112) CCHECK,'# check area estimate?'
          IF(CCHECK.EQ.'y')THEN
            CALL CHEQUEA_AREA(IDLOG,NPOINTSELLIPSE,XEL,YEL)
          END IF
          CALL PGSCI(1)
          CALL PGSUBP(1,1)

        END IF
C..............................................................................
C Reiniciamos
        GOTO 90
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
996     CONTINUE
        WRITE(*,100) 'FATAL ERROR: while reading '
        L1=TRUEBEG(DATMODELFILE(NMODEL))
        L2=TRUELEN(DATMODELFILE(NMODEL))
        WRITE(*,101) DATMODELFILE(NMODEL)(L1:L2)
        WRITE(*,101) '>> unexpected end of file reached'
        CLOSE(10)
        STOP
C------------------------------------------------------------------------------
997     CONTINUE
        WRITE(*,100) 'FATAL ERROR: while reading '
        L1=TRUEBEG(DATMODELFILE(NMODEL))
        L2=TRUELEN(DATMODELFILE(NMODEL))
        WRITE(*,101) DATMODELFILE(NMODEL)(L1:L2)
        CLOSE(10)
        STOP
C------------------------------------------------------------------------------
998     WRITE(77,112) COPC,'# COPC'
999     IF(IDLOG.NE.0)THEN
          CLOSE(IDLOG)
          ISYSTEM=SYSTEMFUNCTION('rm -f rmodel.log')
          CALL PGEND
          STOP
        END IF
        WRITE(77,101) 'END_of_rmodel.log'
        CLOSE(77)
        CSAVE(1:1)=READC(IDLOG,
     +   'Do you want to save a logfile of this session (y/n)',
     +   'n','yYnN')
        IF((CSAVE.EQ.'y').OR.(CSAVE.EQ.'Y'))THEN
          FILELOG=READC(IDLOG,'Log file name','@','@')
          ISYSTEM=SYSTEMFUNCTION('mv -f rmodel.log '//
     +     FILELOG(TRUEBEG(FILELOG):TRUELEN(FILELOG)))
        ELSE
          ISYSTEM=SYSTEMFUNCTION('rm -f rmodel.log')
        END IF
        CALL PGEND
        STOP
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
111     FORMAT(I12,1X,A)
112     FORMAT(11X,A1,1X,A)
201     FORMAT(79('='))
202     FORMAT(79('-'))
203     FORMAT(79('*'))
        END
