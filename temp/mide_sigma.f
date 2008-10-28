        PROGRAM MIDE_SIGMA
        IMPLICIT NONE
C
        INTEGER I
        REAL X0,Y0,X,Y
        REAL XMIN,XMAX,YMIN,YMAX
        CHARACTER*255 INFILE
C------------------------------------------------------------------------------
        WRITE(*,100) 'Input file name? '
        READ(*,101) INFILE
        OPEN(10,FILE=INFILE,STATUS='OLD',FORM='FORMATTED')
        DO I=1,7 !salta comentarios
          READ(10,*)
        END DO
        READ(10,*) X0,Y0 !punto central
        XMIN=X0
        XMAX=X0
        YMIN=Y0
        YMAX=Y0
        DO I=0,360
          READ(10,*) X,Y
          XMIN=AMIN1(XMIN,X)
          XMAX=AMAX1(XMAX,X)
          YMIN=AMIN1(YMIN,Y)
          YMAX=AMAX1(YMAX,Y)
        END DO
        WRITE(*,*) 'Sigma X: ',(XMAX-XMIN)/2.0
        WRITE(*,*) 'Sigma Y: ',(YMAX-YMIN)/2.0
        CLOSE(10)
        STOP
100     FORMAT(A,$)
101     FORMAT(A)
        END
