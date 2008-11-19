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
C Calculamos el �rea usando Monte Carlo. Para ello determinamos si un punto
C est� dentro o no del pol�gono mirando si el producto vectorial tiene
C siempre el mismo signo.
        SUBROUTINE CHEQUEA_AREA(IDLOG,N,XEL,YEL)
        IMPLICIT NONE
        INTEGER IDLOG
        INTEGER N
        REAL XEL(N),YEL(N)
C
        INTEGER READI
        INTEGER READILIM
        REAL RANRED
        EXTERNAL RANRED
C
        INTEGER I,K
        INTEGER NSEED
        INTEGER NSIMULATIONS
        INTEGER NINSIDE
        REAL VPX,VPY,VEX,VEY
        REAL XR,YR
        REAL PVECT,PVECT_INI
        REAL XMIN,XMAX,YMIN,YMAX
        REAL AREA_PLOT,AREA_POLIGONO
        LOGICAL LOK
C
        COMMON/BLKNSEED/NSEED
C------------------------------------------------------------------------------
        NSIMULATIONS=READILIM(IDLOG,
     +   'No. of points to check area estimate (0=none)',
     +   '@',0,10000000)
        WRITE(77,111) NSIMULATIONS,'# no. of points to check area'
        IF(NSIMULATIONS.LE.0) RETURN
        IF(NSEED.EQ.-1)THEN
          NSEED=READI(IDLOG,'NSEED for random numbers','-1')
          WRITE(77,111) NSEED,'# NSEED for random numbers'
        END IF
        CALL PGQWIN(XMIN,XMAX,YMIN,YMAX)
        AREA_PLOT=(XMAX-XMIN)*(YMAX-YMIN)
        WRITE(*,100) 'Total area in plot region...............'//
     +   '................: '
        WRITE(*,*) AREA_PLOT
C
        NINSIDE=0
        DO K=1,NSIMULATIONS
          LOK=.TRUE.
          XR=XMIN+RANRED(NSEED)*(XMAX-XMIN)
          YR=YMIN+RANRED(NSEED)*(YMAX-YMIN)
          PVECT_INI=0 !evita un warning de compilacion
          DO I=1,N-1
            VEX=XEL(I)-XEL(I+1)
            VEY=YEL(I)-YEL(I+1)
            VPX=XEL(I)-XR
            VPY=YEL(I)-YR
            PVECT=VEX*VPY-VEY*VPX
            IF(I.EQ.1)THEN
              PVECT_INI=PVECT
            ELSE
              IF(PVECT*PVECT_INI.LT.0.0) LOK=.FALSE.
            END IF
          END DO
          IF(LOK)THEN
            CALL PGSCI(5)
            NINSIDE=NINSIDE+1
          ELSE
            CALL PGSCI(7)
          END IF
          CALL PGPOINT(1,XR,YR,1)
        END DO
        AREA_POLIGONO=AREA_PLOT*REAL(NINSIDE)/REAL(NSIMULATIONS)
        WRITE(*,100) 'Area inside polygon (to be compared with'//
     +   ' area_az_3sigma): '
        WRITE(*,*) AREA_POLIGONO
C
100     FORMAT(A,$)
111     FORMAT(I12,1X,A)
        END
