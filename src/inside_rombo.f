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
C Devuelve .TRUE. si el punto X,Y esta dentro del rombo definido por los puntos
C X1,Y1,...,X4,Y4. Para saberlo, comprobamos que el signo del producto
C vectorial de los cuatro puntos con el punto problema no cambia de signo. 
C En el caso de que el punto este dentro del rombo, determinamos que vertice
c IBEST es el mas proximo al punto X,Y. Para ello podemos determinar 
C directamente cual es el mas cercano (LRENORM=.FALSE.) o podemos renormalizar
C el recorrido en X e Y para que en la determinacion del punto mas cercano no
C influya la escala en cada eje (LRENORM=.TRUE.).
        LOGICAL FUNCTION INSIDE_ROMBO(X,Y,X1,Y1,X2,Y2,X3,Y3,X4,Y4,
     +   LRENORM,IBEST)
        IMPLICIT NONE
        REAL X,Y
        REAL X1,Y1,X2,Y2,X3,Y3,X4,Y4
        LOGICAL LRENORM
        INTEGER IBEST
C
        REAL V12,V23,V34,V41
        REAL DMIN,DIST
        REAL XMIN,XMAX,YMIN,YMAX
        REAL XX,YY
        REAL XX1,YY1,XX2,YY2,XX3,YY3,XX4,YY4
C------------------------------------------------------------------------------
        V12=(X2-X1)*(Y-Y1)-(Y2-Y1)*(X-X1)
        V23=(X3-X2)*(Y-Y2)-(Y3-Y2)*(X-X2)
        V34=(X4-X3)*(Y-Y3)-(Y4-Y3)*(X-X3)
        V41=(X1-X4)*(Y-Y4)-(Y1-Y4)*(X-X4)
!       INSIDE_ROMBO=
!    +  (
!    +   (V12.GE.0.0).AND.(V23.GE.0.0).AND.(V34.GE.0.0).AND.(V41.GE.0.0)
!    +   ).OR.(
!    +   (V12.LE.0.0).AND.(V23.LE.0.0).AND.(V34.LE.0.0).AND.(V41.LE.0.0)
!    +  )
        INSIDE_ROMBO=(V12*V23.GE.0.0).AND.(V23*V34.GE.0.0).AND.
     +               (V34*V41.GE.0.0)
C
        IF(INSIDE_ROMBO)THEN
          IF(LRENORM)THEN
            !calculamos limites
            XMIN=AMIN1(X1,X2)
            XMIN=AMIN1(XMIN,X3)
            XMIN=AMIN1(XMIN,X4)
            XMAX=AMAX1(X1,X2)
            XMAX=AMAX1(XMAX,X3)
            XMAX=AMAX1(XMAX,X4)
            YMIN=AMIN1(Y1,Y2)
            YMIN=AMIN1(YMIN,Y3)
            YMIN=AMIN1(YMIN,Y4)
            YMAX=AMAX1(Y1,Y2)
            YMAX=AMAX1(YMAX,Y3)
            YMAX=AMAX1(YMAX,Y4)
            !renormalizamos
            XX=(X-XMIN)/(XMAX-XMIN)
            YY=(Y-YMIN)/(YMAX-YMIN)
            XX1=(X1-XMIN)/(XMAX-XMIN)
            XX2=(X2-XMIN)/(XMAX-XMIN)
            XX3=(X3-XMIN)/(XMAX-XMIN)
            XX4=(X4-XMIN)/(XMAX-XMIN)
            YY1=(Y1-YMIN)/(YMAX-YMIN)
            YY2=(Y2-YMIN)/(YMAX-YMIN)
            YY3=(Y3-YMIN)/(YMAX-YMIN)
            YY4=(Y4-YMIN)/(YMAX-YMIN)
          ELSE !................................................LRENORM=.FALSE.
            !no cambiamos nada
            XX=X
            YY=Y
            XX1=X1
            XX2=X2
            XX3=X3
            XX4=X4
            YY1=Y1
            YY2=Y2
            YY3=Y3
            YY4=Y4
          END IF
          !calculamos distancia minima
          DMIN=(XX-XX1)*(XX-XX1)+(YY-YY1)*(YY-YY1)
          IBEST=1
          DIST=(XX-XX2)*(XX-XX2)+(YY-YY2)*(YY-YY2)
          IF(DIST.LT.DMIN)THEN
            IBEST=2
            DMIN=DIST
          END IF
          DIST=(XX-XX3)*(XX-XX3)+(YY-YY3)*(YY-YY3)
          IF(DIST.LT.DMIN)THEN
            IBEST=3
            DMIN=DIST
          END IF
          DIST=(XX-XX4)*(XX-XX4)+(YY-YY4)*(YY-YY4)
          IF(DIST.LT.DMIN)THEN
            IBEST=4
            DMIN=DIST
          END IF
        ELSE !.............................................INSIDE_ROMBO=.FALSE.
          IBEST=0
        END IF
C
        END
