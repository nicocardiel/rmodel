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
Comment
C
C REAL FUNCTION LININTERP(N,X,Y,X0,IFLAG,N1,N2)
C
C Input: N,X,Y,X0
C Output: LININTERP(function), IFLAG
C
C Performs a linear interpolation in the table X(N),Y(N) at x=X0. Note that the
C X matrix must be sorted in ascending order, although the 
C
C INTEGER N -> input number of data in X and Y
C REAL    X(N) -> data matrix 
C REAL    Y(N) -> data matrix 
C REAL    X0 -> x-point at which the linear interpolation is evaluated
C INTEGER IFLAG -> = 0 : interpolation
C                  = -1 : extrapolation towards lower X values
C                  = +1 : extrapolation towards higher X values
C                  = +2 : X0=X(N), which could produce some "border" effects
C                  = +9 : error (division by zero)
C INTEGER N1 -> first data entry towards the left of X0
C INTEGER N2 -> first data entry towards the right of X0
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION LININTERP(N,X,Y,X0,IFLAG,N1,N2)
        IMPLICIT NONE
C       
        INTEGER N
        REAL X(N),Y(N),X0
        INTEGER IFLAG
        INTEGER N1,N2
C------------------------------------------------------------------------------
        IF(X0.EQ.X(N))THEN !extremo superior: da division por cero abajo
          IFLAG=2          !porque BINSEARCH devuelve N1=N, por lo que ser�a
                           !N2=N+1, pero este valor est� fuera del recorrido
                           !definido por el intervalo [1,N]
          N1=N
          N2=N
          LININTERP=Y(N)
          RETURN
        ELSEIF(X0.LT.X(1))THEN !extrapolacion a la izquierda
          IFLAG=-1
          N1=1
          N2=2
        ELSEIF(X0.GT.X(N))THEN !extrapolacion a la derecha
          IFLAG=1
          N1=N-1
          N2=N
        ELSE !caso general
          IFLAG=0
          N1=N/2 !como inicio de busqueda tomamos el centro de la tabla
          CALL BINSEARCH(X,N,X0,N1)
          N2=N1+1
        END IF
C
        IF(X(N1).NE.X(N2))THEN
          LININTERP=Y(N1)+((X0-X(N1))/(X(N2)-X(N1)))*(Y(N2)-Y(N1))
        ELSE
          IFLAG=9
          LININTERP=0.
        END IF
C       
        END
