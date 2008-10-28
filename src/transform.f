C------------------------------------------------------------------------------
C Version 22-October-2008                                     File: transform.f
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
C Computes a spatial transformation using least-squares with polynomials 
C (see "Digital Image Warping", George Wolberg, IEEE Computer 
C Society Press Monograph, pag. 61).
C Input: M,X,Y,U,V,N
C Output: AIJ,BIJ
C------------------------------------------------------------------------------
C NFIT: numero de puntos con los que vamos a calcular la transformacion
C X(NFIT),Y(NFIT),U(NFIT),V(NFIT): coordenadas de los puntos
C N: grado de la transformacion
C AIJ((N+1)*N),BIJ((N+1)*N): coeficientes de la transformacion
C------------------------------------------------------------------------------
        SUBROUTINE TRANSFORM(NFIT,X,Y,U,V,N,AIJ,BIJ)
        IMPLICIT NONE
C parametros de la subrutina (ver tambien subrutinas auxiliares)
        INTEGER NECUAMAX
        PARAMETER (NECUAMAX=12)  !numero maximo de terminos que puede ajustarse
C parametros de entrada/salida en la subrutina
        INTEGER NFIT
        REAL X(NFIT),Y(NFIT)
        REAL U(NFIT),V(NFIT)
        INTEGER N
        REAL AIJ((N+1)*N),BIJ((N+1)*N)
C variables locales
        INTEGER I,J,K,L,M,II,JJ
        INTEGER NECUA
        INTEGER ORDER(NECUAMAX),IOK,IPAR
        REAL UU,VV
        DOUBLE PRECISION DSUM
        REAL CHISQR1,CHISQR2
        REAL SCALEROW(NECUAMAX)
        REAL A(NECUAMAX,NECUAMAX),B(NECUAMAX)
C variables usadas para renormalización en el caso N=2
        real phi !funcion definida mas abajo
        real xmin,xmax,ymin,ymax
        real cx1,cx2,cy1,cy2
        real aij_(6),bij_(6)
        logical lnormaliza
C------------------------------------------------------------------------------
!       do k=1,nfit
!         print*,'polynomial fit input> ',k,x(k),y(k),u(k),v(k)
!       end do
C He descubierto que es esencial normalizar el recorrido de las variables
C antes del ajuste. En caso contrario, cuando se quiere estudiar diagramas
C con los índices estrechos de A. Vazdekis, obtenemos matrices singulares.
C ¡Todo es un problema de redondeo!
        lnormaliza=.true.
C para el caso N=2 podemos renormalizar el recorrido de las variables y
C reducir así el problema de obtener una matriz casi singular
        if((n.eq.2).and.(lnormaliza))then
          xmax=x(1)
          ymax=y(1)
          xmin=xmax
          ymin=ymax
          if(nfit.gt.1)then
            do i=2,nfit
              if(x(i).gt.xmax) xmax=x(i)
              if(y(i).gt.ymax) ymax=y(i)
              if(x(i).lt.xmin) xmin=x(i)
              if(y(i).lt.ymin) ymin=y(i)
            end do
          end if
          if(xmin.eq.xmax)then
            cx1=1.0
            cx2=0.
          else
            cx1=2./(xmax-xmin)
            cx2=(xmax+xmin)/(xmax-xmin)
          end if
          do i=1,nfit
            x(i)=x(i)*cx1-cx2
          end do
          if(ymin.eq.ymax)then
            cy1=1.0
            cy2=0.
          else
            cy1=2./(ymax-ymin)
            cy2=(ymax+ymin)/(ymax-ymin)
          end if
          do i=1,nfit
            y(i)=y(i)*cy1-cy2
          end do
        end if
C------------------------------------------------------------------------------
        NECUA=(N+1)*N
        IF(NECUA.GT.NECUAMAX)THEN
          WRITE(*,101) '***FATAL ERROR***'
          WRITE(*,100) '=> NECUA,NECUAMAX: '
          WRITE(*,*) NECUA,NECUAMAX
          WRITE(*,101) '=> NECUA > NECUAMAX in subroutine TRANSFORM'
          STOP
        END IF
C------------------------------------------------------------------------------
C matriz del primer sistema de ecuaciones a resolver
        II=0
        DO L=0,N
          DO M=0,N-L
            II=II+1
            JJ=0
            DO I=0,N
              DO J=0,N-I
                JJ=JJ+1
                DSUM=0.D0
                DO K=1,NFIT
                  DSUM=DSUM+(DBLE(X(K))**(I+L))*(DBLE(Y(K))**(J+M))
                END DO
                A(II,JJ)=REAL(DSUM)
              END DO
            END DO
            DSUM=0.D0
            DO K=1,NFIT
              DSUM=DSUM+(DBLE(X(K))**L)*(DBLE(Y(K))**M)*DBLE(U(K))
            END DO
            B(II)=REAL(DSUM)
          END DO
        END DO
C resolvemos el sistema de ecuaciones
!       WRITE(*,100)'---> LU Descomposition...'
        CALL LUDCMP(A,NECUA,NECUAMAX,ORDER,SCALEROW,IOK,IPAR)
!       WRITE(*,101)' OK!'
!       WRITE(*,100)'---> Forward substitution and back substitution...'
        CALL LUSOLV(A,NECUA,NECUAMAX,ORDER,SCALEROW,B,AIJ)
!       WRITE(*,101)' OK!'
C..............................................................................
C segundo sistema de ecuaciones a resolver (notar que la matriz
C del sistema de ecuaciones es la misma)
        II=0
        DO L=0,N
          DO M=0,N-L
            II=II+1
            DSUM=0.D0
            DO K=1,NFIT
              DSUM=DSUM+(DBLE(X(K))**L)*(DBLE(Y(K))**M)*DBLE(V(K))
            END DO
            B(II)=REAL(DSUM)
          END DO
        END DO
C resolvemos el sistema de ecuaciones
!       WRITE(*,100)'---> Forward substitution and back substitution...'
        CALL LUSOLV(A,NECUA,NECUAMAX,ORDER,SCALEROW,B,BIJ)
!       WRITE(*,101)' OK!'
C..............................................................................
C chisqr
        CHISQR1=0.
        CHISQR2=0.
        DO K=1,NFIT
          CALL FMAP(N,AIJ,BIJ,X(K),Y(K),UU,VV)
          CHISQR1=CHISQR1+(UU-U(K))*(UU-U(K))
          CHISQR2=CHISQR2+(VV-V(K))*(VV-V(K))
        END DO
C..............................................................................
C mostramos coeficientes aij
!       II=0
!       DO L=0,N
!         DO M=0,N-L
!           II=II+1
!           WRITE(*,'(A2,I1,A1,I1,A3,$)') 'a(',L,',',M,'): '
!           WRITE(*,*) AIJ(II)
!         END DO
!       END DO
!       WRITE(*,100) 'Residual standard deviation: '
!       IF(NFIT.GT.NECUA)THEN
!         WRITE(*,*) SQRT(CHISQR1/REAL(NFIT-NECUA))
!       ELSE
!         WRITE(*,*) 0.0
!       END IF
C mostramos coeficientes bij
!       II=0
!       DO L=0,N
!         DO M=0,N-L
!           II=II+1
!           WRITE(*,'(A2,I1,A1,I1,A3,$)') 'b(',L,',',M,'): '
!           WRITE(*,*) BIJ(II)
!         END DO
!       END DO
!       WRITE(*,100) 'Residual standard deviation: '
!       IF(NFIT.GT.NECUA)THEN
!         WRITE(*,*) SQRT(CHISQR2/REAL(NFIT-NECUA))
!       ELSE
!         WRITE(*,*) 0.0
!       END IF
C------------------------------------------------------------------------------
C Si hemos hecho un cambio de escala, lo deshacemos
        if((n.eq.2).and.(lnormaliza))then
          do i=1,nfit
            x(i)=(x(i)+cx2)/cx1
            y(i)=(y(i)+cy2)/cy1
          end do
          aij_(6)=aij(6)/phi(2,0,0,0,cx1,cx2,cy1,cy2)
          aij_(5)=aij(5)/phi(1,0,1,0,cx1,cx2,cy1,cy2)
          aij_(4)=(aij(4)-aij_(5)*phi(1,0,1,1,cx1,cx2,cy1,cy2)
     +             -aij_(6)*phi(2,1,0,0,cx1,cx2,cy1,cy2))/
     +            phi(1,0,0,0,cx1,cx2,cy1,cy2)
          aij_(3)=aij(3)/phi(0,0,2,0,cx1,cx2,cy1,cy2)
          aij_(2)=(aij(2)-aij_(3)*phi(0,0,2,1,cx1,cx2,cy1,cy2)
     +             -aij_(5)*phi(1,1,1,0,cx1,cx2,cy1,cy2))/
     +            phi(0,0,1,0,cx1,cx2,cy1,cy2)
          aij_(1)=(aij(1)-aij_(2)*phi(0,0,1,1,cx1,cx2,cy1,cy2)
     +             -aij_(3)*phi(0,0,2,2,cx1,cx2,cy1,cy2)
     +             -aij_(4)*phi(1,1,0,0,cx1,cx2,cy1,cy2)
     +             -aij_(5)*phi(1,1,1,1,cx1,cx2,cy1,cy2)
     +             -aij_(6)*phi(2,2,0,0,cx1,cx2,cy1,cy2))/
     +             phi(0,0,0,0,cx1,cx2,cy1,cy2)
          do i=1,6
            aij(i)=aij_(i)
          end do
          bij_(6)=bij(6)/phi(2,0,0,0,cx1,cx2,cy1,cy2)
          bij_(5)=bij(5)/phi(1,0,1,0,cx1,cx2,cy1,cy2)
          bij_(4)=(bij(4)-bij_(5)*phi(1,0,1,1,cx1,cx2,cy1,cy2)
     +             -bij_(6)*phi(2,1,0,0,cx1,cx2,cy1,cy2))/
     +            phi(1,0,0,0,cx1,cx2,cy1,cy2)
          bij_(3)=bij(3)/phi(0,0,2,0,cx1,cx2,cy1,cy2)
          bij_(2)=(bij(2)-bij_(3)*phi(0,0,2,1,cx1,cx2,cy1,cy2)
     +             -bij_(5)*phi(1,1,1,0,cx1,cx2,cy1,cy2))/
     +            phi(0,0,1,0,cx1,cx2,cy1,cy2)
          bij_(1)=(bij(1)-bij_(2)*phi(0,0,1,1,cx1,cx2,cy1,cy2)
     +             -bij_(3)*phi(0,0,2,2,cx1,cx2,cy1,cy2)
     +             -bij_(4)*phi(1,1,0,0,cx1,cx2,cy1,cy2)
     +             -bij_(5)*phi(1,1,1,1,cx1,cx2,cy1,cy2)
     +             -bij_(6)*phi(2,2,0,0,cx1,cx2,cy1,cy2))/
     +             phi(0,0,0,0,cx1,cx2,cy1,cy2)
          do i=1,6
            bij(i)=bij_(i)
          end do
!         do i=1,6
!           print*,'fin> ',i,aij(i),bij(i)
!         end do
        end if
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C Rutina que calcula la transformacion segun la transformacion ajustada
C
        SUBROUTINE FMAP(N,AIJ,BIJ,X,Y,U,V)
        INTEGER N
        REAL AIJ((N+1)*N)
        REAL BIJ((N+1)*N)
        REAL X,Y,U,V
C
        INTEGER II,L,M
C------------------------------------------------------------------------------
        II=0
        U=0.
        V=0.
        DO L=0,N
          DO M=0,N-L
            II=II+1
            U=U+AIJ(II)*(X**L)*(Y**M)
            V=V+BIJ(II)*(X**L)*(Y**M)
          END DO
        END DO
C
        END
C------------------------------------------------------------------------------
C Version 13-September-1999                                      File: ludcmp.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE LUDCMP(A,N,NDIM,ORDER,SCALEROW,IOK,IPAR)
C
C Input: A,N,NDIM
C Output: A,ORDER,SCALEROW,IOK,IPAR
C
C This subroutine computes the L and U triangular matrices equivalent to the
C A matrix, such that LU = A.  These matrices are returned in the space of A,
C in compact form. See C.F. Gerald and P. O. Wheatley, in Applied Numerical
C Analysis, 4th edition, pag. 106.
C
C REAL A(NDIM,NDIM) -> matrix of coefficients
C INTEGER N -> logical dimension of A
C INTEGER NDIM -> physical dimension of A in the calling program
C INTEGER ORDER(N) -> vector holding row order after pivoting
C REAL SCALEROW(N) -> vector holding scaling factors applied to each row
C INTEGER IOK -> returns 0 if everything works properly, +(the row number)
C                if all elements in a row are zero, or -(the row number) if 
C                the pivot value is zero.
C INTEGER IPAR -> returns as +1 or -1 depending on whether the number of row
C                 interchanges was even or odd, respectively
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE LUDCMP(A,N,NDIM,ORDER,SCALEROW,IOK,IPAR)
        IMPLICIT NONE
C
        INTEGER N,NDIM
        REAL A(NDIM,NDIM)
        INTEGER ORDER(N)
        REAL SCALEROW(N)
        INTEGER IOK,IPAR
C local variables
        INTEGER I,J,K
        REAL AMIN,AMAX,ATEST
        DOUBLE PRECISION DSUM
        LOGICAL LSCALE,LOK
C------------------------------------------------------------------------------
C el orden inicial es el de partida
        DO I=1,N
          ORDER(I)=I
        END DO
        IOK=0
        IPAR=1
C------------------------------------------------------------------------------
C si N es igual a 1, no hay nada que hacer
        IF(N.EQ.1)THEN
          SCALEROW(1)=1.0
          RETURN
        END IF
C------------------------------------------------------------------------------
C Antes de realizar el proceso, es interesante verificar si existen numeros
C con diferentes ordenes de magnitud en una misma fila. Si es asi, realizamos
C un proceso de reescalado de forma que el valor maximo en cada fila sea igual
C a uno. Si la diferencia no es muy grande no reescalamos, para evitar asi 
C introducir errores de redondeo al realizar precisamente dicho reescalado.
        LSCALE=.FALSE.
C
        DO I=1,N
          AMIN=ABS(A(I,1))
          AMAX=ABS(A(I,1))
          DO J=2,N
            ATEST=ABS(A(I,J))
            IF(ATEST.LT.AMIN) AMIN=ATEST
            IF(ATEST.GT.AMAX) AMAX=ATEST
          END DO
          IF(AMAX.EQ.0.0)THEN
            WRITE(*,100)'ERROR: all coefficientes are zero in row '
            WRITE(*,*)I
            WRITE(*,100)'Press <CR> to continue...'
            READ(*,*)
            IOK=I
            RETURN
          ELSEIF(AMIN.EQ.0.0)THEN
            LSCALE=.TRUE.
          ELSE
            IF(ALOG10(AMAX)-ALOG10(AMIN).GT.1.5) LSCALE=.TRUE.
          END IF
          SCALEROW(I)=1./AMAX
        END DO
C
        IF(LSCALE)THEN
          DO I=1,N
            DO J=1,N
              A(I,J)=A(I,J)*SCALEROW(I)
            END DO
          END DO
        ELSE
          DO I=1,N
            SCALEROW(I)=1.
          END DO
        END IF
C------------------------------------------------------------------------------
C hacemos pivote sobre la primera columna, siempre que N sea mayor que 1
ccc10      CALL PIVOTE(A,N,NDIM,ORDER,1,IPAR)
        CALL PIVOTE(A,N,NDIM,ORDER,1,IPAR)
C------------------------------------------------------------------------------
C si el pivote es muy pequen~o, podemos tener una matriz singular o casi 
C singular
        CALL LITTLEPIVOTE(A(1,1),LOK)
        IF(.NOT.LOK)THEN
          IOK=1
          RETURN
        END IF
C------------------------------------------------------------------------------
C calculamos los coeficientes de la primera fila de la matriz U
        DO J=2,N
          A(1,J)=A(1,J)/A(1,1)
        END DO
C------------------------------------------------------------------------------
C continuamos el calculo de los coeficientes de L y U (salvo el ultimo
C coeficiente de L, que lo dejamos para despues). Notar como el sumatorio
C puede realizarse en doble precision (Ver Gerald and Wheatley, pag. 109).
        DO J=2,N-1
C calculamos una columna de la matriz L
          DO I=J,N
            DSUM=0.D0
            DO K=1,J-1
              DSUM=DSUM+DBLE(A(I,K))*DBLE(A(K,J))
            END DO
            A(I,J)=A(I,J)-REAL(DSUM)
          END DO
C en caso necesario podemos intercambiar filas
          CALL PIVOTE(A,N,NDIM,ORDER,J,IPAR)
C comprobamos si el pivote es demasiado pequen~o
          CALL LITTLEPIVOTE(A(J,J),LOK)
          IF(.NOT.LOK)THEN
            IOK=J
            RETURN
          END IF
C calculamos una fila de la matriz U
          DO K=J+1,N
            DSUM=0.D0
            DO I=1,J-1
              DSUM=DSUM+DBLE(A(J,I))*DBLE(A(I,K))
            END DO
            A(J,K)=(A(J,K)-REAL(DSUM))/A(J,J)
          END DO
        END DO
C finalmente, calculamos el ultimo elemento de la matriz L
        DSUM=0.D0
        DO K=1,N-1
          DSUM=DSUM+DBLE(A(N,K))*DBLE(A(K,N))
        END DO
        A(N,N)=A(N,N)-REAL(DSUM)
        CALL LITTLEPIVOTE(A(N,N),LOK)
        IF(.NOT.LOK)THEN
          IOK=N
          RETURN
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
        END
C
C******************************************************************************
C Esta subrutina encuentra el elemento mayor para realizar pivote en la columna
C J0, intercambia los elementos de la matriz A y guarda un registro de los
C cambios realizados en la matriz ORDER. La variable IPAR retorna +1 o -1
C dependiendo del numero de cambios realizados en las filas.
        SUBROUTINE PIVOTE(A,N,NDIM,ORDER,J0,IPAR)
        IMPLICIT NONE
C
        INTEGER N,NDIM
        REAL A(NDIM,NDIM)
        INTEGER ORDER(N),J0,IPAR
C local variables
        INTEGER I,IPIVOTE,J,I0
        REAL AMAX,ATEST
C------------------------------------------------------------------------------
        IF(J0.EQ.N)THEN
          WRITE(*,100)'ERROR: no pivot row can be found for the last '
          WRITE(*,101)'element on diagonal.'
          WRITE(*,100)'Press <CR> to continue...'
          READ(*,*)
        END IF
C------------------------------------------------------------------------------
C buscamos la fila para realizar pivote, considerando solamente los elementos
C que se encuentran sobre y bajo la diagonal
        AMAX=ABS(A(J0,J0))
        IPIVOTE=J0
        DO I=J0+1,N
          ATEST=ABS(A(I,J0))
          IF(ATEST.GT.AMAX)THEN
            AMAX=ATEST
            IPIVOTE=I
          END IF
        END DO
C------------------------------------------------------------------------------
C Si la fila con el elemento mayor es J0, no hay nada que hacer. En caso
C contrario, realizamos el intercambio, tanto en la matriz A como en el
C vector ORDER.
        IF(IPIVOTE.EQ.J0) RETURN
C
        DO J=1,N
          ATEST=A(J0,J)
          A(J0,J)=A(IPIVOTE,J)
          A(IPIVOTE,J)=ATEST
        END DO
        I0=ORDER(J0)
        ORDER(J0)=ORDER(IPIVOTE)
        ORDER(IPIVOTE)=I0
        IPAR=-IPAR
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C Comprueba si el valor de A es demasiado pequen~o como para poder tener
C una matriz problematica.
        SUBROUTINE LITTLEPIVOTE(A,LOK)
        IMPLICIT NONE
C
        REAL A
        LOGICAL LOK
C------------------------------------------------------------------------------
        LOK=.TRUE.
C
        IF(ABS(A).EQ.0.0)THEN
          WRITE(*,101)'ERROR: singular matrix.'
          WRITE(*,100)'Press <CR> to continue...'
          READ(*,*)
          LOK=.FALSE.
        ELSEIF(ABS(A).LT.1.E-7)THEN
          WRITE(*,101)'WARNING: nearly singular matrix.'
          WRITE(*,100)'Press <CR> to continue...'
          READ(*,*)
          LOK=.FALSE.
        END IF
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
C------------------------------------------------------------------------------
C Version 18-June-1998                                           File: lusolv.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE LUSOLV(A,N,NDIM,ORDER,SCALEROW,B,X)
C
C Input: A,N,NDIM,ORDER,SCALEROW,B
C Output: X
C
C This subroutine solves the set of N linear equations A X = B, where the
C A matrix corresponds to the LU decomposition of the initial coefficient
C matrix. See C.F. Gerald and P. O. Wheatley, in Applied Numerical
C Analysis, 4th edition, pag. 110. The matrix A remains unchanged (also ORDER
C and SCALEROW), so subsequent calls to this subroutine, variying the B matrix,
C can be performed.
C
C REAL A(NDIM,NDIM) -> matrix of coefficients (LU in compact scheme)
C INTEGER N -> logical dimension of A
C INTEGER NDIM -> physical dimension of A in the calling program
C INTEGER ORDER(N) -> vector holding row order after pivoting in LUDCMP
C REAL SCALEROW(N) -> vector holding scaling factors applied to each row
C REAL B(N) -> right-hand side vector B
C REAL X(N) -> solution vector X
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE LUSOLV(A,N,NDIM,ORDER,SCALEROW,B,X)
        IMPLICIT NONE
C
        INTEGER N,NDIM
        REAL A(NDIM,NDIM)
        INTEGER ORDER(N)
        REAL SCALEROW(N)
        REAL B(N),X(N)
C local variables
        INTEGER I,I0,J
        DOUBLE PRECISION DSUM
C------------------------------------------------------------------------------
C reordenamos y reescalamos los elementos del vector B siguiendo la informacion
C contenida en los vectores ORDER y SCALEROW
        DO I=1,N
          I0=ORDER(I)
          X(I)=B(I0)*SCALEROW(I0)
        END DO
C------------------------------------------------------------------------------
C calculamos el vector Bprima
        X(1)=X(1)/A(1,1)
        IF(N.EQ.1) RETURN
        DO I=2,N
          DSUM=0.D0
          DO J=1,I-1
            DSUM=DSUM+DBLE(A(I,J))*DBLE(X(J))
          END DO
          X(I)=(X(I)-REAL(DSUM))/A(I,I)
        END DO
C------------------------------------------------------------------------------
C finalmente obtenemos las soluciones para X
        DO I=2,N
          DSUM=0.D0
          DO J=N-I+2,N
            DSUM=DSUM+DBLE(A(N-I+1,J))*DBLE(X(J))
          END DO
          X(N-I+1)=X(N-I+1)-REAL(DSUM)
        END DO
C------------------------------------------------------------------------------
        END
c
c******************************************************************************
c funcion phi para cambio de escala en datos de entrada para los ajustes
        real function phi(i,ii,j,jj,cx1,cx2,cy1,cy2)
        integer i,ii,j,jj
        real cx1,cx2,cy1,cy2
c
        double precision combpf
c
        real cxx1,cxx2,cyy1,cyy2
c------------------------------------------------------------------------------
        cxx1=1./cx1
        cxx2=cx2/cx1
        cyy1=1./cy1
        cyy2=cy2/cy1
        phi=real(combpf(i,ii))*real(combpf(j,jj))*
     +   (cxx1**(i-ii))*(cyy1**(j-jj))*(cxx2**ii)*(cyy2**jj)
c------------------------------------------------------------------------------
        end
C------------------------------------------------------------------------------
Comment
C
C DOUBLE PRECISION FUNCTION COMBPF(N,K)
C
C Input: N,K
C Output: COMBPF (function)
C
C Calculate the binomial coefficient N over K
C
C INTEGER N
C INTEGER K
C
Comment
C------------------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION COMBPF(N,K)
        IMPLICIT NONE
C
        DOUBLE PRECISION FACTORIALPF
        INTEGER N,K
C------------------------------------------------------------------------------
        IF(K.GT.N)THEN
          WRITE(*,101)'FATAL ERROR: in function COMBPF(N,K), K>N'
          STOP
        END IF
        COMBPF=FACTORIALPF(N)/(FACTORIALPF(K)*FACTORIALPF(N-K))
101     FORMAT(A)
        END
C------------------------------------------------------------------------------
Comment
C
C DOUBLE PRECISION FUNCTION FACTORIALPF(N)
C
C Input: N
C Output: FACTORIALPF (function)
C
C Calculate N factorial
C
C INTEGER N
C
Comment
C------------------------------------------------------------------------------
        DOUBLE PRECISION FUNCTION FACTORIALPF(N)
        IMPLICIT NONE
        INTEGER N
C
        INTEGER I
C------------------------------------------------------------------------------
        IF(N.LT.0)THEN
          WRITE(*,101)'FATAL ERROR: factorial(n<0)!'
          STOP
        END IF
        IF(N.GT.30)THEN
          WRITE(*,101)'FATAL ERROR: factorial(n>30)!'
          STOP
        END IF
        FACTORIALPF=1.D0
        IF(N.EQ.0) RETURN
        DO I=1,N
          FACTORIALPF=DBLE(I)*FACTORIALPF 
        END DO
C
101     FORMAT(A)
        END
