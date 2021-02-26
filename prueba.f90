PROGRAM prueba
USE, intrinsic :: iso_fortran_env, ONLY: WP => REAL64
USE roots, ONLY: biseccion
IMPLICIT NONE
REAL(WP) :: a, b, tol, raiz
INTEGER :: n, clave
! Datos de iniciales
a = 0.0_WP
b = 2.0_WP
tol = 0.5E-6_WP
n = 100
! Determinar la raíz
CALL biseccion(f,a,b,n,tol,raiz,clave)
IF (clave == 0) THEN
WRITE(*,*) 'Raiz = ', raiz
WRITE(*,*) 'Numero de iteraciones realizadas =', n
ELSE
WRITE(*,*) 'Error =', clave
ENDIF
CONTAINS
REAL(WP) FUNCTION f(x)
! Función que define la ecuación
REAL(WP), INTENT(IN) :: x
f = 1.0-x
END FUNCTION f
END PROGRAM prueba
