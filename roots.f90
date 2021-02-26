
SUBROUTINE biseccion(f,a,b,n,tol,raiz,clave)
  ! ---------------------------------------------------
  ! METODO DE BISECCION para encontrar una solución
  ! de f(x)=0 dada la función continua f en el intervalo
  ! [a,b] donde f(a) y f(b) tienen signos opuestos.
  ! ---------------------------------------------------
  ! Bloque de declaración de argumentos
  ! ---------------------------------------------------
  INTERFACE
  REAL(8) FUNCTION f(x) ! Función que define la ecuación
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: x
  END FUNCTION f
  END INTERFACE
  
  REAL(8), INTENT(IN) :: a ! Extremo izquierdo del intervalo inicial
  REAL(8), INTENT(IN) :: b ! Extremo derecho del intervalo inicial
  INTEGER, INTENT(INOUT) :: n ! Límite de iteraciones/iteraciones realizadas
  REAL(8), INTENT(IN) :: tol ! Tolerancia para el error absoluto
  REAL(8), INTENT(OUT) :: raiz ! Aproximación a la raiz
  INTEGER, INTENT(OUT) :: clave ! Clave de éxito:
  ! 0 : éxito
  ! >0 : iteraciones excedidas
  ! <0 : no se puede proceder (f de igual signo
  !en a y b)
  ! ---------------------------------------------------
  ! Bloque de declaración de variables locales
  ! ---------------------------------------------------
  INTEGER :: i
  REAL(8) :: xl, xr, error
  ! ---------------------------------------------------
  ! Bloque de procesamiento
  ! ---------------------------------------------------
  clave = 1
  xl = a
  xr = b
  IF (SIGN(1.0_8,f(xl))*SIGN(1.0_8,f(xr)) > 0.0_8) THEN
  clave = -1
  RETURN
  ENDIF
  DO i=1,n
  error = (xr-xl)*0.5_8
  raiz = xl + error
  IF (error < tol) THEN
  clave = 0
  n = i
  EXIT
  ENDIF
  IF ( SIGN(1.0_8,f(xl))* SIGN(1.0_8,f(raiz)) > 0.0_8) THEN
  xl = raiz
  ELSE
  xr = raiz
  ENDIF
  ENDDO
  ! ---------------------------------------------------
END SUBROUTINE biseccion
