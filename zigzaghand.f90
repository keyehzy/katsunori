PROGRAM ZIGZAGHAND
  USE OGPF
  USE INTERFACE
  USE CHEBYINTER, ONLY: INTERPOLATE
!  USE TRISECT_LIB, ONLY: TRISECT
  USE PLOT, ONLY: WRITE_RESULT

  IMPLICIT NONE

  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX) :: AKX       !WAVE VECTOR IN THE X DIRECTION
  REAL (KIND=8), DIMENSION(-NGRIDP:NGRIDP) :: AKP       !WAVE VECTOR IN THE Y DIRECTION
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX) :: A0X       !AUXILIARY
  REAL (KIND=8), DIMENSION(-NGRIDP:NGRIDP) :: A0P       !AUXILIARY

  REAL (KIND=8), DIMENSION(1:9,1:8) :: REFERENCE
  REAL (KIND=8), DIMENSION(1:9) :: REF

  INTEGER, DIMENSION(-NGRIDX:NGRIDX) :: AUX_N

  INTEGER :: I1,I2,I3,I,T2          !DO LOOP COUNTERS
  REAL (KIND=8) :: A1,A2,A3,T1,T3,R     !AUXILIARY
  LOGICAL :: SIGNAL                   !AUX FOR ROOT FINDING

  TYPE(GPF) :: MATPLOT                !GNUPLOT EXTENSION

  !THIS PROGRAM CALCULATES THE 'CORRECTED' TRANSCENDENTAL FUNCTION F(k,N,p) DEFINED
  !IN KATSUNORI'S PAPER [Sci. Technol. Adv. Mater. 11 (2010) 054504]

  !GENERATING THE VECTORS FOR kx and for p
  A0X  = (/ (DBLE(I), I = -NGRIDX, NGRIDX)  /)
  A0P  = (/ (DBLE(I), I = -NGRIDP, NGRIDP)  /)

  AKX(-NGRIDX:NGRIDX) = A0X(-NGRIDX:NGRIDX) * PI/DBLE(NGRIDX)
  AKP(-NGRIDP:NGRIDP) = A0P(-NGRIDP:NGRIDP) * PI/DBLE(NGRIDP)

  !DEFINING THE 'CORRECTED' TRANSCENDENTAL FUNCTION
  DO I1 = -NGRIDX, NGRIDX         !       GOES OVER THE WHOLE k_x AXIS
     DO I2 = -NGRIDP, NGRIDP        !       GOES OVER THE QUASI-MOMENTUM p
        FKNP(I1,I2) = FTRANS_MARTINS4(AKX(I1),AKP(I2),NRIB,M)
     END DO
  END DO

  ROOTS = 0.D0

  !GETTING ROOTS OF TRANSCENDENTAL EQUATION BY FROBENIUS-CHEBYSHEV COMPANION MATRIX
  DO I1 = -NGRIDX, NGRIDX
     CALL INTERPOLATE(-PI+1.D-6,PI-1.D-6,FTRANS_MARTINS4, NGRIDX, I1, AKX(I1), NRIB, M, ROOTS)
  END DO  
  
  !CALCULATING THE ENERGY AS IN MARTINS NOTES
  DO I1 = -NGRIDX, NGRIDX
     DO I2 = 1, NRIB
        ENERGY1(I1,I2) = ENERGY_MARTINS4(AKX(I1), ROOTS(I1,I2), M)
        ENERGY2(I1,I2) =  -ENERGY_MARTINS4(AKX(I1), ROOTS(I1,I2),M)
     END DO
  END DO

!!$ NUMERICAL REFERENCE
!!$  OPEN(UNIT=1,FILE='bandsN=1.dat',STATUS='OLD', ACTION='READ')
!!$  DO I1=1,9
!!$     READ(1,*) REF(I1), (REFERENCE(I1,I2),I2=1,8)
!!$  END DO
!!$  CLOSE(1)
!!$

!!$ GNUPLOT SCRIPTING
!  CALL MATPLOT%OPTIONS('set xrange[0:3.15]; set yrange[0:3]')
!!$  CALL MATPLOT%MULTIPLOT(1,2)
!  CALL MATPLOT%PLOT(AKX,ENERGY1,'w l')
!!$  CALL MATPLOT%PLOT(AKX,FKNP, 'w l')
!!$  CALL MATPLOT%PLOT(REF,REFERENCE, 'w l')

  CALL WRITE_RESULT(AKX, FKNP, 'transcendental')
  CALL WRITE_RESULT(AKX, ENERGY1, 'energy1')
  CALL WRITE_RESULT(AKX, ENERGY2, 'energy2')
  CALL WRITE_RESULT( 0.D0 * AKX, ROOTS, 'roots')


  !FOR FUTURE REFERENCE I'LL BE USING N=1, NGRID=8, K=5, AND CHOOSING THE 2ND SITE OF THE UNIT TO COMPARE
  !THE WAVEFUNCTIONS

!!$  NORMA = 0.D0
!!$  PSI_A = 0.D0
!!$
!!$  OPEN(UNIT=1,FILE='states_analytic.dat',STATUS='UNKNOWN')
!!$
!!$  I = 0
!!$  DO I1=3,3
!!$     DO I2 = 1,AUX_N(I1)
!!$        DO I3 = 1,NRIB
!!$           NORMA(I2) = NORMA(I2) + 2.D0*DSIN(DBLE(I3)*ROOTS(I1,I2))**2
!!$        END DO   
!!$        PSI_A(I1,I2) = ABS(-DSIN(ROOTS(I1,I2)*(NRIB+1-32))/DSQRT(NORMA(I2)))**2
!!$        I = I+1
!!$        WRITE(1,*) I, PSI_A(I1,I2)
!!$     END DO
!!$  END DO
!!$  CLOSE(1)
           
100 FORMAT(65(F8.5,1X))

  
CONTAINS

  FUNCTION FTRANS_MARTINS4(K, P, N, BETA) RESULT(J)
    REAL(KIND=8) :: K, P
    INTEGER :: N, BETA
    REAL(KIND=8) :: J
    
    J = 4.D0 * DSIN(P*DBLE(N+1)) * DCOS(K/2.D0)**2 + &
         &(4.D0 * DCOS(K/2.D0)**2 + 2 * DBLE(BETA) * ABS(2.D0*DCOS(K/2.D0)*DCOS(P/2.D0))) * DSIN(P*DBLE(N))

    RETURN
  END FUNCTION FTRANS_MARTINS4

    FUNCTION ENERGY_MARTINS4(K, P, BETA) RESULT(J)
    REAL(KIND=8) :: K,P
    INTEGER :: BETA
    REAL(KIND=8) :: J

    J = DSQRT(4.D0 * DCOS(K/2.D0)**2 + DBLE(BETA) * ABS(4.D0*DCOS(K/2.D0)*DCOS(P/2.D0)) + 1.D0)

    RETURN
  END FUNCTION ENERGY_MARTINS4
  
  FUNCTION FTRANS_MARTINS(K, P, N, M) RESULT(J)
    REAL(KIND=8) :: K, P
    INTEGER :: N, M
    REAL(KIND=8) :: J
    
    J = 2.D0 * DCOS(K/2.D0) * DSIN(P*DBLE(N+1)+DBLE(M)*K/2.D0) + DSIN(P*DBLE(N))

    RETURN
  END FUNCTION FTRANS_MARTINS

  FUNCTION ENERGY_MARTINS(K, P, M) RESULT(J)
    REAL(KIND=8) :: K,P
    INTEGER :: M
    REAL(KIND=8) :: J

    J = DSQRT(4.D0 * DCOS(K/2.D0)**2 + 1.D0 + 4.D0 * DCOS(K/2.D0)*DCOS(P + DBLE(M)*K/2.D0))

    RETURN
  END FUNCTION ENERGY_MARTINS

  !JUST SUBSTITUTE THESE FOR THE ACTUAL FUNCTION TO OBTAIN THE ANALYTICAL CONTINUATION
  FUNCTION ZFTRANS_MARTINS(K, ETTA, N, M) RESULT(J)
    REAL(KIND=8) :: K, ETTA
    INTEGER :: N, M
    REAL(KIND=8) :: J

    J = 2.D0 * DCOS(K/2.D0) * DSINH(ETTA*DBLE(N+1)+DBLE(M)*K/2.D0) + DSINH(ETTA*DBLE(N))

    RETURN
  END FUNCTION ZFTRANS_MARTINS

  FUNCTION ZENERGY_MARTINS(K, ETTA, M) RESULT(J)
    REAL(KIND=8) :: K, ETTA
    INTEGER :: M
    REAL(KIND=8) :: J

    J = DSQRT(4.D0 * DCOS(K/2.D0)**2 + 1.D0 + 4.D0 * DCOS(K/2.D0)*DCOSH(ETTA + DBLE(M)*K/2.D0))

    RETURN
  END FUNCTION ZENERGY_MARTINS
END PROGRAM ZIGZAGHAND

!!$  !GETTING THE ROOTS BY BISECTION
!!$  AUX_N = 0 !COUNTER FOR THE NUMBER OF ROOTS
!!$  DO I2 = -NGRIDX,NGRIDX
!!$     T1 = 0.D0
!!$     T2 = 1
!!$     T3 = 0.D0
!!$     IF(FKNP(I2,0).LT.0.D0) THEN
!!$        SIGNAL = .TRUE.!.FALSE.
!!$     ELSE
!!$        SIGNAL = .FALSE.!.TRUE.
!!$     END IF
!!$     DO I1 = -NGRIDP,NGRIDP
!!$        IF(SIGNAL .AND. FKNP(I2,I1) .LT. T1) THEN
!!$           CALL TRISECT(FTRANS_MARTINS4, AKX(I2), NRIB, M, AKP(I1-1)-1.D-15, AKP(I1)+1.D-15,R)
!!$           ROOTS(I2,T2) = R
!!$           !ROOTS(I2,T2) = 0.5D0*AKP(I1-1) + 0.5D0*AKP(I1) !NAIVE
!!$           T2 = T2+1
!!$           SIGNAL = .NOT.SIGNAL! .FALSE.               !FLIP TO THE NEGATIVE SIGN
!!$        ELSE IF(.NOT.SIGNAL .AND. FKNP(I2,I1) .GT. T1) THEN
!!$           CALL TRISECT(FTRANS_MARTINS4, AKX(I2), NRIB, M, AKP(I1-1)-1.D-15, AKP(I1)+1.D-15, R)
!!$           ROOTS(I2,T2) = R
!!$           !ROOTS(I2,T2) = 0.5D0*AKP(I1-1) + 0.5D0*AKP(I1) !NAIVE
!!$           T2 = T2+1
!!$           SIGNAL = .NOT.SIGNAL!.TRUE.                 !FLIP TO THE POSITIVE SIGN
!!$        END IF
!!$     END DO
!!$     AUX_N(I2) = T2 - 1
!!$  END DO
!!$
