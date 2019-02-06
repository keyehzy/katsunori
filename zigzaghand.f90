PROGRAM ZIGZAGHAND

  USE INTERFACE

  IMPLICIT NONE
  
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX) :: AKX       !       WAVE VECTOR IN THE X DIRECTION
  REAL (KIND=8), DIMENSION(-NGRIDP:NGRIDP) :: AKP       !       WAVE VECTOR IN THE Y DIRECTION
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX) :: A0X                     !       AUXILIARY
  REAL (KIND=8), DIMENSION(-NGRIDP:NGRIDP) :: A0P                     !       AUXILIARY

  INTEGER, DIMENSION(-NGRIDX:NGRIDX) :: AUX_N                     
  

  INTEGER :: I1,I2,I3,I,T2,M          !       DO LOOP COUNTERS
  REAL (KIND=8) :: A1,A2,A3,T1,T3        !       AUXILIARY
  LOGICAL :: SIGNAL

  !       THIS PROGRAM CALCULATES THE 'CORRECTED' TRANSCENDENTAL FUNCTION F(k,N,p) DEFINED
  !       IN KATSUNORI'S PAPER [Sci. Technol. Adv. Mater. 11 (2010) 054504]

  !       GENERATING THE VECTORS FOR kx and for p

  A0X  = (/ (DBLE(I), I = -NGRIDX, NGRIDX)  /)
  A0P  = (/ (DBLE(I), I = -NGRIDP, NGRIDP)  /)

  AKX(-NGRIDX:NGRIDX) = A0X(-NGRIDX:NGRIDX) * PI/DBLE(NGRIDX)
  AKP(-NGRIDP:NGRIDP) = A0P(-NGRIDP:NGRIDP) * PI/DBLE(NGRIDP)


  M = 0        ! SIGN

  DO I1 = -NGRIDX, NGRIDX         !       GOES OVER THE WHOLE k_x AXIS
     DO I2 = -NGRIDP, NGRIDP        !       GOES OVER THE QUASI-MOMENTUM p
        A1 = 2.D0 * DCOS(AKX(I1)/2.D0)
        A2 = DSIN(AKP(I2)*DBLE(NRIB+1)+DBLE(M)*AKX(I1)/2.D0)
        !           A2 = DSIN(AKP(I2)*DBLE(NRIB+1))
        A3 = DSIN(AKP(I2)*DBLE(NRIB))
        FKNP(I1,I2) = A1 * A2 + A3
     END DO
  END DO

  AUX_N = 0

  !GETTING THE ROOTS
  DO I2 = -NGRIDX,NGRIDX     
     T1 = 0.D0
     T2 = 1
     T3 = 0.D0
     IF(FKNP(I2,0).LT.0.D0) THEN
        SIGNAL = .TRUE.!.FALSE.
     ELSE
        SIGNAL = .FALSE.!.TRUE.
     END IF
     DO I1 = -NGRIDP, NGRIDP
        IF(SIGNAL .AND. FKNP(I2,I1).LT.T1) THEN
           ROOTS(I2,T2) = 0.5D0*AKP(I1) + 0.5D0*AKP(I1-1)
      !     WRITE(*,'((2(I8.2,1X)), F8.5)') I2, T2, ROOTS(I2, T2)
           T2 = T2+1
           SIGNAL = .NOT.SIGNAL! .FALSE.               !FLIP TO THE NEGATIVE SIGN
        ELSE IF(.NOT.SIGNAL .AND. FKNP(I2,I1).GT.T1) THEN
           ROOTS(I2,T2) = 0.5D0*AKP(I1) + 0.5D0*AKP(I1-1)
       !    WRITE(*,'((2(I8.2,1X)), F8.5)') I2, T2, ROOTS(I2, T2)
           T2 = T2+1
           SIGNAL = .NOT.SIGNAL!.TRUE.                 !FLIP TO THE POSITIVE SIGN
        END IF
     END DO
     AUX_N(I2) = T2 - 1
    ! WRITE(*,*) I2, AUX_N(I2)
  END DO  

  ! CALCULATING THE ENERGY AS IN MARTINS NOTES
  DO I1 = -NGRIDX, NGRIDX
     DO I2 = 1, AUX_N(I1)
        A1 = 4.D0 * DCOS(AKX(I1)/2.D0)**2 + 1.D0
        A2 = 4.D0 * DCOS(AKX(I1)/2.D0)
        ENERGY1(I1,I2) =  DSQRT(A1 + A2*DCOS(ROOTS(I1,I2) + DBLE(M)*AKX(I1)/2.D0))
        ENERGY2(I1,I2) =  -DSQRT(A1 + A2*DCOS(ROOTS(I1,I2) + DBLE(M)*AKX(I1)/2.D0))
     END DO
  END DO

!!$  DO I2 = -NGRIDX, NGRIDX
!!$     WRITE(*,'(65(F18.5,1X))') AKX(I2), (ROOTS(I2,I1),I1=1,AUX_N(I2))
!!$  END DO
!!$
  OPEN(UNIT=4,FILE='./results/new/ROOTS.dat',STATUS='UNKNOWN')
  DO I2 = -NGRIDX,NGRIDX
     WRITE(4,'(65(F8.5,1X))') 0.D0, (ROOTS(I2,I1), I1=1,AUX_N(I2))
  END DO
  CLOSE(4)
!!$
  OPEN(UNIT=3, FILE='./results/new/energy1_0.dat',STATUS='UNKNOWN')
  OPEN(UNIT=4, FILE='./results/new/energy2_0.dat',STATUS='UNKNOWN')
!!$
  DO I1 = -NGRIDX,-NGRIDX
     WRITE(3,'(65(F8.5,1X))') AKX(I1),(ENERGY1(I1,I2),I2=1,AUX_N(I1))
     WRITE(4,'(65(F8.5,1X))') AKX(I1),(ENERGY2(I1,I2),I2=1,AUX_N(I1))
  END DO
!!$  
  CLOSE(3)
  CLOSE(4)
!!$
  OPEN(UNIT=1,FILE='./results/new/zzhand.dat',STATUS='UNKNOWN')
  DO I1 = -NGRIDP, NGRIDP
     WRITE(1,100)AKP(I1),(FKNP(I2,I1),I2=-NGRIDX,NGRIDX)
  END DO
  CLOSE(1)

100 FORMAT(65(F8.5,1X))

END PROGRAM ZIGZAGHAND











!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$ SAME CALCULATIONS FOR THE EXPRESSION IN KATSUNORI
!!$  DO I1 = -NGRIDX, NGRIDX         !       GOES OVER THE WHOLE k_x AXIS
!!$     DO I2 = 0, NGRIDP        !       GOES OVER THE QUASI-MOMENTUM p
!!$        A1 = 2.D0 * DCOS(AKX(I1)/2.D0)
!!$        A2 = DSIN(AKP(I2)*DBLE(NRIB+1))
!!$        A3 = DSIN(AKP(I2)*DBLE(NRIB))
!!$        FK0(I1,I2) = A1 * A2 + A3
!!$     END DO
!!$  END DO
!!$  
!!$  DO I2 = -NGRIDX,NGRIDX     
!!$     T1 = 0.D0
!!$     T2 = 1
!!$     T3 = 0.D0
!!$     SIGNAL = .TRUE.     
!!$     DO I1 = 0, NGRIDP
!!$        IF(SIGNAL .AND. FK0(I2,I1).LT.T1) THEN
!!$           ROOTS(I2,T2) = 0.5D0*AKP(I1) + 0.5D0*AKP(I1-1)
!!$           T2 = T2+1
!!$           SIGNAL = .FALSE.               !FLIP TO THE NEGATIVE SIGN
!!$        ELSE IF(.NOT.SIGNAL .AND. FK0(I2,I1).GT.T1) THEN
!!$           ROOTS(I2,T2) = 0.5D0*AKP(I1) + 0.5D0*AKP(I1-1)
!!$           T2 = T2+1
!!$           SIGNAL = .TRUE.                 !FLIP TO THE POSITIVE SIGN
!!$        END IF
!!$     END DO
!!$  END DO
!!$  DO I1 = -NGRIDX,NGRIDX
!!$     DO I2 = 1,NRIB
!!$        A1 = 4.D0 * DCOS(AKX(I1)/2.D0)**2 + 1.D0
!!$        A2 = 4.D0 * DCOS(AKX(I1)/2.D0)
!!$        ENERGY1_0(I1,I2) =  DSQRT(A1 + A2*DCOS(ROOTS(I1,I2)))
!!$        ENERGY2_0(I1,I2) =  -DSQRT(A1 + A2*DCOS(ROOTS(I1,I2)))
!!$     END DO
!!$  END DO
!!$
!!$  OPEN(UNIT=3, FILE='energy1_0.dat',STATUS='UNKNOWN')
!!$  OPEN(UNIT=4, FILE='energy2_0.dat',STATUS='UNKNOWN')
!!$  DO I1 = 0,NGRIDX
!!$     WRITE(3,'(10(F8.5,1X))') AKX(I1),(ENERGY1_0(I1,I2),I2=1,NRIB)
!!$     WRITE(4,'(10(F8.5,1X))') AKX(I1),(ENERGY2_0(I1,I2),I2=1,NRIB)
!!$  END DO
!!$
!!$  DO I1 = -NGRIDX,-1
!!$     WRITE(3,'(3(F8.5,1X))') AKX(I1),(ENERGY1_0(I1,I2),I2=1,NRIB)
!!$     WRITE(4,'(3(F8.5,1X))') AKX(I1),(ENERGY2_0(I1,I2),I2=1,NRIB)
!!$  END DO
!!$  CLOSE(3)
!!$  CLOSE(4)
!!$
!!$  OPEN(UNIT=4,FILE='roots_0.dat',STATUS='UNKNOWN') !8 roots for each k OK
!!$  DO I1 = 1,NRIB
!!$     WRITE(4,'(65(F8.5))') 0.D0,(ROOTS(I2,I1),I2=0,NGRIDX)
!!$  END DO
!!$  CLOSE(4)
!!$
!!$  OPEN(UNIT=1,FILE='zzhand_0.dat',STATUS='UNKNOWN')
!!$  DO I1 = 0, NGRIDP
!!$     WRITE(1,100)AKP(I1),(FK0(I2,I1),I2=0,NGRIDX)
!!$     !WRITE(6,*)AKP(I1),(FKNP(I2,I1),I2=0,NGRIDX)
!!$  END DO
!!$  CLOSE(1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
