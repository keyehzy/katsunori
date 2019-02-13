MODULE interface

  IMPLICIT NONE

  INTEGER, PARAMETER :: NRIB = 2                !       WIDTH OF THE ZIGZAG NANORIBBON
  INTEGER, PARAMETER :: NGRIDX = 16           !       NUMBER OF POINTS IN THE kx AXIS
  INTEGER, PARAMETER :: NGRIDP = 16 !             !       NUMBER OF POINTS IN THE p AXIS
  INTEGER, PARAMETER :: M = 0        ! SIGN OF THE 'BRANCH': 0 = KATUSNORI, +-1 = MARTINS' BRANCHES
  
  REAL (KIND=8), PARAMETER  :: PI = DACOS(-1.D0)                         !    3.1415...

  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,-NGRIDP:NGRIDP) :: FKNP      !       TRANSCENDENTAL FUNCTION
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,-NGRIDP:NGRIDP) :: FK0      !       TRANS FUNC FROM KATSUNORI  
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,1:100) :: ROOTS       !       ROOTS OF TRANSCENDENTAL FUNCTION
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,1:100) :: ENERGY1     ! ENERGIES
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,1:100) :: ENERGY2
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,1:NRIB) :: ENERGY1_0
  REAL (KIND=8), DIMENSION(-NGRIDX:NGRIDX,1:NRIB) :: ENERGY2_0

  REAL (KIND=8), DIMENSION(-NGRIDP:NGRIDP) :: P                !       QUASI-MOMENTUM P

END MODULE interface
