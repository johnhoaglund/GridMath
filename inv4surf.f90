!  Subroutine INV4SURF -- Invert a matrix and provide header 
!                         to write a Golden Software .GRD file
!  UPPER LEFT 1,1 ASSUMED TO CORRESPOND TO XMIN,YMAX
!  REQUIRES SUBROUTINE UPPER TO CONVERT FILENAME STRINGS
!  REQUIRES ZM,NX,NY,DX,DY,XMIN,YMAX,OUTFIL,IOUT
!  RETURNS XMAX,YMIN,ZMIN,ZMAX
      SUBROUTINE INV4SURF(ZM,DX,DY,XMIN,YMIN,XMAX,YMAX, &
ZMIN,ZMAX,NDV,NX,NY,IOUT,OUTFIL) 
      REAL ZM(NX,NY)
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,DX,DY
      REAL NDV
      INTEGER NX,NY
      CHARACTER*(*) OUTFIL
      CHARACTER*4  BINAS
      LOGICAL SET
      XMAX = XMIN + (NX-1)*DX
      YMIN = YMAX - (NY-1)*DY 
! BINARY OR ASCII VARIABLE IS DSAA FOR ASCII OR DSBB FOR BINARY
      BINAS='DSAA'
      CALL upper(OUTFIL)      
      IF ( (OUTFIL(lenchr(OUTFIL)-3:lenchr(OUTFIL))).EQ.'.GRD' ) THEN
        CALL lower(OUTFIL)
        OPEN (IOUT,FILE=(OUTFIL(1:lenchr(OUTFIL))), &
        STATUS='UNKNOWN')
      ELSE
        CALL lower(OUTFIL)
        OPEN (IOUT,FILE=(OUTFIL(1:lenchr(OUTFIL)))//'.grd', &
        STATUS='UNKNOWN')
      ENDIF
! DETERMINE MAX MIN FOR MATRIX
      SET = .FALSE.
      DO 110 J = 1,NY
        DO 100 I = 1,NX
           IF (ZM(I,J).GE.NDV) GOTO 100
           IF (.NOT.SET) THEN
             ZMIN=ZM(I,J)
             ZMAX=ZM(I,J)
             SET = .TRUE.
           ENDIF
           IF (ZM(I,J).GT.ZMAX) ZMAX = ZM(I,J)
           IF (ZM(I,J).LT.ZMIN) ZMIN = ZM(I,J)
 100    CONTINUE
 110  CONTINUE                         
! CONVERT A MATRIX TO A SURFER GRID
 2001 WRITE(IOUT,'(A)') BINAS
      WRITE(IOUT,*) NX,NY
      WRITE(IOUT,*) XMIN,XMAX
      WRITE(IOUT,*) YMIN,YMAX
      WRITE(IOUT,*) ZMIN,ZMAX
! NOTE SURFER GRIDS ARE STORED WITH INCREASING Y FROM TOP DOWN
      DO 2010 J = NY,1,-1
         WRITE(IOUT,*) (ZM(I,J), I = 1,NX)
 2010 CONTINUE
 2500 FORMAT (10F8.1)
! CLOSE FILE AND QUIT
      CLOSE(IOUT)
      RETURN
      END
