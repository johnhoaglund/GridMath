      subroutine GetSurfDim(nx,ny,iary,onefil)
      integer nx,ny,iary
      character*80 onefil
      character*4 binas
! open and read a surfer file to get problem dimensions
      open (iary, file=onefil,status = 'unknown')
! read a matrix formatted as a surfer grid
      read(iary,'(a)') binas

! get dimensions from Surfer binary files
      if ((binas.eq.'dsbb').or.(binas.eq.'DSBB')) then
        close(iary)
        open (iary, file=onefil, form = 'binary',  &
status = 'unknown')
        read(iary) binas
! write(*,*) 're-read binas as ',binas
! write(*,*) 'read first line skipping binas'
        read(iary) nx
!              write(*,*) 'nxs = ',nxs
        read(iary) ny
      endif        
! get dimensions from Surfer ASCII files              
      if ((binas.eq.'dsaa').or.(binas.eq.'DSAA')) then
! skip nx ny read                        
        read(iary,*) nx,ny 
      endif
      close (iary)
      return
      end

