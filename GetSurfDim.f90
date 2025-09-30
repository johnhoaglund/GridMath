      subroutine GetSurfDim(nx,ny,iary,onefil)
      integer nx,ny,iary
      character*80 onefil
      character*4 binas
c open and read a surfer file to get problem dimensions
      open (iary, file=onefil,status = 'unknown')
C read a matrix formatted as a surfer grid
      read(iary,'(a)') binas

c get dimensions from Surfer binary files
      if ((binas.eq.'dsbb').or.(binas.eq.'DSBB')) then
        close(iary)
        open (iary, file=onefil, form = 'binary', 
     $   status = 'unknown')
        read(iary) binas
c write(*,*) 're-read binas as ',binas
c write(*,*) 'read first line skipping binas'
        read(iary) nx
c              write(*,*) 'nxs = ',nxs
        read(iary) ny
      endif        
c get dimensions from Surfer ASCII files              
      if ((binas.eq.'dsaa').or.(binas.eq.'DSAA')) then
c skip nx ny read                        
        read(iary,*) nx,ny 
      endif
      close (iary)
      return
      end

