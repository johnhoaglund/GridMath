      Program GridMathMain
      integer nx, ny, nz, mo
      character*80 infile
      character*80 onefil
c get GridMath option (mo) and corresponding minimum number of grids to open (nz)
      call GetGridMathOption(nz,mo)
c get and list grid filenames
c open input files list file 
      open (7, file = 'GridMathInFiles.txt', status = 'unknown')
c open output files list file
      open (5, file = 'GridMathOutFiles.txt', status = 'unknown')           
      do k = 1,nz
        write(*,*) 'Enter the Surfer input filename for grid #',k
        read(*,'(A)') infile
        if (k.eq.1) onefil = infile        
        write (7,'(a)') infile
        write (5,'(a)') 'new_'//infile 
      enddo
c close the file list files to be re-opened in GridMathFillZ
c GetSurfDim will open onefil     
      close (7)
      close (5)
c set iary for GetSurfDim file open of onefil
      iary = 11      
c get domain dimensions nx,ny from iary Surfer file (re-opened)      
      call GetSurfDim(nx,ny,iary,onefil)
      call GridMathFillZ(nx,ny,nz,mo)
      stop
      end      


