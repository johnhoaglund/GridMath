      subroutine GridMathFillZ(nx,ny,nz,mo)
c first occurence in program of full 3D array zm3d      
      real zm3d(nx,ny,nz)
      integer nx,ny,nz,mo
      real minx,maxx,miny,maxy,minz,maxz,delx,dely  
      real ndv   
      integer iout
      character*80 outfile,infile      
c open filelist file
      open (7, file = 'GridMathInFiles.txt', status = 'unknown') 
      open (5, file = 'GridMathOutFiles.txt', status = 'unknown')     
      do k = 1,nz
        iary = k + 10
c get input filename for surface from list file for iary = k + 10
        read (7,'(a)') infile
c open file for layer.  Send its name to readsurf 
        open (iary, file=infile,status = 'unknown')                            
c note:  subroutine readsurf reads previously opened files (iary)                  
        call readsurf(zm3d(1,1,k),nx,ny,iary,
     $      minx,maxx,miny,maxy,minz,maxz,delx,dely,infile)
      enddo
c note:  subroutine readsurf closes all input files (iary)
c all input files opened, read, and closed
c close input files list file
      close (7)    
c now process math
      call GridMath(zm3d,nx,ny,nz,mo,minx,maxx,miny,maxy,minz,maxz,
     $              delx,dely)
c zm3d array processing now complete / revised
c write output zm3d arrays
c set ndv (null data value) above which Surfer does not process
      ndv = 1.70141E38
      do k = 1,nz
        iout = k + 10
c get filename for iout output file        
c        write(*,*) 'Enter the Surfer output filename for grid #',i
c continue reading file list for output files
        read(5,'(A)') outfile
c note:  subroutine inv4surf needs subroutine upper to be linked in        
c note:  subroutine inv4surf opens iout        
        call inv4surf(zm3d(1,1,k),delx,dely,minx,miny,maxx,maxy,
     $    minz,maxz,ndv,nx,ny,iout,outfile)
c note:  subroutine inv4surf closes iout
      enddo 
c all output files opened, read, and closed
c close output files list file
      close (5)              
      return
      end


