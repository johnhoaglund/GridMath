            subroutine readsurf(zm2d,nx,ny,iary,
     $      minx,maxx,miny,maxy,minz,maxz,delx,dely,infile)
            character*4 binas
            character*80 infile
            integer nx,ny,iary
            real zm2d(nx,ny)
            real minx,maxx,miny,maxy,minz,maxz
            real delx,dely            
C READ PREVIOUSLY OPENED SURFER GRID FILE (iary)
c test for extension (from readreal subroutine) not needed here
c          if ((extn.eq.'grd').or.(extn.eq.'GRD')) then
c            open (iary, file=infile,status = 'unknown')
C read a matrix formatted as a surfer grid
            read(iary,'(a)') binas
            if ((binas.eq.'dsbb').or.(binas.eq.'DSBB')) then
              close(iary)
              open (iary, file=infile, form = 'binary', 
     $         status = 'unknown')
              read(iary) binas
c              write(*,*) 're-read binas as ',binas
c              write(*,*) 'read first line skipping binas'
              read(iary) nxs
c              write(*,*) 'nxs = ',nxs
              read(iary) nys
c              write(*,*) 'nys = ',nys
              read(iary) xlo
c              write(*,*) 'xlo = ',xlo
              read(iary) xhi
c              write(*,*) 'xhi = ',xhi
              read(iary) ylo
c              write(*,*) 'ylo = ',ylo
              read(iary) yhi
c              write(*,*) 'yhi = ',yhi
              read(iary) zlo
c              write(*,*) 'zlo = ',zlo
              read(iary) zhi 
c              write(*,*) 'zhi = ',zhi
C note surfer grids are stored with increasing y from top down            
              do j = ny,1,-1
                read(iary) (zm2d(i,j), i = 1,nx)
              enddo
c              nx = nxs
c              ny = nys
              minx = real(xlo)
              maxx = real(xhi)
              miny = real(ylo)
              maxy = real(yhi)
              minz = real(zlo)
              maxz = real(zhi)           
              delx = (maxx-minx)/(nx-1)
              dely = (maxy-miny)/(ny-1)
            endif
            if ((binas.eq.'dsaa').or.(binas.eq.'DSAA')) then
c skip nx ny read                        
              read(iary,*) 
              read(iary,*) minx,maxx
              read(iary,*) miny,maxy
              read(iary,*) minz,maxz
C note surfer grids are stored with increasing y from top down
              do j = ny,1,-1
                read(iary,*) (zm2d(i,j), i = 1,nx)
              enddo
              delx = (maxx-minx)/(nx-1)
              dely = (maxy-miny)/(ny-1)              
            endif
            close(iary)
c endif not needed (from readreal extension test)            
c          endif            
            return
            end


