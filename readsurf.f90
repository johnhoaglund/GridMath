            subroutine readsurf(zm2d,nx,ny,iary, &
minx,maxx,miny,maxy,minz,maxz,delx,dely,infile)
            character*4 binas
            character*80 infile
            integer nx,ny,iary
            real zm2d(nx,ny)
            real minx,maxx,miny,maxy,minz,maxz
            real delx,dely            
! READ PREVIOUSLY OPENED SURFER GRID FILE (iary)
! test for extension (from readreal subroutine) not needed here
!          if ((extn.eq.'grd').or.(extn.eq.'GRD')) then
!            open (iary, file=infile,status = 'unknown')
! read a matrix formatted as a surfer grid
            read(iary,'(a)') binas
            if ((binas.eq.'dsbb').or.(binas.eq.'DSBB')) then
              close(iary)
              open (iary, file=infile, form = 'binary',  &
status = 'unknown')
              read(iary) binas
!              write(*,*) 're-read binas as ',binas
!              write(*,*) 'read first line skipping binas'
              read(iary) nxs
!              write(*,*) 'nxs = ',nxs
              read(iary) nys
!              write(*,*) 'nys = ',nys
              read(iary) xlo
!              write(*,*) 'xlo = ',xlo
              read(iary) xhi
!              write(*,*) 'xhi = ',xhi
              read(iary) ylo
!              write(*,*) 'ylo = ',ylo
              read(iary) yhi
!              write(*,*) 'yhi = ',yhi
              read(iary) zlo
!              write(*,*) 'zlo = ',zlo
              read(iary) zhi 
!              write(*,*) 'zhi = ',zhi
! note surfer grids are stored with increasing y from top down            
              do j = ny,1,-1
                read(iary) (zm2d(i,j), i = 1,nx)
              enddo
!              nx = nxs
!              ny = nys
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
! skip nx ny read                        
              read(iary,*) 
              read(iary,*) minx,maxx
              read(iary,*) miny,maxy
              read(iary,*) minz,maxz
! note surfer grids are stored with increasing y from top down
              do j = ny,1,-1
                read(iary,*) (zm2d(i,j), i = 1,nx)
              enddo
              delx = (maxx-minx)/(nx-1)
              dely = (maxy-miny)/(ny-1)              
            endif
            close(iary)
! endif not needed (from readreal extension test)            
!          endif            
            return
            end


