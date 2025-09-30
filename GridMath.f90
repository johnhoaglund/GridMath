      subroutine GridMath(zm3d,nx,ny,nz,mo,minx,maxx,miny,maxy,minz, &
maxz,delx,dely)
! arrays      
      real zm3d(nx,ny,nz),znew(nx,ny)
! dimensions and option flag     
      integer nx,ny,nz,mo,nmo
! domain variables     
      real minx,maxx,miny,maxy,minz,maxz,delx,dely
      real ndv
! option 2 or 5 multiplicative scalar or option 9 threshold gradient, gradient, and change in z
      real scalar, gradient, delz, step1, step2, step3, scalbak
      real avgrad, mxgrad
      real avstep, mxstep
! option 12 gradmag, gradazi, node x, node y, point xw, point yw, elev
      real pi
      real gradmag, gradazi
      real x, y
      real xw, yw, elev 

! smoothing interval and counter
      integer istrt, iend, kount            
! addition constant
      real constant
! output file for options 3 and 4
      integer iout
      character*80 outfile,outnfile
! option 13 
      integer nsd, n
      real tsd
      character*3 cn,buffr      
! set pi
      pi = 3.14159265359       
! set the Surfer null data value above which Surfer does not process
      ndv = 1.70141E38                  
! read the option file      
      open (9,file = 'gridmath.txt', status = 'unknown') 
! read option and assure match
      read (9,*) nmo
      if (nmo.ne.mo) then
        close(9) 
        open (8,file = 'GridMathOptionError.txt', status = 'unknown')
        write (8,*) 'Option read from file does not match option'
        write (8,*) 'option selected in program. Old gridmath.txt?'
        close(8)
        goto 1000
      endif  
! SINGLE GRID OPTIONS      

! addition of constant option 1      
      if (mo.eq.1) then
        do k = 1,nz
!     read constant for layer k       
          read (9,*) constant
          do j = 1,ny
            do i = 1,nx
              zm3d(i,j,k) = zm3d(i,j,k) + constant
            enddo
          enddo
        enddo
        close(9)
      endif

! multiplication of scalar option 2 
      if (mo.eq.2) then
        do k = 1,nz
!     read scalar for layer k
          read (9,*) scalar
          do j = 1,ny
            do i = 1,nx 
                zm3d(i,j,k) = zm3d(i,j,k)*scalar
            enddo
          enddo
        enddo
        close(9)
      endif

! grid zeroing negatives option 8      
      if (mo.eq.8) then      
        do k = 1,nz
          do j = 1,ny
            do i = 1,nx
              if (zm3d(i,j,k).lt.0.) zm3d(i,j,k) = 0.
            enddo
          enddo
        enddo
        close(9)
      endif       

! smoothing with gradient threshold option 9      
      if (mo.eq.9) then
        do k = 1,nz
! read scalar (gradient threshold) for layer k
          read (9,*) scalar
          do j = 1,ny
            kount = 0
            mxgrad = 0.
            avgrad = 0.
            do i = 1,nx-1
              gradient = (zm3d(i+1,j,k)-zm3d(i,j,k))/delx
              if (gradient.gt.mxgrad) mxgrad = gradient
              avgrad = avgrad + gradient
              kount = kount + 1
              if (gradient.gt.scalar) then
                istrt = i-5
                if (istrt.lt.1) istrt = 1
                iend = i+5 
                if (iend.gt.nx) iend = nx
                write (*,*) 'correcting'
                do ii = istrt+1,iend-1                              
                  delz = (zm3d(iend,j,k)-zm3d(istrt,j,k) )
                  zm3d(ii,j,k) = zm3d(istrt,j,k) + delz*                  &
(real(ii-istrt)/real(iend-istrt))
                  write(*,*) zm3d(ii,j,k)
                enddo
              endif                                                
            enddo
            avgrad = avgrad / kount
            write (*,*) 'average gradient ',avgrad
            write (*,*) 'maximum gradient ', mxgrad
          enddo
        enddo
        close(9)
      endif  
      
! smoothing with step threshold option 11      
      if (mo.eq.11) then
        do k = 1,nz
! read scalar (step threshold) for layer k
          read (9,*) scalar
          read (9,*) scalbak
! PROCESS ROWS FIRST          
          do j = 1,ny
            kount = 0
            mxstep = 0.
            avstep = 0.
            do i = 1,nx-3
              step1 = zm3d(i+1,j,k)-zm3d(i,j,k)
              step2 = zm3d(i+2,j,k)-zm3d(i+1,j,k)
              step3 = zm3d(i+3,j,k)-zm3d(i+2,j,k)
! track statistics on step1
              if (abs(step1).gt.mxstep) mxstep = abs(step1)
              avstep = avstep + abs(step1)
              kount = kount + 1
! meet the minimum threshold for smoothing               
              if (abs(step1).gt.scalar) then                           
! identify backsteps with respects to the first step
! for second step
                if ( ((step1.gt.0).and.(step2.lt.0)).or. &
((step1.lt.0).and.(step2.gt.0)) ) then
                   step2 = abs(step2)
                else
                   step2 = 0
                endif
! for third step               
                if ( ((step1.gt.0).and.(step3.lt.0)).or. &
((step1.lt.0).and.(step3.gt.0)) ) then
                   step3 = abs(step3)
                else
                   step3 = 0
                endif
! take absolute value of first step for magnitude comparison              
                step1 = abs(step1)
! now compare magnitudes of scalar threshold and any backsteps*scalbak and correct for large enough backsteps                  
                if ((scalar.lt.scalbak*step2).or.  &
(scalar.lt.scalbak*step3)) then
                  write (*,*) 'correcting'
                  zm3d(i+1,j,k) = zm3d(i,j,k) 
                  write(*,*) zm3d(i+1,j,k)
! endif backstep threshold is met                  
                endif
! endif minimum threshold met                
              endif 
! enddo for col loop                                                             
            enddo
            avstep = avstep / kount
            write (*,*) 'average step ',avstep
            write (*,*) 'maximum step ', mxstep
! enddo for row loop               
          enddo
! PROCESS COLUMNS SECOND
          do i = 1,nx
            kount = 0
            mxstep = 0.
            avstep = 0.
            do j = 1,ny-3
              step1 = zm3d(i,j+1,k)-zm3d(i,j,k)
              step2 = zm3d(i,j+2,k)-zm3d(i,j+1,k)
              step3 = zm3d(i,j+3,k)-zm3d(i,j+2,k)
! track statistics on step1
              if (abs(step1).gt.mxstep) mxstep = abs(step1)
              avstep = avstep + abs(step1)
              kount = kount + 1
! meet the minimum threshold for smoothing               
              if (abs(step1).gt.scalar) then                           
! identify backsteps with respects to the first step
! for second step
                if ( ((step1.gt.0).and.(step2.lt.0)).or. &
((step1.lt.0).and.(step2.gt.0)) ) then
                   step2 = abs(step2)
                else
                   step2 = 0
                endif
! for third step               
                if ( ((step1.gt.0).and.(step3.lt.0)).or. &
((step1.lt.0).and.(step3.gt.0)) ) then
                   step3 = abs(step3)
                else
                   step3 = 0
                endif
! take absolute value of first step for magnitude comparison              
                step1 = abs(step1)
! now compare magnitudes of scalar threshold and any backsteps*scalbak and correct for large enough backsteps                  
                if ((scalar.lt.scalbak*step2).or.  &
(scalar.lt.scalbak*step3)) then
                  write (*,*) 'correcting'
                  zm3d(i,j+1,k) = zm3d(i,j,k) 
                  write(*,*) zm3d(i,j+1,k)
! endif backstep threshold is met                  
                endif
! endif minimum threshold met                
              endif 
! enddo for col loop                                                             
            enddo
            avstep = avstep / kount
            write (*,*) 'average step ',avstep
            write (*,*) 'maximum step ', mxstep
! enddo for col loop               
          enddo
         
! enddo for lay loop             
        enddo
        close(9)
      endif  

! create surface with a point and dip and dip direction option 12                     
      if (mo.eq.12) then
        read (9,*) xw,yw,elev
        read (9,*) gradmag
        read (9,*) gradazi
! debug
        write (*,*) xw,yw,elev
        write (*,*) gradmag
        write (*,*) gradazi
! enddebug                
! warn for set points outside of domain
        if ( (xw.lt.minx).or.(xw.gt.maxx).or.         &
(yw.lt.miny).or.(yw.gt.maxy) ) then
          write(*,*) 'Warning, set point xw = ',xw,' yw = ',yw
          write(*,*) 'is outside the domain.'
          write(*,*) 'It will still be used to calculate the surface '
          write(*,*) 'inside the domain.'
        endif
        k = 1
        do j = 1,ny
          do i = 1,nx
! calculate the x and y of the node based on minx,miny and cell count
            x = minx + (i-1)*delx
            y = miny + (ny-j)*dely
! for this node, calculate initial regional head by adding draw-down / draw-up to initial head at well #1
            zm3d(i,j,k) = elev + gradmag*(  (x-xw)*sin(gradazi*pi/180) &
+ (y-yw)*cos(gradazi*pi/180) )
! next column
          enddo
! next row          
        enddo
      endif        


! DOUBLE GRID OPTIONS   

! grid subtraction option 3
      if (mo.eq.3) then
        read(9,*) k1
        read(9,*) k2
        read(9,'(a)') outfile
        do j = 1,ny
          do i = 1,nx
            znew(i,j) = zm3d(i,j,k1) - zm3d(i,j,k2)     
          enddo
        enddo
        close(9)
!     note:  subroutine inv4surf opens iout           
        iout = 9
        call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf closes iout   
      endif

! grid addition option 4      
      if (mo.eq.4) then
        read(9,*) k1
        read(9,*) k2
        read(9,'(a)') outfile
        do j = 1,ny
          do i = 1,nx
            znew(i,j) = zm3d(i,j,k1) + zm3d(i,j,k2)     
          enddo
        enddo
        close(9)
!     note:  subroutine inv4surf opens iout           
        iout = 9
        call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf closes iout      
      endif  

! grid scaling of grid subtraction; resubtraction,  option 5          
      if (mo.eq.5) then
        read (9,*) k1
        read (9,*) k2
        read (9,*) scalar
        read(9,'(a)') outfile
        do j = 1,ny
          do i = 1,nx
            znew(i,j) = zm3d(i,j,k1) - zm3d(i,j,k2) 
            znew(i,j) = znew(i,j)*scalar
            zm3d(i,j,k2) = zm3d(i,j,k1) - znew(i,j)   
          enddo
        enddo
        close(9)
!     note:  subroutine inv4surf opens iout           
        iout = 9
        call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf closes iout        
      endif

! grid multiplication option 6      
      if (mo.eq.6) then
        read(9,*) k1
        read(9,*) k2
        read(9,'(a)') outfile
        do j = 1,ny
          do i = 1,nx
            znew(i,j) = zm3d(i,j,k1) * zm3d(i,j,k2)     
          enddo
        enddo
        close(9)
!     note:  subroutine inv4surf opens iout           
        iout = 9
        call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf opens iout        
      endif                        

! grid division option 7      
      if (mo.eq.7) then
        read(9,*) k1
        read(9,*) k2
        read(9,'(a)') outfile
        do j = 1,ny
          do i = 1,nx
            if (zm3d(i,j,k2).eq.0) then
              znew(i,j) = 0.
            else
              znew(i,j) = zm3d(i,j,k1) / zm3d(i,j,k2)     
            endif
          enddo
        enddo
        close(9)
!     note:  subroutine inv4surf opens iout           
        iout = 9
        call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf opens iout        
      endif 

! raise bottom to pinch any layer less than thickness threshold option 10    
      if (mo.eq.10) then
        read (9,*) k1
        read (9,*) k2
        read (9,*) scalar
        read(9,'(a)') outfile
        do j = 1,ny
          do i = 1,nx
            znew(i,j) = zm3d(i,j,k1) - zm3d(i,j,k2) 
            if (znew(i,j).lt.0) znew(i,j) = 0
            if ((znew(i,j).lt.scalar).and.(znew(i,j).gt.0)) then
!             if (zm3d(i,j,k1).gt.zm3d(i,j,k2)) zm3d(i,j,k2)=zm3d(i,j,k1) 
              zm3d(i,j,k2)=zm3d(i,j,k1) 
            endif
          enddo
        enddo
        close(9)
!     note:  subroutine inv4surf opens iout           
        iout = 9
        call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf closes iout        
      endif  
      
! grid scaling of grid subtraction; resubtraction,  option 13          
      if (mo.eq.13) then
        read (9,*) k1
        read (9,*) k2
        read (9,*) nsd
        read(9,'(a)') outnfile       
        do n = 1,nsd
! construct output filename from dividing surface number n
          write(buffr,'(I3)') n
          read(buffr,'(A)') cn
          outfile = trim(cn(nfirstchr(cn):3))//'_'//trim(outnfile)
          do j = 1,ny
            do i = 1,nx
              tsd = zm3d(i,j,k1) - zm3d(i,j,k2) 
              tsd = tsd/(nsd+1)
              znew(i,j) = zm3d(i,j,k1) - n*tsd   
            enddo
          enddo
          close(9)
!     note:  subroutine inv4surf opens iout           
          iout = 9
          call inv4surf(znew,delx,dely,minx,miny,maxx,maxy, &
minz,maxz,ndv,nx,ny,iout,outfile)
!     note:  subroutine inv4surf closes iout   
        enddo     
      endif                                        

 1000 continue      
      return
      end
