      Subroutine GetGridMathOption(nz,mo)
      integer nz, mo
c variables written to gridmath.txt
      real constant,scalar, scalbak
c option 12 gradmag, gradazi, node x, node y, point xw, point yw, elev      
      real dip
      real pi
      real gradmag, gradazi
      real x, y
      real xw, yw, elev       
      character*80 outfile
      integer l1,l2
c option 13 number of subdividers
      integer nsd 
c set pi
      pi = 3.14159265359                  
c get nz
      write (*,*) 'Enter the number of Surfer grid files to read, nz'
      write (*,*) 'for Z mathematical processing options. '
      write (*,*) 'Some math options may require more than one grid.'
      write (*,*) 
      write (*,*) 'For the following options, enter 1 for nz'
      write (*,*) '-- Add a constant (+ or -) to modfify input grid.'
      write (*,*) '-- Multiply a grid times scalar x to modify input',
     $                ' grid.'  
      write (*,*) '   ( x > 1.0 or x < 1.0 )'
      write (*,*) '-- Zero negative values.'      
      write (*,*) '-- Smooth a grid using a gradient threshold value'
      write (*,*) '-- Smooth a grid using a step threshold value'
      write (*,*) '-- Create a planar grid using elevation at a point'
      write (*,*) '     and dip and dip direction.  Zero dip will '
      write (*,*) '     create a uniform surface.  '
      write (*,*)       
      write (*,*) 'For the following options, enter 2 for nz'
      write (*,*) '-- Subtract one grid from another, create new.'
      write (*,*) '     example:  create isopach from two surfaces.'
      write (*,*) '-- Add one grid to another, create new '
      write (*,*) '     example:  create new surface by adding isopach',      
     $                 ' to surface.  ' 
      write (*,*) '-- Re-scale thickness between 2 surfaces, '
      write (*,*) '   Re-subtract thickness from upper surface, and '
      write (*,*) '   Output both new bottom surface and new isopach.'
      write (*,*) '-- Multiply one grid by another, create new'
      write (*,*) '     example: make regional scalars or adjust values'  
      write (*,*) '-- Divide one grid by another, create new'
      write (*,*) '   CAUTION:  All 0 divisors will result in 0 values.'
      write (*,*) '     example: make regional scalars or adjust values' 
      write (*,*) '-- Raise bottom surface equal to top surface to '
      write (*,*) '   pinch out any layer < a threshold thickness.'
      write (*,*) '   Output both new bottom surface and new isopach.'
      write (*,*) '-- Evenly divide a layer thickness between two '
      write (*,*) '   surfaces into n+1 layers using n intermediate'
      write (*,*) '   surfaces. Output each of n surfaces. '       
      write (*,*) 
      write (*,*) 'Enter nz (1 or 2; see above): '
      read(*,*) nz
c get mo 
      write (*,*) 'Select an option for processing.'
      write (*,*)                       
      if (nz.eq.1) then
      write (*,*) ' Single grid options'
      write (*,*) '1  Add a constant (+ or -) to modfify input grid.'
      write (*,*) '2  Multiply a grid times scalar x to modify input',
     $                ' grid.'  
      write (*,*) '   ( x > 1.0 or x < 1.0 )'
      write (*,*) '8  Zero negative values.'      
      write (*,*) '9  Smooth a grid using a gradient threshold value'
      write (*,*) '11 Smooth a grid using a step threshold value'
      write (*,*) '12 Create a planar grid using elevation at a point'
      write (*,*) '     and dip and dip direction.  Zero dip will '
      write (*,*) '     create a uniform surface.  '      
      write (*,*)
      write (*,*) 'Enter option number: '
      endif
      if (nz.eq.2) then                               
      write (*,*) ' Double grid options'
      write (*,*) '3  Subtract one grid from another, create new.'
      write (*,*) '     example:  create isopach from two surfaces.'
      write (*,*) '4  Add one grid to another, create new '
      write (*,*) '     example:  create new surface by adding isopach',      
     $                 ' to surface.  ' 
      write (*,*) '5  Re-scale thickness between 2 surfaces, '
      write (*,*) '   Re-subtract thickness from upper surface, and '
      write (*,*) '   Output both new bottom surface and new isopach.'
      write (*,*) '6  Multiply one grid by another, create new'
      write (*,*) '     example: make regional scalars or adjust values'  
      write (*,*) '7  Divide one grid by another, create new'
      write (*,*) '   CAUTION:  All 0 divisors will result in 0 values.'
      write (*,*) '     example: make regional scalars or adjust values' 
      write (*,*) '10 Raise bottom surface equal to top surface to '
      write (*,*) '   pinch out any layer < a threshold thickness.'
      write (*,*) '   Output both new bottom surface and new isopach.'
      write (*,*) '13 Evenly divide a layer thickness between two '
      write (*,*) '   surfaces into n+1 layers using n intermediate'
      write (*,*) '   surfaces. Output each of n surfaces.'                       
      write (*,*)
      write (*,*) 'Enter option number: ' 
      endif  
      read(*,*) mo
c open file to write options (gridmath.txt)
      open (9,file = 'gridmath.txt', status = 'unknown') 

C SINGLE GRID OPTIONS                  
c addition of constant option 1     
      if (mo.eq.1) then
        write (9,*) mo
        do k = 1,nz
          write(*,*) 'Enter constant for grid# ',k
          write(*,*) 'Enter 0 to leave grid unchanged.'
          read(*,*) constant
          write (9,*) constant
        enddo
        close(9)
      endif
c multiplication of scalar option 2     
      if (mo.eq.2) then
        write (9,*) mo
        do k = 1,nz
          write(*,*) 'Enter scalar for grid# ',k
          write(*,*) 'Enter 0 or 1 to leave grid unchanged.'
          read(*,*) scalar
          if (scalar.eq.0) scalar = 1.0
          write (9,*) scalar
        enddo
        close(9)
      endif
c grid negative zeroing, option 8
      if (mo.eq.8) then
        write (9,*) mo
        close(9)
      endif       
c smooth a grid using gradient threshold, option 9
      if (mo.eq.9) then
        write (9,*) mo
        do k = 1,nz
          write(*,*) 'Enter the gradient threshold to smooth grid '
          read(*,*) scalar
          if (scalar.eq.0) scalar = 1.0
          write (9,*) scalar
        enddo
        close(9)
      endif
c smooth a grid using step threshold, option 11
      if (mo.eq.11) then
        write (9,*) mo
        do k = 1,nz
          write(*,*) 'Enter the first-step threshold to smooth grid '
          read(*,*) scalar         
          write (9,*) scalar
          write(*,*) 'Enter the scalar times any backstep needed to '
          write(*,*) 'meet the same threshold to smooth grid.'  
          read(*,*) scalbak
          write(9,*) scalbak
        enddo
        close(9)
      endif
c create a planar surface from an elevation and dip and dip direction, option 12           
      if (mo.eq.12) then
        write (9,*) mo
        write (*,*) 'Enter xw, yw, and elev at the point (or well).'
        write (*,*) 'Elev is the known elevation of the surface '
        write (*,*) 'to be reconstructed at the point (xw, yw)'  
        read (*,*) xw,yw, elev
        write (9,*) xw,yw,elev
        write (*,*) 'Enter the dip angle in degrees (NOT radians).'  
        write (*,*) 'The dip is the arctan of the surface gradient.'
        read (*,*) dip
        gradmag = tan(dip*pi/180)
        write (9,*) gradmag
        write (*,*) 'Enter the azimuth (0 to 360 degrees) of the '
        write (*,*) 'dip direction perpendicular to strike.' 
        write (*,*) '[This is the direction opposite (negative)'
        write (*,*) 'to the surface gradient.]'
c read dip azimuth direction        
        read (*,*) gradazi
c gradient azimuth is the other direction        
        gradazi = gradazi - 180
        write (9,*) gradazi
        write (*,*) 'Enter a Surfer filename to output result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile        
        close(9) 
      endif
C DOUBLE GRID OPTIONS (all double grid options will separately output znew)         
c grid subtraction option 3
      if (mo.eq.3) then
        write (9,*) mo
        write (*,*) 'Enter the layer number to subtract from.'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the layer number to subtract.'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter a Surfer filename to output result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9)
      endif
c grid addition option 4      
      if (mo.eq.4) then
        write (9,*) mo
        write (*,*) 'Enter the layer number to add to.'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the layer number to add.'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter a Surfer filename to output result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9) 
      endif      
c grid scale thickness option 5         
      if (mo.eq.5) then
        write (9,*) mo
        write (*,*) 'Enter the upper surface number.'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the lower surface number.'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter scalar '
        read (*,*) scalar
        write (9,*) scalar
        write (*,*) 'Enter a Surfer filename to output isopach result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9) 
      endif  
c grid multiplication option 6      
      if (mo.eq.6) then
        write (9,*) mo
        write (*,*) 'Enter the layer number to multiply by'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the layer number to multiply'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter a Surfer filename to output result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9) 
      endif 
c grid division option 7      
      if (mo.eq.7) then
        write (9,*) mo
        write (*,*) 'Enter the layer number to divide'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the layer number to divide by'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter a Surfer filename to output result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9) 
      endif      
c raise bottom to pinch any layer less than thickness threshold option 10         
      if (mo.eq.10) then
        write (9,*) mo
        write (*,*) 'Enter the upper surface number.'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the lower surface number.'
        write (*,*) 'This lower surface number will be locally raised.'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter thickness threshold '
        read (*,*) scalar
        write (9,*) scalar
        write (*,*) 'Enter a Surfer filename to output isopach result.'
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9) 
      endif
      if (mo.eq.13) then
        write (9,*) mo
        write (*,*) 'Enter the upper surface number.'
        read(*,*) k1
        write (9,*) k1
        write (*,*) 'Enter the lower surface number.'
        read(*,*) k2
        write (9,*) k2
        write (*,*) 'Enter the number of subdividing surfaces, nsd '
        write (*,*) 'nsd will subdivide the layer into nsd+1 layers:'
        read (*,*) nsd
        write (9,*) nsd
        write (*,*) 'Enter a Surfer filename to output surface results.'
        write (*,*) 'The filename will be appended with surface numbers'
        write (*,*) 'sequentially downward to output each surface.' 
        read(*,'(a)') outfile
        write (9,'(a)') outfile
        close(9) 
      endif                 
c evenly divide a layer thickness between two surfaces into n+1 using n intermediate layers, option 13                                         
      return
      end      


