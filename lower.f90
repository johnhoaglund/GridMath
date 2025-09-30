! SUBROUTINE BY DAVE POLLARD
! ***************************** LOWER **********************************
!
      subroutine lower (cin)
 
!        Lower-cases character string cin
 
      character*(*) cin
 
      ia = ichar('A')
      iz = ichar('Z')
      ishif = ichar('a') - ichar('A')
 
      do i=1,len(cin)
        if (ichar(cin(i:i)).ge.ia .and. ichar(cin(i:i)).le.iz) &
          cin(i:i) = char (ichar(cin(i:i)) + ishif)
      enddo
 
      return
      end


