C Subroutine by Dave Pollard
C ***************************** UPPER **********************************
C
      subroutine upper (cin)
 
c        Upper-cases character string cin
 
      character*(*) cin
      
      ia = ichar('a')
      iz = ichar('z')
      ishif = ichar('A') - ichar('a')
      
      do i=1,len(cin)
        if (ichar(cin(i:i)).ge.ia .and. ichar(cin(i:i)).le.iz)
     *    cin(i:i) = char (ichar(cin(i:i)) + ishif)
      enddo
      
      return
      end


