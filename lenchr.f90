      integer function lenchr(ch)
!     Returns length of character variable ch, less trailing blanks
      character*(*) ch
      do i = len(ch),1,-1
        lenchr = i
        if (ch(i:i).ne.' ' .and. ch(i:i).ne.char(0)) return
      enddo
      return
      end
