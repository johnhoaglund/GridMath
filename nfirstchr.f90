      integer function nfirstchr(ch)
c     Returns the position of the first character in variable ch, 
c     skipping leading blanks
      character*(*) ch
      do i = 1,len(ch)
        nfirstchr = i
        if (ch(i:i).ne.' ' .and. ch(i:i).ne.char(0)) return
      enddo
      return
      end
