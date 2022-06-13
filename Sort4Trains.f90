program hello


  
  ! This is a comment line; it is ignored by the compiler
    
    
    !Gets the number of lines in the file
    character(100) :: arg1
    character(100) :: arg2
    integer :: stat
    integer :: dentLevel = 1 !Keeps track of the level of ident
    integer :: checks = 0
    integer :: lockLevel = 0
    integer :: canLock = 1
    integer :: startLock = 0
    integer :: tempIndex = 0
    integer :: failureCount = 0
    integer :: iterCount = 0

    real :: percentChange
    ! 1 is no indent

    character(102), dimension (:), allocatable :: starry
    character(200) :: exec
    character(100) :: line
    character(100) :: temp

    call get_command_argument(number=1, value=arg1, status=stat)
    print *, arg1
    call get_command_argument(number=2, value=arg2, status=stat)
    print *, arg2




  nlines = 0
  open (2, file = arg2, status = 'old')
  do
      read (2,*,iostat=io)
      if (io /= 0) exit
      nlines = nlines + 1
  end do
  close(2)

  allocate(character(102) :: starry(nlines))


  open(2, file = arg2, status = 'old')

  !Adds newlines to everything and preps for being executes later
  do i = 1,nlines
    read(2,'(A)') line
    starry(i) = achar(i+64)//" = """//trim(Replace_Text(Replace_Text(Replace_Text(Replace_Text(Replace_Text(Replace_Text( & ! 0n1y 1337 hax0r5 c4n und3rst4nd th1s l1n3 y0u n00b
    line,"\""",'(Q)'),'\n','(?)'),'(?)','\\n'),'"','(!)'),"(!)","\"""),'(Q)',"\\\"""))//"\n"""
    !print *, line
    
  end do
  close(2)

  !You have to trim when concating like this trim(trim(a)\\trim(b))

  dentLevel = getIndentLevel(starry(1))

  print *, dentLevel



  dentLevel = getIndentLevel(starry(6))
  print *, dentLevel

  do i = 1,nlines
    !starry(i) = Replace_Text(starry(i),'','')
    print *, starry(i)

  end do

  print *, compareThing("abc","abc")


  !Sorting algo

  temp = ""

  print *, ""
  print *, "Sorting..."
  print *, ""

  !Step one do an awful probablistic sort that maybe makes bubble sorting faster, but probably doesn't
  do while (lockLevel < nlines)
    iterCount = iterCount + 1
    do i = 1,nlines


      if (lockLevel >= i) then
        cycle !Continue
      end if


      startLock = lockLevel


      !Locks in elements from the start down if they are in the right spots
      canLock = 1
      do l = 1, nlines

        if(canLock == 0) then
          exit
        end if

        do m = l, nlines

          if(compareThing(starry(l)(6:),starry(m)(6:)) == 1) then !if m goes before l then l is not in the right spot
            canLock = 0
            exit
          end if

        end do

        if (canLock == 1) then
          lockLevel = l
          !print *, "Locking " // starry(l)
        else
          !print *, "Exiting as " // starry(l) // " can't be locked"
          exit
        end if
      end do

      if(startLock /= lockLevel) then
        exit
      end if

      temp = starry(i)
      checks = 0
      percentChange = 0
      do j = i+1, nlines
        checks = checks + 1
        if (checks == 1) then
          if (compareThing(temp(6:),starry(j)(6:)) == 0) then
            percentChange = -1.0
          else if (compareThing(temp(6:),starry(j)(6:)) == 1) then
            percentChange = 1.0
          end if
        else
          if (compareThing(temp(6:),starry(j)(6:)) == 0) then
            percentChange = (percentChange - 1.0)/2
          else if (compareThing(temp(6:),starry(j)(6:)) == 1) then
            percentChange = (percentChange + 1.0)/2
          end if
        end if
      end do

      if(nint(i+i*percentChange) > lockLevel) then !Set the change level to zero if we want to try changing to a locked index
        percentChange = 0
      end if

      if (nint(i+i*percentChange) <= 0 .or. nint(i+i*percentChange) >= nlines) then
        percentChange = 1
      end if



      if ((nint(i+i*percentChange) <= 0 .or. nint(i+i*percentChange) >= nlines)) then
        !It is too messed up for me to bother to fix it so uhhhh
        if(nint(i+i*percentChange) <= 0 .and. lockLevel+1 <= nlines) then
          starry(i) = starry(lockLevel+1)
          starry(lockLevel + 1) = temp
        else
          starry(i) = starry(nlines)
          starry(nlines) = temp
        end if

      else if (nint(i+i*percentChange) > lockLevel) then
        starry(i) = starry(nint(i+i*percentChange))
        starry(nint(i+i*percentChange)) = temp
      end if

      !Locks in elements from the start down if they are in the right spots
      canLock = 1
      do l = 1, nlines

        if(canLock == 0) then
          exit
        end if

        do m = l, nlines

          if(compareThing(starry(l)(6:),starry(m)(6:)) == 1) then !if m goes before l then l is not in the right spot
            canLock = 0
            exit
          end if

        end do

        if (canLock == 1) then
          lockLevel = l
          !print *, "Locking " // starry(l)
        else
          !print *, "Exiting as " // starry(l) // " can't be locked"
          exit
        end if
      end do

      if(startLock /= lockLevel) then
        exit
      end if

    end do

    !Do one round of selection sort when we're really stuck so the program will actually halt
    if(startLock == lockLevel) then
      print *, "Failed to improve sortedness"
      failureCount = failureCount + 1
      if(failureCount >= nlines) then
        print *, "Got stuck!"
        failureCount = 0
        do j = lockLevel + 1, nlines
          temp = starry(lockLevel + 1)
          tempIndex = lockLevel + 1
          if(compareThing(temp(6:),starry(j)(6:)) == 1) then
            temp = starry(j)
            tempIndex = j
          end if

        end do

        if(lockLevel + 1 <= nlines) then

          starry(tempIndex) = starry(lockLevel + 1)
          starry(lockLevel + 1) = temp
        end if
      end if
    end if

      print *, "Prob sort"
      do i = 1,nlines
        !starry(i) = Replace_Text(starry(i),'','')
        print *, starry(i)
      end do

  end do

  print *, "Sorted in this many iterations: "
  print *, iterCount

  print *, "Bubble sort!"

  !Bubble sort but I wrote it from memory after drinking a few beers, for confirming that things are sorted correctly
  temp =''
  !Done with checks so reuse it because spaghetti code funi
  checks = 0

  do while (checks == 0)
    do i = 1, nlines-1
      temp = starry(i)
      if(compareThing(temp(6:),starry(i+1)(6:)) == 0) then
        !Already sorted fine
      else
        starry(i) = starry(i+1)
        starry(i+1) = temp
        checks = checks + 1
      end if
    end do

    if (checks == 0) then
      checks = 1
    else
      checks = 0
    end if


  end do



  do i = 1,nlines
    !starry(i) = Replace_Text(starry(i),'','')
    print *, starry(i)
  end do

  !Need to print to file and make video

  open(1, file='test.py', status='old')

  do i = 1, nlines
    write(1,'(a)') adjustl(starry(i))
  end do

  exec = "exec("

  do i = 1, nlines-1
    exec = trim(exec)//achar(i+64)//"+"
  end do

  exec = trim(exec)//achar(nlines+64)//")"
  write(1,'(a)') adjustl(exec)
  close(1)


contains
  !Definitely original work that I did not borrow from https://fortranwiki.org/fortran/show/String_Functions at all
  FUNCTION Replace_Text (s,text,rep)  RESULT(outs)
    CHARACTER(*)        :: s,text,rep
    CHARACTER(LEN(s)+100) :: outs     ! provide outs with extra 100 char len
    INTEGER             :: i, nt, nr
    
    outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
    DO
       i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
       outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
    END DO
    END FUNCTION Replace_Text


    function getIndentLevel(str) result(out)
      character(*) :: str
      integer :: out !Indent level
      integer :: tf !True or false
      tf = 1
      out = 1


      do while (tf == 1)
    
        if(str(out:out) == ' ') then
          out = out + 1
        else
          tf = 0
        end if
    
      end do
    end function getIndentLevel

    function lowerChar(letter) result(out)
      character :: letter
      character :: out

      out = achar(iachar(letter)+32)
      
    end function lowerChar

    function compareThing(a,b) result(out)
      character(*) :: a,b
      character(100) :: c,d
      integer out
      c = adjustl(a)
      d = adjustl(b)

      !print *, a(1:1)

      if (iachar(c(1:1)) < iachar(d(1:1))) then
        out = 0
      else if (iachar(c(1:1)) > iachar(d(1:1))) then
        out = 1
      else if (getIndentLevel(a) < getIndentLevel(b)) then
        out = 0
      else if (getIndentLevel(a) > getIndentLevel(b)) then
        out = 1
      else
        out = 0
      end if
    end function compareThing

end program hello