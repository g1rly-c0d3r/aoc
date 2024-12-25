program safety
! Advent of Code 2024: day 2
! Author: g1rly-c0d3r
  implicit none
! variable declaration
  integer, parameter   :: maxlev = 10
  character(len=50)    :: inputfile
  character(len=50)    :: levels
  integer, allocatable :: reports(:,:)
  integer              :: input, i, j, numsafe, len
  logical              :: exists, safe


! setup and error handling for the input file
! #####################################################################################
! get the name of the input
  CALL get_command_argument(1, inputfile)
  if(inputfile.eq.'') then
    print"('Usage: ""./safety <input_file> ""')"
    call exit(1)
  end if

  inquire(file=inputfile, exist=exists)
  if(.not.exists) then
    print"('Can not open file ""', a, '"": No such file')", TRIM(inputfile)
    call exit(2)
  end if
  open(newunit=input, file=inputfile, status="old", action="read")

  len = 0

  do while(.true.)
    read(input, *, END=1)
    len = len + 1
  end do

1 if(len.eq.0) then
    print"('File has no reports. Exiting...')"
    call exit(1)
  end if
! go back to the start of the file
  REWIND input

! Allocating and reading the reports from input
! #############################################
  allocate(reports(maxlev, len))
! init reports to check for the last level
! (some reports may have fewer than 10 levels)
  reports = -1

  do i=1, len
    read(input, "(a)") levels
! read the levels from this report into memory
! if there are no more levels, go to next report
    read(levels, *, END=2) reports(:,i)
2   continue
  end do


! Part 1: find the number of safe reports
! #######################################
  numsafe = 0
  call part01()

  numsafe = 0
  call part02better()


  contains

subroutine part01() 
  
  rep:do i=1, LEN
      if(monotonic(reports(:,i), 0).ne.0) then
        !print"('Report ', i4, ' Not Monotonic!')",i
        cycle
      end if
      safe = .false.

  lev:do j=1, maxlev
        if(reports(j+1,i).eq.-1) then
          exit lev
        end if


        if((ABS(reports(j,i)-reports(j+1,i)).le.3)&
          .and.(ABS(reports(j,i)-reports(j+1,i)).ge.1)) then
            safe = .true.
        else
          !print"('Report ', i4, ' Not Safe!')", i
          safe = .false.
          exit lev
        end if

      end do lev
      if(safe.eqv..true.) then
        numsafe = numsafe + 1
        !print"('Report ', i4, ' Is Safe!')", i
      end if
    end do rep

    print"('Part 1: safe reports == ', i0)", numsafe

end subroutine

subroutine part02()
  integer :: skip
 
rep:do i=1, LEN
    skip = monotonic(reports(:,i), 0)
    ! if there is only 1 non-monotonic level, then keep goin, otherwise ONTO THE NEXT REPORT
    if((skip.ne.0).and.(monotonic(reports(:,i), skip).ne.0)) then
      print"('Report ', i0, ' is not monotonic with PD!')", i
      cycle rep
    end if
    safe = .false.

  lev:do j=1, maxlev
    if(reports(j+1,i).eq.-1) then 
      exit lev
    end if

! the final branch is if the PD is not needed, therefore is the same as part one
   if((ABS(reports(j,i)-reports(j+1,i)).le.3)&
      .and.(ABS(reports(j,i)-reports(j+1,i)).ge.1)) then
        safe = .true.
    else 
      safe = .false.
      exit lev
    end if
    
    end do lev
    if(safe.eqv..true.) then
      numsafe = numsafe + 1
      print"('Report ', i4, ' Is Safe!')", i
    end if
  end do rep

  print"('Part 2: safe reports == ', i0)", numsafe

end subroutine

subroutine part02better()
  integer :: skip, difference
  logical :: increasing

rep:do i=1, LEN
  if(reports(2,i)-reports(1,i).ge.0) then
    increasing = .true.
  else 
    increasing = .false.
  end if
  lev:do j=1, maxlev
    if(reports(j+1,i).eq.-1) then
      exit lev
    end if
    
    safe = .true.
    difference = reports(j+1,i) - reports(j,i)

    if(.not.(increasing.eqv.difference.gt.0)) then
      !print"('Report ', i3, ' Not Monotonic!')",i
      safe = .false.
      exit lev
    else if(ABS(difference).gt.3.or.ABS(difference).lt.1) then
      safe = .false.
      exit lev
    end if
  end do lev

  if(safe.eqv..true.) then 
    numsafe = numsafe + 1
    !print"('Report ', i3, ' is safe.')", i
  end if
end do rep

print"('Part 2: safe reports == ', i0)", numsafe

end subroutine

function monotonic(report, skip) result(notmono)
  integer, intent(in) :: report(:)
  integer, intent(in) :: skip
  logical             :: increasing
  integer             :: notmono, i

  ! This whole select is so that if the skip level is 1 or 2, &
  ! we skip that level when deciding if the report is monotonic
  SELECT CASE (skip)
  CASE (1)
    if(report(2).lt.report(3)) then
      increasing = .true.
    else
      increasing = .false.
    end if
  CASE (2)
    if(report(1).lt.report(3)) then
      increasing = .true.
    else
      increasing = .false.
    end if
  CASE DEFAULT 
    if(report(1).lt.report(2)) then
      increasing = .true.
    else
      increasing = .false.
    end if
  END SELECT


  select case (increasing)
    case (.true.)
      do i=1, maxlev
! to check if there is a next level
! (levels are always greater than 0)
        if(report(i+1).eq.-1) then
! if the next level == -1, then the report is over
          exit ! select
        else if (i.eq.skip) then
! for the PD, we skip this one and check the rest
          cycle ! 1<=i<=maxlev loop
        end if

        if(report(i).gt.report(i+1)) then 
          notmono=i
          return
        end if
      end do

    case (.false.)
      do i=1, maxlev
! to check if there is a next level
! (levels are always greater than 0)
        if(report(i+1).eq.-1) then
! if the next level == -1, then the report is over
          exit ! select
        else if (i.eq.skip) then
! for the PD, we skip this one and check the rest
          cycle ! 1<=i<=maxlev loop
        end if

        if(report(i).lt.report(i+1)) then 
          notmono=i
          return
        end if
      end do
  end select
  notmono = 0
end function


end program safety
