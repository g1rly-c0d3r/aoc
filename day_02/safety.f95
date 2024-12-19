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
  logical              :: exists


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

  




end program safety
