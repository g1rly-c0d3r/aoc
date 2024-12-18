program dist
  implicit none
!declare variables  
  character(len=20)   :: filename, filelen
  integer             :: LEN
  integer,allocatable :: a(:), b(:)
  integer             :: input, sum, multi, i, j

! get name and length of input
  call get_command_argument(1, filelen)
  call get_command_argument(2, filename)

  if ((filelen.eq.'').or.(filename.eq.'')) then
    print"('Usage: ""./day_01 <input_length> <input_file>""')"
    call exit(1)
  end if

! get length into an int and initialize a & b
  read(filelen, *) LEN
  allocate(a(LEN))
  allocate(b(LEN))
  
! open the input file to read a and b into memory
  open(newunit=input, file=filename)
  do i=1, LEN
   read(input, *) a(i), b(i)
  end do
  close(input)

! Part 1:
! Sort both a and b
  call sort(a, len)
  call sort(b, len)

  sum = 0

  do i=1, LEN 
  ! add the distance between them
    sum = sum + abs(a(i) - b(i))
  end do

  print"('Part 1 == ', i0)", sum

! Part 2:
sum = 0
La: do i = 1, LEN
  multi = 0 
Lb: do j = 1, LEN
  if (a(i).eq.b(j)) then
    multi = multi + 1
  end if
end do Lb
  sum = sum + a(i) * multi
end do La  
  
print"('Part 2 == ', i0)", sum


  CONTAINS
  
  subroutine sort(arr, length)
    integer, intent(inout) :: arr(:)
    integer, intent(in)    :: length
    integer                :: i, j, key 

    do i=2, length 
      key = arr(i)
      j = i-1
      whi:do while((j.gt.0).and.(arr(j).gt.key))
        arr(j+1) = arr(j)
        j = j-1
        arr(j+1) = key
      end do whi
    end do 
  end subroutine

end program
