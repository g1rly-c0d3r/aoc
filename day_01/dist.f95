program dist
  implicit none
  
  integer, parameter :: LEN = 1000
  integer            :: a(LEN), b(LEN), i
  integer            :: input, sum

  open(newunit=input, file="input.txt")

  do i=1, LEN
   read(input, *) a(i), b(i)
   !write(*, "(i5, '   ', i5)") a(i), b(i)
  end do

  call sort(a(:), len)
  call sort(b(:), len)

  sum = 0

  do i=1, LEN 
    write(*, "(i5, '   ', i5)") a(i), b(i)
    !print "(i5)", a(i)
    sum = sum + a(i) - b(i)
  end do

  !print*, sum


  CONTAINS
  
  subroutine sort(arr, length)
    integer, intent(inout) :: arr(:)
    integer, intent(in)    :: length
    integer                :: i, j, key 

    do i=1, length-1 
      key = arr(i)
      j = i-1

      whi:do while((j.ge.0).and.(arr(j).gt.key))

      arr(j+1) = arr(j)
      j = j-1
      arr(j+1) = key
      end do whi
    end do 
  end subroutine

end program
