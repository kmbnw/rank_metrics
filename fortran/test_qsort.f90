! Copyright 2016 Krysta M Bouzek
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at

!    http://www.apache.org/licenses/LICENSE-2.0

!    Unless required by applicable law or agreed to in writing, software
!    distributed under the License is distributed on an "AS IS" BASIS,
!    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!    See the License for the specific language governing permissions and
!    limitations under the License.

module qsort_test
	use qsort
	implicit none
contains
	subroutine assert_same_int(x, xcopy)
		integer, intent(in), dimension(:) :: x, xcopy

		if (any(x .NE. xcopy)) then
			write (*, *) '*** Arrays not equal'
			write(*, *) x
			write(*, *) xcopy
		end if
	end subroutine assert_same_int
end module qsort_test

program test_qsort
	use qsort_test
	implicit none

	integer, dimension(5) :: x5
	integer, dimension(1) :: x1
	integer, dimension(2) :: x2

	x1 = (/4/)
	call quicksort(x1)
	call assert_same_int(x1, (/4/))

	x2 = (/2, 8/)
	call quicksort(x2)
	call assert_same_int(x2, (/2, 8/))

	x2 = (/8, 2/)
	call quicksort(x2)
	call assert_same_int(x2, (/2, 8/))

	x2 = (/-8, 4/)
	call quicksort(x2)
	call assert_same_int(x2, (/-8, 4/))

	x5 = (/3, 8, 2, 9, 0/)
	call quicksort(x5)
	call assert_same_int(x5, (/0, 2, 3, 8, 9/))

	x5 = (/0, 8, 3, 9, 0/)
	call quicksort(x5)
	call assert_same_int(x5, (/0, 0, 3, 8, 9/))

	x5 = (/0, 8, 3, 9, 0/)
	call quicksort(x5)
	call assert_same_int(x5, (/0, 0, 3, 8, 9/))

	! inverted sort
	x5 = -1 * (/0, 8, 3, 9, 0/)
	call quicksort(x5)
	call assert_same_int(-1 * x5, (/9, 8, 3, 0, 0/))

	! TODO test with largest/smallest ints
end program test_qsort
