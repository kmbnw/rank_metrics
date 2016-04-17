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

! https://en.wikipedia.org/wiki/Quicksort

module qsort
    implicit none
contains

	! Quick-sort the array in-place.
	! Stable sort is not guaranteed.
	! @param A: The array to sort.  If size(A) < 2,
	! this is effectively a no-op.
	subroutine quicksort(A)
		integer, intent(inout), dimension(:) :: A
		if (size(A) > 1) then
			call rquicksort(A, 1, size(A))
		end if
	end subroutine

	! In-place recursive variant of quicksort.
	! Stable sort is not guaranteed.
	! Not intended to be called outside the module
	! @param A: The array to sort.  If size(A) < 2,
	! this is effectively a no-op.
	! @param lo: Low endpoint of sub-array to sort.
	! @param hi: High endpoint of sub-array to sort.
	recursive subroutine rquicksort(A, lo, hi)
		integer, intent(inout), dimension(:) :: A
		integer, intent(in) :: lo, hi
		integer :: p

	    if (lo < hi .AND. size(A) > 1) then
	        p = partition(A, lo, hi)
	        call rquicksort(A, lo, p)
	        call rquicksort(A, p + 1, hi)
		end if
	end subroutine rquicksort

	integer function partition(A, lo, hi)
		integer, intent(inout), dimension(:) :: A
		integer, intent(in) :: lo, hi
		integer :: i, j, pivot, tmp

	    pivot = A(lo)
	    i = lo - 1
	    j = hi + 1

	    do
			! walk right, stop when A(i) is GTE pivot
	        do
	            i = i + 1

				if (A(i) >= pivot) then
					exit
				end if
			end do !while (A(i) < pivot)

			! walk left, stop when A(j) is LTE pivot
	        do
	            j = j - 1

				if (A(j) <= pivot) then
					exit
				end if
			end do !while (A(j) > pivot)

	        if (i >= j) then
				partition = j
	            return
			end if

			! swap
			tmp = A(i)
			A(i) = A(j)
			A(j) = tmp
		end do
	end function partition
end module qsort
