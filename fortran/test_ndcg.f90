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

module ndcg_test
	use ranking_ndcg
contains
	subroutine assert_almost_equal_r8(expected, actual, name)
		double precision, intent(in) :: expected, actual
		character(len=*), intent(in) :: name

		if (abs(actual - expected) > tol) then
			write (*, *) '*** ', name, ' not equal to : ', expected, actual
		end if

	end subroutine assert_almost_equal_r8

	subroutine assert_same_int(x, xcopy)
		integer, intent(in), dimension(:) :: x, xcopy

		if (any(x .NE. xcopy)) then
			write (*, *) '*** Arrays not equal', x, xcopy
		end if
	end subroutine assert_same_int

	subroutine assert_cg(relevance, expected)
		! TODO use an actual unit test framework
		! sadly most seem to rely on Ruby
		integer, intent(in), dimension(:) :: relevance
		integer, intent(in) :: expected
		integer :: actual

		actual = rank_cg(relevance)

		if (actual .NE. expected) then
			write (*, *) '*** Cumulative gain not equal to : ', expected, actual
		end if
	end subroutine assert_cg

	subroutine assert_log2(x, expected)
		real, intent(in), dimension(:) :: x, expected
		real, dimension(size(x)) :: actual
		actual = log2(x * 1.0D0)

		if (any(abs(actual - expected) > tol)) then
			write (*, *) '*** log2 not equal to : ', expected, 'got ', actual
		end if
	end subroutine

	subroutine assert_dcg(x, alternate, expected)
		integer, intent(in), dimension(:) :: x
		double precision, intent(in) :: expected
		integer, dimension(size(x)) :: xcopy
		logical :: alternate

		xcopy = x
		call assert_almost_equal_r8(rank_dcg(x, alternate), expected, 'DCG')
		call assert_same_int(xcopy, x)

	end subroutine assert_dcg

	subroutine assert_idcg(x, alternate, expected)
		integer, intent(in), dimension(:) :: x
		double precision, intent(in) :: expected
		integer, dimension(size(x)) :: xcopy
		logical :: alternate

		xcopy = x
		call assert_almost_equal_r8(rank_idcg(x, alternate), expected, 'IDCG')
		call assert_same_int(xcopy, x)

	end subroutine assert_idcg

	subroutine assert_ndcg(x, nranks, alternate, expected)
		integer, intent(in), dimension(:) :: x
		double precision, intent(in) :: expected
		integer, dimension(size(x)) :: xcopy
		double precision :: actual
		logical :: alternate

		xcopy = x
		actual = rank_ndcg(x, nranks, alternate)
		call assert_almost_equal_r8(actual, expected, 'NDCG')
		call assert_same_int(xcopy, x)

	end subroutine assert_ndcg

end module ndcg_test

program test_ndcg
	use ndcg_test
	implicit none
	integer, dimension(0) :: empty_array
	integer :: i

	!!! Test CG !!!

	! test cumulative gain for zeros / empty array
	call assert_cg((/0, 0, 0/), 0)
	call assert_cg((/0, 0, 0/), 0)
	call assert_cg(empty_array, 0)
	call assert_cg(empty_array, 0)

	call assert_cg((/3, 2, 3, 0, 1, 2/), 11)
	! order should not matter for cumulative gain
	call assert_cg((/3, 3, 2, 1, 0, 2/), 11)

	call assert_log2((/3.0, 4.0, 5.6/), (/1.5849625, 2.0, 2.48542683/))
	! make sure it works with a do loop constructor
	call assert_log2((/ (i, i=2, 5) /) * 1.0, (/1.0 , 1.58496, 2.0, 2.321928/))

	!!! Test DCG !!!
	! empty
	call assert_dcg(empty_array, .TRUE., 0.0D0)
	call assert_dcg(empty_array, .FALSE., 0.0D0)

	! zeros
	call assert_dcg((/0, 0, 0/), .TRUE., 0.0D0)
	call assert_dcg((/0, 0, 0/), .FALSE., 0.0D0)


	! from wikipedia
	! standard formulation
	call assert_dcg((/1, 1, 0, 1/), .FALSE., 2.5D0)
	call assert_dcg((/8, 9, 1, 0, 2/), .FALSE., 18.4922829D0)
	call assert_dcg((/3, 2, 3, 0, 1, 2/), .FALSE., 8.0971714D0)

	! check alternate DCG implementation
	call assert_dcg((/1, 1, 0, 1/), .TRUE., 2.0616063D0)
	call assert_dcg((/8, 9, 1, 0, 2 /), .TRUE., 579.0656625D0)

	!!!! test ideal DCG !!!
	! empty
	call assert_idcg(empty_array, .TRUE., 0.0D0)
	call assert_idcg(empty_array, .FALSE., 0.0D0)

	! zeros
	call assert_idcg((/0, 0, 0, 0/), .TRUE., 0.0D0)
    call assert_idcg((/0, 0, 0, 0/), .FALSE., 0.0D0)

	! from wikipedia
	! order is irrelevant
    call assert_idcg((/3, 2, 3, 0, 1, 2/), .FALSE., 8.6925361D0)
	call assert_idcg((/3, 2, 0, 3, 2, 1/), .FALSE., 8.6925361D0)

	!!! test NDCG !!!!

    ! zeros
	call assert_ndcg((/0, 0, 0, 0/), 2, .TRUE., 0.0D0)
	call assert_ndcg((/0, 0, 0, 0/), 2, .FALSE., 0.0D0)
	call assert_ndcg((/0, 0, 0, 0/), 4, .TRUE., 0.0D0)
	call assert_ndcg((/0, 0, 0, 0/), 4, .FALSE., 0.0D0)
	call assert_ndcg((/0, 0, 0, 0/), 6, .TRUE., 0.0D0)
	call assert_ndcg((/0, 0, 0, 0/), 6, .FALSE., 0.0D0)

	! empty array
	call assert_ndcg(empty_array, 2, .TRUE., 0.0D0)
	call assert_ndcg(empty_array, 2, .FALSE., 0.0D0)
	call assert_ndcg(empty_array, 4, .TRUE., 0.0D0)
	call assert_ndcg(empty_array, 4, .FALSE., 0.0D0)
	call assert_ndcg(empty_array, 6, .TRUE., 0.0D0)
	call assert_ndcg(empty_array, 6, .FALSE., 0.0D0)

    ! from wikipedia
    call assert_ndcg((/3, 2, 3, 0, 1, 2/), 6, .FALSE., 0.9315085D0)

	! is nranks respected?
    call assert_ndcg((/3, 2, 3, 0/), 4, .FALSE., 0.9491769D0)
    call assert_ndcg((/3, 2, 3/), 4, .FALSE., 0.9491769D0)

end program test_ndcg
