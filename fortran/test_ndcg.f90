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

	! can't use exact equality for floating point, but this should suffice
	real, parameter :: tol = 1.0E-5

contains
	subroutine assert_cg(relevance, expected)
		! TODO use an actual unit test framework
		! sadly most seem to rely on Ruby
		integer, intent(in), dimension(:) :: relevance
		real, intent(in) :: expected
		real :: actual

		actual = rank_cg(relevance * 1.0D0)

		if (abs(actual - expected) > tol) then
			write (*,*) '*** Cumulative gain not equal to : ', expected, actual
		end if
	end subroutine assert_cg

	subroutine assert_log2(x, expected)
		real, intent(in), dimension(:) :: x, expected
		real, dimension(size(x)) :: actual
		actual = log2(x * 1.0D0)

		if (any(abs(actual - expected) > tol)) then
			write (*,*) '*** log2 not equal to : ', expected, 'got ', actual
		end if
	end subroutine

	subroutine assert_dcg(x, alternate, expected)
		integer, intent(in), dimension(:) :: x
		real, intent(in) :: expected
		real :: actual
		logical :: alternate

		actual = rank_dcg(x * 1.0D0, alternate)
		if (abs(actual - expected) > tol) then
			write (*,*) '*** Discounted cumulative gain not equal to : ', expected, actual
		end if

	end subroutine assert_dcg
end module ndcg_test

program test_ndcg
	use ndcg_test
	implicit none
	integer, dimension(0) :: empty_array
	integer :: i

	! test cumulative gain for zeros / empty array
	call assert_cg((/0, 0, 0/), 0.0)
	call assert_cg((/0, 0, 0/), 0.0)
	call assert_cg(empty_array, 0.0)
	call assert_cg(empty_array, 0.0)

	call assert_cg((/3, 2, 3, 0, 1, 2/), 11.0)
	! order should not matter for cumulative gain
	call assert_cg((/3, 3, 2, 1, 0, 2/), 11.0)

	call assert_log2((/3.0, 4.0, 5.6/), (/1.5849625, 2.0, 2.48542683/))
	! make sure it works with a do loop constructor
	call assert_log2((/ (i, i=2, 5) /) * 1.0, (/1.0 , 1.58496, 2.0, 2.321928/))

	! test DCG for zeros / empty array
	call assert_dcg((/0, 0, 0/), .TRUE., 0.0)
	call assert_dcg((/0, 0, 0/), .FALSE., 0.0)
	call assert_dcg(empty_array, .TRUE., 0.0)
	call assert_dcg(empty_array, .FALSE., 0.0)

	! from wikipedia
	! standard formulation
	call assert_dcg((/1, 1, 0, 1/), .FALSE., 2.5)
	call assert_dcg((/8, 9, 1, 0, 2/), .FALSE., 18.4922829)
	call assert_dcg((/3, 2, 3, 0, 1, 2/), .FALSE., 8.0971714)

	! check alternate DCG implementation
	call assert_dcg((/1, 1, 0, 1/), .TRUE., 2.0616063)
	call assert_dcg((/8, 9, 1, 0, 2 /), .TRUE., 579.0656625)

end program test_ndcg
