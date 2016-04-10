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
	use rank_ndcg

	! can't use exact equality for floating point, but this should suffice
	real, parameter :: tol = 1.0E-5

contains
	subroutine assert_cum_gain(relevance, expected)
		! TODO use an actual unit test framework
		! sadly most seem to rely on Ruby
		real, intent(in), dimension(:) :: relevance
		real, intent(in) :: expected
		real :: actual

		actual = cum_gain(relevance)

		if (abs(actual - expected) > tol) then
			write (*,*) '*** Cumulative gain not equal to : ', expected, actual
		end if
	end subroutine assert_cum_gain

	subroutine assert_log2(x, expected)
		real, intent(in), dimension(:) :: x, expected
		real, dimension(size(x)) :: actual
		actual = log2(x)

		if (any(abs(actual - expected) > tol)) then
			write (*,*) '*** log2 not equal to : ', expected, 'got ', actual
		end if
	end subroutine
end module ndcg_test

program test_ndcg
	use ndcg_test
	implicit none

	call assert_cum_gain((/3.0, 2.0, 3.0, 0.0, 1.0, 2.0/), 11.0)
	! order should not matter for cumulative gain
	call assert_cum_gain((/3.0, 3.0, 2.0, 1.0, 0.0, 2.0/), 11.0)

	call assert_log2((/3.0, 4.0, 5.6/), (/1.5849625, 2.0, 2.48542683/))

end program test_ndcg
