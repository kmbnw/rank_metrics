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
program test_ndcg
	use rank_ndcg

	REAL :: j(6)
	REAL :: cum_gain_j

	j = (/3, 2, 3, 0, 1, 2/)
	cum_gain_j = cum_gain(j)

	if (cum_gain_j .NE. 11.0) then
		write (*,*) '*** Cumulative gain not equal to 11.0: ', cum_gain_j
	end if
end program test_ndcg
