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

module rank_ndcg
    implicit none
contains
    ! Calculate cumulative gain.
    ! This ignores the position of a result, but may still be generally useful.

    ! @param relevance: Graded relevances of the results.
    pure real function cum_gain(relevance)
        real, intent(in), dimension(:) :: relevance

        cum_gain = 0.0
        if (size(relevance) < 1) then
            return
        end if

        cum_gain = sum(relevance)
    end function cum_gain

    !    Calculate normalized discounted cumulative gain.
    !    @param relevance: Graded and ordered relevance array of the results.
    !    @param nranks: Number of ranks to use when calculating NDCG.
    !    Will be used to rightpad with zeros if len(relevance) is less
    !    than nranks
    pure real function ndcg(relevance, nranks)
          real, intent(in), dimension(:) :: relevance
          integer, intent(in) :: nranks

          ndcg = 0.0

          if (size(relevance) < 1 .OR. nranks < 1) then
              return
          end if

    end function ndcg
end module rank_ndcg
