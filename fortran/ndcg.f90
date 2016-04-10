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

    pure function log2(x)
        ! I am mildly surprised Fortran does not have this
        real, intent(in), dimension(:) :: x
        real, dimension(size(x)) :: log2
        log2 = log(x) / log(2.0)
    end function

!    Calculate discounted cumulative gain.
!
!    @param relevance: Graded and ordered relevances of the results.
!    @param alternate: True to use the alternate scoring (intended to
!    place more emphasis on relevant results).
    real function dcg(relevance, alternate)
        real, intent(in), dimension(:) :: relevance
        logical, intent(in) :: alternate
        real :: log2i
        ! placeholders
        integer :: i, p

        dcg = 0.0

        if (size(relevance) < 1) then
            return
        end if

        p = size(relevance)

        if (alternate) then
            ! from wikipedia: "An alternative formulation of
            ! DCG[5] places stronger emphasis on retrieving relevant documents"

            !log2i = log2((/(i, i=1, p + 1)/) + 1)
            !dcg = sum(((2 ** relevance) - 1) / log2i)
        else
            !log2i = log2((/(i, i=2, p + 1)/))
            !dcg = relevance(1) + sum(relevance(2:) / log2i)
        end if
    end function dcg

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
