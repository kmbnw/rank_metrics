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

! https://en.wikipedia.org/wiki/Discounted_cumulative_gain
module ranking_ndcg
    implicit none
contains
    ! Calculate cumulative gain.
    ! This ignores the position of a result, but may still be generally useful.

    ! @param relevance: Graded relevances of the results.
    pure double precision function rank_cg(relevance)
        double precision, intent(in), dimension(:) :: relevance

        rank_cg = 0.0D0
        if (size(relevance) < 1) then
            return
        end if

        rank_cg = sum(relevance)
    end function rank_cg

    pure function log2(x)
        ! I am mildly surprised Fortran does not have this
        double precision, intent(in), dimension(:) :: x
        double precision, dimension(size(x)) :: log2
        log2 = log(x) / log(2.0D0)
    end function

!    Calculate discounted cumulative gain.
!
!    @param relevance: Graded and ordered relevances of the results.
!    @param alternate: True to use the alternate scoring (intended to
!    place more emphasis on relevant results).
    pure double precision function rank_dcg(relevance, alternate)
        double precision, intent(in), dimension(:) :: relevance
        logical, intent(in) :: alternate
        double precision, dimension(size(relevance)) :: log2i
        ! placeholders
        integer :: i, p

        rank_dcg = 0.0

        if (size(relevance) < 1) then
            return
        end if

        p = size(relevance)

        if (alternate) then
            ! from wikipedia: "An alternative formulation of
            ! DCG[5] places stronger emphasis on retrieving relevant documents"

            log2i = log2((/ (i, i=1, p) /) + 1.0D0)
            rank_dcg = sum(((2 ** relevance) - 1.0) / log2i)
        else
            ! slightly different than wikipedia so I don't have to declare
            ! two arrays; this one only uses elements 2 onward
            log2i = log2((/ (i, i=1, p) /) * 1.0D0)
            rank_dcg = relevance(1) + sum(relevance(2:) / log2i(2:))
        end if
    end function rank_dcg

    !    Calculate normalized discounted cumulative gain.
    !    @param relevance: Graded and ordered relevance array of the results.
    !    @param nranks: Number of ranks to use when calculating NDCG.
    !    Will be used to rightpad with zeros if len(relevance) is less
    !    than nranks
    pure real function rank_ndcg(relevance, nranks)
          real, intent(in), dimension(:) :: relevance
          integer, intent(in) :: nranks

          rank_ndcg = 0.0

          if (size(relevance) < 1 .OR. nranks < 1) then
              return
          end if

    end function rank_ndcg
end module ranking_ndcg
