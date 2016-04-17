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
    use qsort
    implicit none

    real, parameter :: tol = 1.0E-5
contains

    ! Elementwise log base 2 of x.
    ! @param x: Array to get log base 2 of.
    ! @return: Log base 2 of input array.
    pure function log2(x)
        ! I am mildly surprised Fortran does not have this
        real, intent(in), dimension(:) :: x
        real, dimension(size(x)) :: log2
        log2 = log(x) / log(2.0)
    end function

    ! Create a new subarray of A with size == len.
    ! Will be padded with initval if necessary.
    ! Only the first min(len, size(A)) elements of A will be copied.
    ! @param A: The array to copy and pad.
    ! @param initval: The padding value to use.
    ! @param len: The length of the returned array.
    ! @returns: A new padded/clamped array as described above.
    function pad(A, initval, len)
        integer, intent(in), dimension(:) :: A
        integer, intent(in) :: initval, len
        integer, dimension(max(len, size(A))) :: pad
        integer :: i

        pad = initval

        ! copy whatever is possible from A
        do i=1, size(A)
            pad(i) = A(i)
        end do
    end function pad

    ! Calculate cumulative gain.
    ! This ignores the position of a result, but may still be generally useful.

    ! @param relevance: Graded relevances of the results.
    pure integer function rank_cg(relevance)
        integer, intent(in), dimension(:) :: relevance

        rank_cg = 0.0D0
        if (size(relevance) < 1) then
            return
        end if

        rank_cg = sum(relevance)
    end function rank_cg

    ! Calculate discounted cumulative gain.
    !
    ! @param relevance: Graded and ordered relevances of the results.
    ! @param alternate: True to use the alternate scoring (intended to
    ! place more emphasis on relevant results).
    pure real function rank_dcg(relevance, alternate)
        integer, intent(in), dimension(:) :: relevance
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

            log2i = log2((/ (i, i=1, p) /) + 1.0)
            rank_dcg = sum(((2 ** (1.0 * relevance)) - 1.0) / log2i)
        else
            ! slightly different than wikipedia so I don't have to declare
            ! two arrays; this one only uses elements 2 onward
            log2i = log2((/ (i, i=1, p) /) * 1.0)
            rank_dcg = relevance(1) + sum(relevance(2:) / log2i(2:))
        end if
    end function rank_dcg

    ! Calculate ideal discounted cumulative gain (maximum possible DCG).
    !
    ! @param relevance: Graded and ordered relevances of the results.
    ! @param alternate: True to use the alternate scoring (intended to
    ! place more emphasis on relevant results).
    real function rank_idcg(relevance, alternate)
        integer, intent(in), dimension(:) :: relevance
        logical, intent(in) :: alternate
        integer, dimension(size(relevance)) :: rel

        rank_idcg = 0.0
        if (size(relevance) < 1) then
            return
        end if

        ! guard copy before sort, and negate for descending sort
        rel = -1 * relevance
        ! sort in descending order
        call quicksort(rel)
        ! negate again for proper DCG calculation
        rank_idcg = rank_dcg(-1 * rel, alternate)
    end function rank_idcg

    ! Calculate normalized discounted cumulative gain.
    !
    ! @param relevance: Graded and ordered relevance array of the results.
    ! @param nranks: Number of ranks to use when calculating NDCG.
    ! Will be used to rightpad with zeros if len(relevance) is less
    ! than nranks
    ! @param alternate: True to use the alternate scoring (intended to
    ! place more emphasis on relevant results).
    real function rank_ndcg(relevance, nranks, alternate)
          integer, intent(in), dimension(:) :: relevance
          logical, intent(in) :: alternate
          integer, intent(in) :: nranks
          integer, dimension(nranks) :: rel
          real :: ideal_dcg

          rank_ndcg = 0.0

          if (size(relevance) < 1 .OR. nranks < 1) then
              return
          end if

          rel = pad(relevance, 0, size(rel))

          ideal_dcg = rank_idcg(rel, alternate)
          if (abs(ideal_dcg - 0) < tol) then
              return
          end if

          rank_ndcg = rank_dcg(rel, alternate) / ideal_dcg

    end function rank_ndcg
end module ranking_ndcg
