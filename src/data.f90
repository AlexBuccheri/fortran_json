!> Dummy data to parse out
module data
  use,intrinsic :: iso_fortran_env, only: dp => real64
  implicit none

  !> Unit. dalton (Da or u) in kilograms
  real(dp), parameter :: dalton_to_kg = 1.66053906661e-27_dp 
  
  !> Number of atoms
  integer, parameter :: n_atoms = 2

  !> Mass of helium (kg)  
  real(dp), parameter :: mass_He = 4.002602_dp * dalton_to_kg
  
  !> Species labels
  character(len=2), dimension(n_atoms) :: species = ['H ', 'He']

  !> Atomic positions 
  !  Writing like this saves transposing 
  real(dp), dimension(3, n_atoms), target :: positions = reshape(&
       [0._dp, 0._dp, 0._dp,   &
        1._dp, 1._dp, 1._dp ], [3, n_atoms])

  !> Random complex data
  complex(dp) :: complex_scalar = cmplx(2._dp, 3._dp)
  complex(dp), dimension(2), target :: complex_1d = [cmplx(0._dp, 1._dp), cmplx(2._dp, 3._dp)]

  !> To explicitly define the data as it will be in memory, use transpose 
  complex(dp), dimension(2, n_atoms), target :: complex_2d = transpose(reshape(&
       [cmplx(0._dp, 1._dp), cmplx(4._dp, 5._dp), &
        cmplx(2._dp, 3._dp), cmplx(6._dp, 7._dp)  ], [2, n_atoms]))
  

end module data
