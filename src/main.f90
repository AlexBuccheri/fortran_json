! Example JSON outputting using Jacobwilliams' json-fortran library 
!
! Ref: https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage
! A Buccheri 2020

program main
  use,intrinsic :: iso_fortran_env, only: dp => real64
  use json_module
  implicit none

  !> 'put' overloads
  !> Might make sense to make a class extension of json_core
  interface put
     procedure ::  put_real_2d_dp, &
                   put_complex_scalar_dp, put_complex_1d_dp, put_complex_2d_dp
  end interface put

  !> JSON object and trees
  !  TODO(Alex) Look at inheriting json_core in a derived class
  !  and writing some light wrapper methods  
  type(json_core) :: json
  type(json_value), pointer :: results_tree, structure, complex

  !> Atom index
  integer :: ia
  
  !> Atomic index character 
  character(len=5) :: atom_index
  
  ! ------------------------
  ! Dummy data to parse out
  ! ------------------------
  !> Number of atoms
  integer, parameter :: n_atoms = 2

  !> Species labels
  character(len=2), dimension(n_atoms) :: species = ['H ', 'He']

  !> Atomic positions (
  ! Writing like this saves transposing 
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
  
  
  !----------------
  !Main routine
  !----------------

  ! Initialize the class
  call json%initialize()

  ! Initialize JSON tree
  call json%create_object(results_tree,'')

  ! Add "structure" object to the tree
  call json%create_object(structure,'structure')
  call json%add(results_tree, structure)

  ! Add data
  call json%add(structure, 'n_atoms', n_atoms)

  ! Rank 1 arrays
  call json%add(structure, 'species', species)

  ! Can't parse rank 2 arrays and above 
  ! One might imagine you want a 2D array stored as [ [], [], ... []] in JSON
  ! Can either loop over rank 2+ arrays using specific wrappers for the main
  ! data structures, or vectorise/flatten and attach the shape (column-major)

  ! An example of option 1
  do ia = 1, n_atoms
     write(atom_index, '(I5)') ia
     call json%add(structure, 'position_'//trim(adjustl(atom_index)), positions(:, ia))
  enddo

  ! Option 2 
  call put(json, structure, positions, 'positions')

  ! Can't directly parse complex data of any type, hence use a wrapper
  call json%create_object(complex, 'complex')
  call json%add(results_tree, complex)
  call put(json, complex, complex_scalar, 'scalar')
  call put(json, complex, complex_1d, '1d_array')
  call put(json, complex, complex_2d, '2d_array')
  
  ! Write the file
  call json%print(results_tree,'../results.json')
  
  ! Clean up
  nullify(structure)
  !deallocate(structure)
  call json%destroy(results_tree)


contains

  !> Free subroutines to 'put' data types into JSON tree
  !
  ! TODO(Alex) Would make sense to switch data and label ordering in API
  !            so it's consistent with the JSON library 
  !
  ! TODO(Alex) Would make more sense if these subroutines
  !            where member procedures
  !
  ! TODO(Alex) Should assert that the subtree is in
  !            the json object 
  ! 
  !> json      json library object
  !> subtree   results tree in which to place data
  !> data      data to parse
  !> label     label of data in subtree
  !
  subroutine put_real_2d_dp(json, subtree, data, label)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    real(dp), target, contiguous, intent(in)    :: data(:,:)
    character(len=*),             intent(in)    :: label
    !> Allows in-place flattening of rank 2+ arrays
    real(dp),         pointer                   :: flattened_data(:)

    flattened_data(1:size(data)) => data
    call json%add(subtree, trim(label), flattened_data)
    call json%add(subtree, trim(label)//"_shape", shape(data))
    nullify(flattened_data)
    
  end subroutine put_real_2d_dp
  
  
  subroutine put_complex_scalar_dp(json, subtree, data, label)
    type(json_core),           intent(inout) :: json
    type(json_value), pointer, intent(inout) :: subtree
    complex(dp),               intent(in)    :: data
    character(len=*),          intent(in)    :: label

    call json%add(subtree, trim(label)//"_re", real(data))
    call json%add(subtree, trim(label)//"_im", aimag(data))

  end subroutine put_complex_scalar_dp

  
  subroutine put_complex_1d_dp(json, subtree, data, label)
    type(json_core),           intent(inout) :: json
    type(json_value), pointer, intent(inout) :: subtree
    complex(dp),               intent(in)    :: data(:)
    character(len=*),          intent(in)    :: label
    
    call json%add(subtree, trim(label)//"_re", real(data))
    call json%add(subtree, trim(label)//"_im", aimag(data))
    
  end subroutine put_complex_1d_dp

  
  subroutine put_complex_2d_dp(json, subtree, data, label)
    type(json_core),  intent(inout) :: json
    type(json_value), pointer, intent(inout) :: subtree
    complex(dp),      target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: label
    complex(dp),      pointer :: flattened_data(:)

    flattened_data(1:size(data)) => data
    call json%add(subtree, trim(label)//"_re", real(flattened_data))
    call json%add(subtree, trim(label)//"_im", aimag(flattened_data))
    call json%add(subtree, trim(label)//"_shape", shape(data))
    nullify(flattened_data)
    
  end subroutine put_complex_2d_dp

  ! Would write wrappers for trivial cases allowing the whole output
  ! to be abstracted 

  
end program main
