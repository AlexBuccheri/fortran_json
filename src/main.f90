! Example JSON outputting using Jacobwilliams' json-fortran library 
!
! Ref: https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage
! A Buccheri 2020

program main
  use json_parser, only: json_core, json_value, put
  use data 
  implicit none

  !> JSON object and trees
  type(json_core) :: json
  type(json_value), pointer :: results_tree, structure, complex

  !> Atom index
  integer :: ia
  
  !> Atomic index character 
  character(len=5) :: atom_index
  

  
  !----------------
  !Main routine
  !----------------

  ! Initialize the class
  call json%initialize()

  ! Initialize JSON tree
  call json%create_object(results_tree,'')

  ! Add "structure" object to the tree
  call json%create_object(structure, 'structure')
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
  call put(json, structure, 'positions', positions)

  ! Can't directly parse complex data of any type, hence use a wrapper
  call json%create_object(complex, 'complex')
  call json%add(results_tree, complex)
  call put(json, complex, 'scalar',   complex_scalar)
  call put(json, complex, '1d_array', complex_1d)
  call put(json, complex, '2d_array', complex_2d)
  
  ! Write the file
  call json%print(results_tree,'../results.json')
  
  ! Clean up
  nullify(structure, complex)
  
  !deallocate(structure)
  call json%destroy(results_tree)


contains

 

  
end program main
