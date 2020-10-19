! Example JSON outputting using wrappers for Jacobwilliams' json-fortran library
!
! The idea of writing a generic set of functions/wrappers is that then one can
! use the fortran preprocessor to select a specific module with an overloaded
! set of functions, such that the API never has to change at this level. One
! would then has the ability to change the library or language used to parse the
! data out without changing this code.
! 
! A Buccheri 2020

program main
#ifdef JSON_OUTPUT  
  use json_parser, only: json_core, json_value, initialise_tree, add_subtree, &
       output_tree, destroy, put, put_positions
#endif
  use data 
  implicit none

#ifdef JSON_OUTPUT 
  !> JSON object and trees
  type(json_core) :: json
  type(json_value), pointer :: results_tree, structure, complex
#endif
  
  ! Initialise object, with top level tree
  call initialise_tree(json, results_tree)

  ! Add and populate structure subtree
  call add_subtree(json, results_tree, structure, 'structure')
  
  ! Scalars
  call put(json, structure, 'n_atoms', n_atoms)
  call put(json, structure, 'mass_he', mass_He)
  call put(json, structure, 'periodic', .true.)
  call put(json, results_tree, 'functional', 'PBE0')

  ! Rank 1 arrays
  call put(json, structure, 'species', species)

  ! Rank 2 arrays
  ! I don't know how to directly parse rank 2 arrays and above.
  ! Two current solutions are:
  !   1. Loop over rank 2+ arrays using bespoke wrappers for the main
  !      data structures. 
  !   2. Vectorise/flatten and also output the shape (column-major) 
  ! However, it should be possible to parse a 2D array as [ [], [], ... []] in JSON
  ! which is beneficial if post-processing with numpy 

  ! An example of option 1
  call put_positions(json, structure, positions)
  ! Am example of option 2 
  call put(json, structure, 'positions', positions)

  ! Rank 3 arrays work just as rank 2. Overloads require writing.
  ! At this point, one might consider HDF5 

  ! Complex data
  call add_subtree(json, results_tree, complex, 'complex')
  call put(json, complex, 'scalar',   complex_scalar)
  call put(json, complex, '1d_array', complex_1d)
  call put(json, complex, '2d_array', complex_2d)

  ! Write the file
  call output_tree(json, results_tree, '../results.json')
  
  ! Clean up
  nullify(structure, complex)
  call destroy(json, results_tree)

end program main
