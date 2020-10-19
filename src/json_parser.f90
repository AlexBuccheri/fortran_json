!> Module of wrappers for Williams' JSON Parser library,
!> abstracting the functionality demonstrated in the 2nd reference. 
!>
!> https://github.com/jacobwilliams/json-fortran
!> https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage
!> 
!> A Buccheri 2020


!  TODO(Alex)
!  Look at inheriting json_core in a derived class and writing some light wrapper methods  
!  Could then make 'put' a member procedure
!  In many routines, one should assert that the subtree is in the json object 
!  Add functionality to nest trees
!  Add functionality to combine (concatenate) trees.
!  Could potentially write this module with F2008 generic classes (as the JSON library has done)
!  to reduce the amount of boiler-plate code
!  Look at how to parse 2D arrays as  [ [], [], ... []] 


module json_parser
  use,intrinsic :: iso_fortran_env, only: dp => real64
  !> Williams' JSON Parser library
  use json_module, only: json_core, json_value
  implicit none
  private
  
  !> 'put' data into JSON tree of type 'json_value'
  interface put
     module procedure ::  put_int_scalar, put_int_1d, put_int_2d, &
                          put_real_scalar_dp, put_real_1d_dp, put_real_2d_dp, put_real_3d_dp, &
                          put_single_logical, put_logical_1d, &
                          put_single_character, put_character_1d, &
                          put_complex_scalar_dp, put_complex_1d_dp, put_complex_2d_dp
  end interface put

  public :: json_core, json_value
  public :: initialise_tree, add_subtree, output_tree, destroy, put, put_positions 
  
contains
    
  !> Initialise JSON object and top level tree
  ! 
  !> This routine is trivial (as are a few) but it allows: 
  !>  a) Easier abstraction - replacing this call with a routine
  !>     of the same name/signature but from a different module
  !>  b) Use of testing/assertions i.e. does the tree/node exist? 
  !>  c) In general, more unit-testable code 
  ! 
  !> json           json library object
  !> results_tree   results tree in which to place data
  subroutine initialise_tree(json, results_tree)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: results_tree
    call json%initialize()
    call json%create_object(results_tree, '')
  end subroutine initialise_tree


  !> Add a sub-tree (node) to a tree
  !> json           json library object
  !> tree           tree in which to nest subtree 
  !> subtree        subtree
  !> label          subtree label
  subroutine add_subtree(json, tree, subtree, label)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: tree
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    call json%create_object(subtree, trim(label))
    call json%add(tree, subtree) 
  end subroutine add_subtree


  !> Write JSON tree to file
  !> json           json library object
  !> results_tree   JSON tree containing results
  !> file_name      Name of file to write results_tree to 
  subroutine output_tree(json, results_tree, file_name)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(in)    :: results_tree
    character(len=*),             intent(in)    :: file_name
    call json%print(results_tree, trim(adjustl(file_name)))
  end subroutine output_tree


  !> Destroy/free JSON object
  !> json           json library object
  !> results_tree   JSON tree containing results
  subroutine destroy(json, results_tree)
   type(json_core),              intent(inout) :: json
   type(json_value), pointer,    intent(inout)    :: results_tree
    
    call json%destroy(results_tree)
    if (json%failed()) then
       !Depending on how error handling is done, return an error code
       !or force the program to stop here 
       write(*,*) 'Failed to dstroy core JSON object'
       stop
    endif
    
  end subroutine destroy

  
  !> Free subroutines to 'put' data types into JSON tree
  ! 
  !> json      json library object
  !> subtree   results tree in which to place data
  !> data      data to parse
  !> label     label of data in subtree
  subroutine put_int_scalar(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    integer,                      intent(in)    :: data
    call json%add(subtree, label, data)
  end subroutine put_int_scalar

  
  subroutine put_int_1d(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    integer,                      intent(in)    :: data(:)
    call json%add(subtree, label, data)
  end subroutine put_int_1d


  subroutine put_int_2d(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    integer, target, contiguous,  intent(in)    :: data(:,:)
    character(len=*),             intent(in)    :: label
    integer,          pointer                   :: flattened_data(:)

    !> Allows in-place flattening of rank 2+ arrays
    flattened_data(1:size(data)) => data
    call json%add(subtree, trim(label), flattened_data)
    call json%add(subtree, trim(label)//"_shape", shape(data))
    nullify(flattened_data)
    
  end subroutine put_int_2d


  subroutine put_real_scalar_dp(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    real(dp),                     intent(in)    :: data
    call json%add(subtree, label, data)    
  end subroutine put_real_scalar_dp


  subroutine put_real_1d_dp(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    real(dp),                     intent(in)    :: data(:)
    call json%add(subtree, label, data)   
  end subroutine put_real_1d_dp

  
  subroutine put_real_2d_dp(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    real(dp), target, contiguous, intent(in)    :: data(:,:)
    character(len=*),             intent(in)    :: label
    real(dp),         pointer                   :: flattened_data(:)

    flattened_data(1:size(data)) => data
    call json%add(subtree, trim(label), flattened_data)
    call json%add(subtree, trim(label)//"_shape", shape(data))
    nullify(flattened_data)
    
  end subroutine put_real_2d_dp


  subroutine put_real_3d_dp(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    real(dp), target, contiguous, intent(in)    :: data(:,:,:)
    character(len=*),             intent(in)    :: label
    real(dp),         pointer                   :: flattened_data(:)

    flattened_data(1:size(data)) => data
    call json%add(subtree, trim(label), flattened_data)
    call json%add(subtree, trim(label)//"_shape", shape(data))
    nullify(flattened_data)
    
  end subroutine put_real_3d_dp

  
  subroutine put_single_logical(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    logical,                      intent(in)    :: data
    call json%add(subtree, label, data)   
  end subroutine put_single_logical

  
  subroutine put_logical_1d(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    logical,                      intent(in)    :: data(:)
    call json%add(subtree, label, data)   
  end subroutine put_logical_1d

  
  subroutine put_single_character(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    character(len=*),             intent(in)    :: data
    call json%add(subtree, label, data)   
  end subroutine put_single_character

  
  subroutine put_character_1d(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    character(len=*),             intent(in)    :: data(:)
    call json%add(subtree, label, data)   
  end subroutine put_character_1d
  

  ! Can't directly parse complex data, hence split
  ! into real and imaginary parts 
  subroutine put_complex_scalar_dp(json, subtree, label, data)
    type(json_core),           intent(inout) :: json
    type(json_value), pointer, intent(inout) :: subtree
    complex(dp),               intent(in)    :: data
    character(len=*),          intent(in)    :: label

    call json%add(subtree, trim(label)//"_re", real(data))
    call json%add(subtree, trim(label)//"_im", aimag(data))

  end subroutine put_complex_scalar_dp

  
  subroutine put_complex_1d_dp(json, subtree, label, data)
    type(json_core),           intent(inout) :: json
    type(json_value), pointer, intent(inout) :: subtree
    complex(dp),               intent(in)    :: data(:)
    character(len=*),          intent(in)    :: label
    
    call json%add(subtree, trim(label)//"_re", real(data))
    call json%add(subtree, trim(label)//"_im", aimag(data))
    
  end subroutine put_complex_1d_dp

  
  subroutine put_complex_2d_dp(json, subtree, label, data)
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


  !> Data structure - specific parsing routine 
  subroutine put_positions(json, subtree, positions)
    type(json_core),           intent(inout) :: json
    type(json_value), pointer, intent(in)    :: subtree
    real(dp),                  intent(in)    :: positions(:, :)
    
    integer :: ia
    character(len=5) :: atom_index
    
    do ia = 1, size(positions, 2)
       write(atom_index, '(I5)') ia
       call json%add(subtree, 'position_'//trim(adjustl(atom_index)), positions(:, ia))
    enddo
    
  end subroutine put_positions

  
end module json_parser
