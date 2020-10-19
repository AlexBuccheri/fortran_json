!> Module of wrappers for Williams' JSON Parser library
!>  abstracting functionality
!>
!> Ref: https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage
!> A Buccheri 2020

!  TODO(Alex)
!  Look at inheriting json_core in a derived class and writing some light wrapper methods  
!  Consider if it would make more sense to make put subroutine a member procedure
!
!  Functions to add
!  Nest trees
!  Combine (concatenate) trees - => JSON objects
!  Could do this with F2008 generic classes (as JSON library is done) to reduce boiler-plate code 


module json_parser
  use,intrinsic :: iso_fortran_env, only: dp => real64
  !> Williams' JSON Parser library
  use json_module, only: json_core, json_value
  implicit none
  private
  
  !> 'put' data into JSON tree of type 'json_value'
  interface put
     module procedure ::  put_real_2d_dp, &
                          put_complex_scalar_dp, put_complex_1d_dp, put_complex_2d_dp
  end interface put

  public :: json_core, json_value
  public :: initialise_tree, add_subtree, put 
  
contains

  !> Initialise JSON object and top level tree
  !> json           json library object
  !> results_tree   results tree in which to place data
  subroutine initialise_tree(json, results_tree)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: results_tree
    call json%initialize()
    call json%create_object(results_tree, '')
  end subroutine initialise_tree


  subroutine add_subtree(json, tree, subtree, label)

    
  end subroutine add_subtree

  
  subroutine output_tree(json, results_tree, file_name)

    call json%print(results_tree, file_name)
  end subroutine output_tree

  
  subroutine destroy(json, results_tree)

    call json%destroy(results_tree)
    if (json%failed()) then
      ! KILL THE CODE WITH AN ERROR. 
    endif
    
  end subroutine destroy

  
  
  
  !> Free subroutines to 'put' data types into JSON tree
  ! 
  !> json      json library object
  !> subtree   results tree in which to place data
  !> data      data to parse
  !> label     label of data in subtree
  !
  ! TODO(Alex) Should assert that the subtree is in the json object 

  subroutine put_int_scalar(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    integer,                      intent(in)    :: data
    call json%add(subtree, label, data)
  end subroutine put_int_scalar

  
  subroutine put_int_1d(json, subtree, label, data)

  end subroutine put_int_1d


  subroutine put_int_2d(json, subtree, label, data)

  end subroutine put_int_2d



  subroutine put_real_scalar_dp(json, subtree, label, data)
    type(json_core),              intent(inout) :: json
    type(json_value), pointer,    intent(inout) :: subtree
    character(len=*),             intent(in)    :: label
    real(dp),                     intent(in)    :: data
    call json%add(subtree, label, data)    
  end subroutine put_real_scalar_dp


  subroutine put_real_1d_dp(json, subtree, label, data)

  end subroutine put_real_1d_dp

  
  subroutine put_real_2d_dp(json, subtree, label, data)
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


  !logical_scalar

  !logical_1d

  
  !character_single

  !character_vector
  

  ! Can't directly parse complex data of any type, hence split
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



end module json_parser
