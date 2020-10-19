!> Module of wrappers for Williams' JSON Parser library
!> A Buccheri 2020
!
!  TODO(Alex)
!  Look at inheriting json_core in a derived class and writing some light wrapper methods  
!  Consider if it would make more sense to make put subroutine a member procedure
!  Write wrappers for ALL cases allowing the whole output to be abstracted 

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

  public :: json_core, json_value, put 
  
contains

  !> Free subroutines to 'put' data types into JSON tree
  !
  ! TODO(Alex) Should assert that the subtree is in
  !            the json object 
  ! 
  !> json      json library object
  !> subtree   results tree in which to place data
  !> data      data to parse
  !> label     label of data in subtree
  !
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
