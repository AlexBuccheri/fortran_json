! Example JSON outputting using Jacobwilliams' json-fortran library 
!
! Ref: https://github.com/jacobwilliams/json-fortran/wiki/Example-Usage
! A Buccheri 2020

program main
  use json_module
  implicit none

  type(json_core) :: json
  type(json_value), pointer :: p
  
  ! initialize the class
  call json%initialize()

  ! initialize the structure
  call json%create_object(p,'')

  ! Do nothing 
  
  ! clean up
  call json%destroy(p)
  
end program main
