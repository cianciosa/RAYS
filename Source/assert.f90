!*******************************************************************************
!>  @file assert.f90
!>  @brief Assertion utility for error checking.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines assertions. Assertions only run in debug mode.
!*******************************************************************************

MODULE asserts

USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

!*******************************************************************************
!  INTERFACES
!*******************************************************************************
INTERFACE
   SUBROUTINE abort() BIND(C)
   IMPLICIT NONE
   END SUBROUTINE
END INTERFACE

CONTAINS

!-------------------------------------------------------------------------------
!>  @brief Assert that the condition is true.
!>
!>  If the assertion fails, terminate execution but write out a debug message.
!>  When built in release mode this evaluates to a no opt.
!>
!>  @param[in] assertion The value to check.
!>  @param[in] message   The message to write.
!-------------------------------------------------------------------------------
SUBROUTINE assert(assertion, message)

IMPLICIT NONE

!  Declare Arguments
LOGICAL, INTENT(in)           :: assertion
CHARACTER (len=*), INTENT(in) :: message

#if !defined(NDEBUG)

!  Start of executable code
IF (.not.assertion) THEN
   WRITE (*,*) message
   CALL abort
END IF

#endif

END SUBROUTINE

END MODULE
