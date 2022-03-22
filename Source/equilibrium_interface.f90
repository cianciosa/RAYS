!*******************************************************************************
!>  @file equilibrium_interface.f90
!>  @brief Pure virtual class to define the interface of an equilibrium.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the interface for an equilibrium.
!*******************************************************************************

MODULE equilibrium_interface

USE, INTRINSIC :: ISO_C_BINDING
USE asserts

IMPLICIT NONE

!-------------------------------------------------------------------------------
!>  Base class representing a the interface for an equilibrium
!-------------------------------------------------------------------------------
TYPE :: equilibrium_interface_class
CONTAINS
   PROCEDURE, PASS :: get_b_vec => equilibrium_interface_get_b_vec
   PROCEDURE, PASS :: get_b_mag_jac => equilibrium_interface_get_b_mag_jac
   PROCEDURE, PASS :: get_n => equilibrium_interface_get_n
   PROCEDURE, PASS :: get_n_grad => equilibrium_interface_get_n_grad
   PROCEDURE, PASS :: get_t => equilibrium_interface_get_t
   PROCEDURE, PASS :: get_t_grad => equilibrium_interface_get_t_grad
   FINAL           :: equilibrium_interface_destruct
END TYPE

CONTAINS

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref equilibrium_interface_class object.
!>
!>  Deallocates memory and uninitializes a @ref equilibrium_interface_class
!>  object.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!-------------------------------------------------------------------------------
SUBROUTINE equilibrium_interface_destruct(this)

IMPLICIT NONE

!  Declare Arguments
TYPE (equilibrium_interface_class), INTENT(inout) :: this

!  Start of executable code

END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the magnetic field vector.
!>
!>  This is virtual interface must be implimented in subclass.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!>  @param[in]    x    X position.
!>  @param[in]    y    Y position.
!>  @param[in]    z    Z position.
!>  @returns The magnetic field vector.
!-------------------------------------------------------------------------------
FUNCTION equilibrium_interface_get_b_vec(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3) :: equilibrium_interface_get_b_vec
CLASS (equilibrium_interface_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

CALL assert(.FALSE., 'get_b_vec method not implimented.')

END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the magnetic field jacobian.
!>
!>  This computes d B_i/d x_j for each dimension. This is virtual interface must
!>  be implimented in subclass.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!>  @param[in]    x    X position.
!>  @param[in]    y    Y position.
!>  @param[in]    z    Z position.
!>  @returns The magnetic field jacobian.
!-------------------------------------------------------------------------------
FUNCTION equilibrium_interface_get_b_mag_jac(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3,3) :: equilibrium_interface_get_b_mag_jac
CLASS (equilibrium_interface_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

CALL assert(.FALSE., 'get_b_mag_jac method not implimented.')

END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the density.
!>
!>  This is virtual interface must be implimented in subclass.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!>  @param[in]    x    X position.
!>  @param[in]    y    Y position.
!>  @param[in]    z    Z position.
!>  @returns The density.
!-------------------------------------------------------------------------------
FUNCTION equilibrium_interface_get_n(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE) :: equilibrium_interface_get_n
CLASS (equilibrium_interface_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

CALL assert(.FALSE., 'get_n method not implimented.')

END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the density gradient.
!>
!>  This is virtual interface must be implimented in subclass.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!>  @param[in]    x    X position.
!>  @param[in]    y    Y position.
!>  @param[in]    z    Z position.
!>  @returns The density gradient.
!-------------------------------------------------------------------------------
FUNCTION equilibrium_interface_get_n_grad(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3) :: equilibrium_interface_get_n_grad
CLASS (equilibrium_interface_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

CALL assert(.FALSE., 'get_n_grad method not implimented.')

END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the temperature.
!>
!>  This is virtual interface must be implimented in subclass.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!>  @param[in]    x    X position.
!>  @param[in]    y    Y position.
!>  @param[in]    z    Z position.
!>  @returns The temperature.
!-------------------------------------------------------------------------------
FUNCTION equilibrium_interface_get_t(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE) :: equilibrium_interface_get_t
CLASS (equilibrium_interface_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

CALL assert(.FALSE., 'get_t method not implimented.')

END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the temperature gradient.
!>
!>  This is virtual interface must be implimented in subclass.
!>
!>  @param[inout] this A @ref equilibrium_interface_class instance.
!>  @param[in]    x    X position.
!>  @param[in]    y    Y position.
!>  @param[in]    z    Z position.
!>  @returns The temperature gradient.
!-------------------------------------------------------------------------------
FUNCTION equilibrium_interface_get_t_grad(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3) :: equilibrium_interface_get_te
CLASS (equilibrium_interface_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

CALL assert(.FALSE., 'get_t_grad method not implimented.')

END FUNCTION

END MODULE
