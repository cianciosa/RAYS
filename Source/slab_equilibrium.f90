!*******************************************************************************
!>  @file slab_equilibrium.f90
!>  @brief Pure virtual class to define the interface of an equilibrium.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the interface for an equilibrium.
!*******************************************************************************

MODULE slab_equilibrium

USE equilibrium_interface

IMPLICIT NONE

!-------------------------------------------------------------------------------
!>  Base class representing a the interface for an equilibrium
!-------------------------------------------------------------------------------
TYPE, EXTENDS(equilibrium_interface_class) :: slab_equilibrium_class
CONTAINS
   PROCEDURE, PASS :: get_b_vec => slab_equilibrium_get_b_vec
   PROCEDURE, PASS :: get_b_mag_jac => slab_equilibrium_get_b_mag_jac
   PROCEDURE, PASS :: get_n => slab_equilibrium_get_n
   PROCEDURE, PASS :: get_n_grad => slab_equilibrium_get_n_grad
   PROCEDURE, PASS :: get_t => slab_equilibrium_get_t
   PROCEDURE, PASS :: get_t_grad => slab_equilibrium_get_t_grad
   FINAL           :: slab_equilibrium_destruct
END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the slab_equilibrium constructor.
!-------------------------------------------------------------------------------
INTERFACE slab_equilibrium_class
   MODULE PROCEDURE slab_equilibrium_construct
END INTERFACE

CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a default @ref custom_equilibrium_class object.
!>
!>  This is a hello world example of constructing a ways class.
!>
!>  @param[in] ne_scale Scale factor for density.
!>  @param[in] te_scale Scale factor for temperature.
!>  @returns A pointer to a constructed @ref custom_equilibrium_class object.
!-------------------------------------------------------------------------------
FUNCTION slab_equilibrium_construct()

IMPLICIT NONE

!  Declare Arguments
CLASS (slab_equilibrium_class), POINTER :: slab_equilibrium_construct

!  Start of executable code
ALLOCATE (slab_equilibrium_construct)

END FUNCTION

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
SUBROUTINE slab_equilibrium_destruct(this)

IMPLICIT NONE

!  Declare Arguments
TYPE (slab_equilibrium_class), INTENT(inout) :: this

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
FUNCTION slab_equilibrium_get_b_vec(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3) :: slab_equilibrium_get_b_vec
CLASS (slab_equilibrium_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                    :: x
REAL(C_DOUBLE), INTENT(in)                    :: y
REAL(C_DOUBLE), INTENT(in)                    :: z

!  Start of executable code

slab_equilibrium_get_b_vec = (/ 1.0, 0.0, 0.0 /)

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
FUNCTION slab_equilibrium_get_b_mag_jac(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3,3) :: slab_equilibrium_get_b_mag_jac
CLASS (slab_equilibrium_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                    :: x
REAL(C_DOUBLE), INTENT(in)                    :: y
REAL(C_DOUBLE), INTENT(in)                    :: z

!  Start of executable code

slab_equilibrium_get_b_mag_jac = 0.0

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
FUNCTION slab_equilibrium_get_n(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE) :: slab_equilibrium_get_n
CLASS (slab_equilibrium_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                         :: x
REAL(C_DOUBLE), INTENT(in)                         :: y
REAL(C_DOUBLE), INTENT(in)                         :: z

!  Start of executable code

slab_equilibrium_get_n = 1.0

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
FUNCTION slab_equilibrium_get_n_grad(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3) :: slab_equilibrium_get_n_grad
CLASS (slab_equilibrium_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                    :: x
REAL(C_DOUBLE), INTENT(in)                    :: y
REAL(C_DOUBLE), INTENT(in)                    :: z

!  Start of executable code

slab_equilibrium_get_n_grad = 0.0

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
FUNCTION slab_equilibrium_get_t(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE) :: slab_equilibrium_get_t
CLASS (slab_equilibrium_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                    :: x
REAL(C_DOUBLE), INTENT(in)                    :: y
REAL(C_DOUBLE), INTENT(in)                    :: z

!  Start of executable code

slab_equilibrium_get_t = 1.0

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
FUNCTION slab_equilibrium_get_t_grad(this, x, y, z)

IMPLICIT NONE

!  Declare Arguments
REAL(C_DOUBLE), DIMENSION(3) :: slab_equilibrium_get_t_grad
CLASS (slab_equilibrium_class), INTENT(inout) :: this
REAL(C_DOUBLE), INTENT(in)                    :: x
REAL(C_DOUBLE), INTENT(in)                    :: y
REAL(C_DOUBLE), INTENT(in)                    :: z

!  Start of executable code

slab_equilibrium_get_t_grad = 0.0

END FUNCTION

END MODULE
