program test
    use iso_fortran_env, only: sp => real32, dp => real64
    use pyplot
    implicit none
    integer :: i, j
    real(sp) :: x(-500:500), y(-500:500)
    real(dp) :: z(-500:500,-500:500)
    x = [(real(i,sp), i=-500,500)]/500.0_sp*3.0_sp
    y = [(real(j,sp), j=-500,500)]/500.0_sp*3.0_sp
    do i = -500, 500
        do j = -500, 500
            z(i,j) = exp(-x(i)**2.0_sp/2.0_sp)*exp(-y(j)**2.0_sp/2.0_sp)
        end do
    end do
    call plt_init()
    call plt_add_data('X', x)
    call plt_add_data('Y', y)
    call plt_add_data('Z', z, 'Gaussian')
end program test
