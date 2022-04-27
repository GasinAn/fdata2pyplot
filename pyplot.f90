module pyplot

    use iso_fortran_env, only: dp => real64

    implicit none
    private
    integer, save :: txt_name_len = 1
    character(*), parameter :: py_file_name = 'plt.py'
    public :: plt_init, plt_add_data

    interface plt_add_data
        module procedure plt_add_1d_data
        module procedure plt_add_2d_data
    end interface plt_add_data

    contains

        subroutine plt_init()
            open(10, file=py_file_name)
            write(10,'(A18)') "import numpy as np"
            write(10,'(A31)') "import matplotlib.pyplot as plt"
            close(10)
        end subroutine plt_init

        subroutine plt_add_1d_data(py_array_name, fortran_array)
            character(*), intent(in) :: py_array_name
            real(dp), intent(in) :: fortran_array(:)

            character(txt_name_len) :: txt_name
            integer :: py_cmd_len
            character(:), allocatable :: py_cmd

            txt_name = '_'
            open(10, file=txt_name//'.txt')
            write(10,*) fortran_array
            close(10)

            open(10, file=py_file_name, access='append')
            py_cmd_len = len(py_array_name) + 15 + len(txt_name) + 6
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_array_name//" = np.loadtxt('"//txt_name//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            txt_name_len= txt_name_len + 1

        end subroutine plt_add_1d_data

        subroutine plt_add_2d_data(py_array_name, fortran_array)
            character(*), intent(in) :: py_array_name
            real(dp), intent(in) :: fortran_array(:,:)

            integer :: i
            character(txt_name_len) :: txt_name
            integer :: fortran_array_shape(2)
            integer :: py_cmd_len
            character(:), allocatable :: py_cmd

            txt_name = '_'
            open(10, file=txt_name//'.txt')
            fortran_array_shape = shape(fortran_array)
            do i = 1, fortran_array_shape(1)
                write(10,*) fortran_array(i,:)
            end do
            close(10)

            open(10, file=py_file_name, access='append')
            py_cmd_len = len(py_array_name) + 15 + len(txt_name) + 6
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_array_name//" = np.loadtxt('"//txt_name//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            txt_name_len= txt_name_len + 1

        end subroutine plt_add_2d_data

end module pyplot
