module fdata2pyplot

    use iso_fortran_env, only: real32, real64, real128
    implicit none
    private

    character(*), parameter :: py_file_name = 'plt.py'

    public :: fdata2pyplot_pass_data, fdata2pyplot_add_others
    interface fdata2pyplot_pass_data
        module procedure fdata2pyplot_pass_1d_real32_data
        module procedure fdata2pyplot_pass_1d_real64_data
        module procedure fdata2pyplot_pass_1d_real128_data
        module procedure fdata2pyplot_pass_2d_real32_data
        module procedure fdata2pyplot_pass_2d_real64_data
        module procedure fdata2pyplot_pass_2d_real128_data
    end interface fdata2pyplot_pass_data

    contains

        function unopened_unit() result(unit)
            integer :: unit
            logical :: opened
            unit = 10
            do while (.true.)
                inquire(unit=unit, opened=opened)
                if (.not. opened) then
                    exit
                else
                    unit = unit + 1
                end if
            end do
        end function unopened_unit

        subroutine init()
            integer :: unit
            logical, save :: initialized = .false.
            if (.not. initialized) then
                unit = unopened_unit()
                open(unit=unit, file=py_file_name, status='REPLACE', &
                     action='WRITE', position='APPEND')
                write(unit=unit, fmt='(A)') "import numpy as np"
                write(unit=unit, fmt='(A)') "import matplotlib.pyplot as plt"
                close(unit=unit)
                initialized = .true.
            end if
        end subroutine init

        subroutine fdata2pyplot_pass_1d_real32_data( &
                       py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real32), intent(in) :: fortran_arr(:)
            character(*), intent(in), optional :: txt_name

            integer :: unit
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_ = txt_name
            else
                txt_name_ = py_arr_name
            end if

            unit = unopened_unit()
            open(unit=unit, file=txt_name_//'.txt', status='REPLACE', &
                 action='WRITE', position='APPEND')
            write(unit=unit, fmt=*) fortran_arr
            close(unit=unit)

            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(unit=unit, fmt='(A)') py_cmd
            deallocate(py_cmd)
            close(unit=unit)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_1d_real32_data

        subroutine fdata2pyplot_pass_1d_real64_data( &
                       py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real64), intent(in) :: fortran_arr(:)
            character(*), intent(in), optional :: txt_name

            integer :: unit
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_ = txt_name
            else
                txt_name_ = py_arr_name
            end if

            unit = unopened_unit()
            open(unit=unit, file=txt_name_//'.txt', status='REPLACE', &
                 action='WRITE', position='APPEND')
            write(unit=unit, fmt=*) fortran_arr
            close(unit=unit)

            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(unit=unit, fmt='(A)') py_cmd
            deallocate(py_cmd)
            close(unit=unit)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_1d_real64_data

        subroutine fdata2pyplot_pass_1d_real128_data( &
                       py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real128), intent(in) :: fortran_arr(:)
            character(*), intent(in), optional :: txt_name

            integer :: unit
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_ = txt_name
            else
                txt_name_ = py_arr_name
            end if

            unit = unopened_unit()
            open(unit=unit, file=txt_name_//'.txt', status='REPLACE', &
                 action='WRITE', position='APPEND')
            write(unit=unit, fmt=*) fortran_arr
            close(unit=unit)

            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(unit=unit, fmt='(A)') py_cmd
            deallocate(py_cmd)
            close(unit=unit)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_1d_real128_data

        subroutine fdata2pyplot_pass_2d_real32_data( &
                       py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real32), intent(in) :: fortran_arr(:,:)
            character(*), intent(in), optional :: txt_name

            integer :: unit
            integer :: i
            integer :: fortran_arr_shape(2)
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_ = txt_name
            else
                txt_name_ = py_arr_name
            end if

            unit = unopened_unit()
            open(unit=unit, file=txt_name_//'.txt', status='REPLACE', &
                 action='WRITE', position='APPEND')
            fortran_arr_shape = shape(fortran_arr)
            do i = 1, fortran_arr_shape(1)
                write(unit=unit, fmt=*) fortran_arr(i,:)
            end do
            close(unit=unit)

            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(unit=unit, fmt='(A)') py_cmd
            deallocate(py_cmd)
            close(unit=unit)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_2d_real32_data

        subroutine fdata2pyplot_pass_2d_real64_data( &
                       py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real64), intent(in) :: fortran_arr(:,:)
            character(*), intent(in), optional :: txt_name

            integer :: unit
            integer :: i
            integer :: fortran_arr_shape(2)
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_ = txt_name
            else
                txt_name_ = py_arr_name
            end if

            unit = unopened_unit()
            open(unit=unit, file=txt_name_//'.txt', status='REPLACE', &
                 action='WRITE', position='APPEND')
            fortran_arr_shape = shape(fortran_arr)
            do i = 1, fortran_arr_shape(1)
                write(unit=unit, fmt=*) fortran_arr(i,:)
            end do
            close(unit=unit)

            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(unit=unit, fmt='(A)') py_cmd
            deallocate(py_cmd)
            close(unit=unit)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_2d_real64_data

        subroutine fdata2pyplot_pass_2d_real128_data( &
                       py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real128), intent(in) :: fortran_arr(:,:)
            character(*), intent(in), optional :: txt_name

            integer :: unit
            integer :: i
            integer :: fortran_arr_shape(2)
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_ = txt_name
            else
                txt_name_ = py_arr_name
            end if

            unit = unopened_unit()
            open(unit=unit, file=txt_name_//'.txt', status='REPLACE', &
                 action='WRITE', position='APPEND')
            fortran_arr_shape = shape(fortran_arr)
            do i = 1, fortran_arr_shape(1)
                write(unit=unit, fmt=*) fortran_arr(i,:)
            end do
            close(unit=unit)

            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(unit=unit, fmt='(A)') py_cmd
            deallocate(py_cmd)
            close(unit=unit)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_2d_real128_data

        subroutine fdata2pyplot_add_others(others)
            character(*), intent(in) :: others
            integer :: unit
            call init()
            unit = unopened_unit()
            open(unit=unit, file=py_file_name, status='OLD', &
                 action='WRITE', position='APPEND')
            write(unit=unit, fmt='(A)') others
            close(unit=unit)
        end subroutine fdata2pyplot_add_others
end module fdata2pyplot
