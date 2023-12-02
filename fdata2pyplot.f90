module fdata2pyplot

    use iso_fortran_env, only: real32, real64, real128

    implicit none
    private
    character(*), parameter :: py_file_name = 'plt.py'
    public :: fdata2pyplot_pass_data

    interface fdata2pyplot_pass_data
        module procedure fdata2pyplot_pass_1d_real32_data
        module procedure fdata2pyplot_pass_1d_real64_data
        module procedure fdata2pyplot_pass_1d_real128_data
        module procedure fdata2pyplot_pass_2d_real32_data
        module procedure fdata2pyplot_pass_2d_real64_data
        module procedure fdata2pyplot_pass_2d_real128_data
    end interface fdata2pyplot_pass_data

    contains

        subroutine init()
            logical, save :: initialized = .false.
            if (.not.initialized) then
                open(10, file=py_file_name, status='REPLACE', action='WRITE', position='APPEND')
                write(10,'(A)') "import numpy as np"
                write(10,'(A)') "import matplotlib.pyplot as plt"
                close(10)
                initialized = .true.
            end if
        end subroutine init

        subroutine fdata2pyplot_pass_1d_real32_data(py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real32), intent(in) :: fortran_arr(:)
            character(*), intent(in), optional :: txt_name

            integer :: txt_name_len_
            integer :: py_cmd_len
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_len_ = len(txt_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = txt_name
            else
                txt_name_len_ = len(py_arr_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = py_arr_name
            end if

            open(10, file=txt_name_//'.txt', status='REPLACE', action='WRITE', position='APPEND')
            write(10,*) fortran_arr
            close(10)

            open(10, file=py_file_name, status='OLD', action='WRITE', position='APPEND')
            py_cmd_len = len(py_arr_name) + len(" = np.loadtxt('") + len(txt_name_) + len(".txt')")
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_1d_real32_data

        subroutine fdata2pyplot_pass_1d_real64_data(py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real64), intent(in) :: fortran_arr(:)
            character(*), intent(in), optional :: txt_name

            integer :: txt_name_len_
            integer :: py_cmd_len
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_len_ = len(txt_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = txt_name
            else
                txt_name_len_ = len(py_arr_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = py_arr_name
            end if

            open(10, file=txt_name_//'.txt', status='REPLACE', action='WRITE', position='APPEND')
            write(10,*) fortran_arr
            close(10)

            open(10, file=py_file_name, status='OLD', action='WRITE', position='APPEND')
            py_cmd_len = len(py_arr_name) + len(" = np.loadtxt('") + len(txt_name_) + len(".txt')")
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_1d_real64_data

        subroutine fdata2pyplot_pass_1d_real128_data(py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real128), intent(in) :: fortran_arr(:)
            character(*), intent(in), optional :: txt_name

            integer :: txt_name_len_
            integer :: py_cmd_len
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_len_ = len(txt_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = txt_name
            else
                txt_name_len_ = len(py_arr_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = py_arr_name
            end if

            open(10, file=txt_name_//'.txt', status='REPLACE', action='WRITE', position='APPEND')
            write(10,*) fortran_arr
            close(10)

            open(10, file=py_file_name, status='OLD', action='WRITE', position='APPEND')
            py_cmd_len = len(py_arr_name) + len(" = np.loadtxt('") + len(txt_name_) + len(".txt')")
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_1d_real128_data

        subroutine fdata2pyplot_pass_2d_real32_data(py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real32), intent(in) :: fortran_arr(:,:)
            character(*), intent(in), optional :: txt_name

            integer :: i
            integer :: fortran_arr_shape(2)
            integer :: txt_name_len_
            integer :: py_cmd_len
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_len_ = len(txt_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = txt_name
            else
                txt_name_len_ = len(py_arr_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = py_arr_name
            end if

            open(10, file=txt_name_//'.txt', status='REPLACE', action='WRITE', position='APPEND')
            fortran_arr_shape = shape(fortran_arr)
            do i = 1, fortran_arr_shape(1)
                write(10,*) fortran_arr(i,:)
            end do
            close(10)

            open(10, file=py_file_name, status='OLD', action='WRITE', position='APPEND')
            py_cmd_len = len(py_arr_name) + len(" = np.loadtxt('") + len(txt_name_) + len(".txt')")
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_2d_real32_data

        subroutine fdata2pyplot_pass_2d_real64_data(py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real64), intent(in) :: fortran_arr(:,:)
            character(*), intent(in), optional :: txt_name

            integer :: i
            integer :: fortran_arr_shape(2)
            integer :: txt_name_len_
            integer :: py_cmd_len
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_len_ = len(txt_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = txt_name
            else
                txt_name_len_ = len(py_arr_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = py_arr_name
            end if

            open(10, file=txt_name_//'.txt', status='REPLACE', action='WRITE', position='APPEND')
            fortran_arr_shape = shape(fortran_arr)
            do i = 1, fortran_arr_shape(1)
                write(10,*) fortran_arr(i,:)
            end do
            close(10)

            open(10, file=py_file_name, status='OLD', action='WRITE', position='APPEND')
            py_cmd_len = len(py_arr_name) + len(" = np.loadtxt('") + len(txt_name_) + len(".txt')")
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_2d_real64_data

        subroutine fdata2pyplot_pass_2d_real128_data(py_arr_name, fortran_arr, txt_name)
            character(*), intent(in) :: py_arr_name
            real(real128), intent(in) :: fortran_arr(:,:)
            character(*), intent(in), optional :: txt_name

            integer :: i
            integer :: fortran_arr_shape(2)
            integer :: txt_name_len_
            integer :: py_cmd_len
            character(:), allocatable :: txt_name_
            character(:), allocatable :: py_cmd

            call init()

            if (present(txt_name)) then
                txt_name_len_ = len(txt_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = txt_name
            else
                txt_name_len_ = len(py_arr_name)
                allocate(character(txt_name_len_) :: txt_name_)
                txt_name_ = py_arr_name
            end if

            open(10, file=txt_name_//'.txt', status='REPLACE', action='WRITE', position='APPEND')
            fortran_arr_shape = shape(fortran_arr)
            do i = 1, fortran_arr_shape(1)
                write(10,*) fortran_arr(i,:)
            end do
            close(10)

            open(10, file=py_file_name, status='OLD', action='WRITE', position='APPEND')
            py_cmd_len = len(py_arr_name) + len(" = np.loadtxt('") + len(txt_name_) + len(".txt')")
            allocate(character(py_cmd_len) :: py_cmd)
            py_cmd = py_arr_name//" = np.loadtxt('"//txt_name_//".txt')"
            write(10,'(A)') py_cmd
            deallocate(py_cmd)
            close(10)

            deallocate(txt_name_)

        end subroutine fdata2pyplot_pass_2d_real128_data

end module fdata2pyplot
