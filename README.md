# fdata2pyplot

A simple Fortran module for passing Fortran output to a Python script for later using Matplotlib to make figure

## Philosophy

Fortran is weak in making figure, while Matplotlib is strong in making figure. It is not necessary to reinvent a Fortran wheel of making figure, instead, a simple way to remedy the weakness of Fortran is building a simple bridge from Fortran to Matplotlib.

## Overview

The module `fdata2pyplot` is defined in `src/fdata2pyplot.f90`. Three public subroutines `fdata2pyplot_pass_data`, `fdata2pyplot_add_others` and `fdata2pyplot_plt` are defined in it.

## Example

```fortran
program test
    use iso_fortran_env, only: sp => real32, dp => real64
    use fdata2pyplot

    implicit none
    integer :: i, j
    real(sp) :: x(-500:500), y(-500:500)
    real(dp) :: z(-500:500, -500:500)

    x = [(real(i, sp), i=-500, 500)] / 500.0_sp * 3.0_sp
    y = [(real(j, sp), j=-500, 500)] / 500.0_sp * 3.0_sp
    do i = -500, 500
        do j = -500, 500
            z(i, j) = exp(-x(i)**2.0_sp/2.0_sp) * exp(-y(j)**2.0_sp/2.0_sp)
        end do
    end do

    call fdata2pyplot_pass_data(x, "X")
    call fdata2pyplot_pass_data(y, "Y")
    call fdata2pyplot_pass_data(z, "Z", "Gaussian")
    call fdata2pyplot_add_others("plt.contourf(X, Y, Z)")
    call fdata2pyplot_add_others("plt.axis('square')")
    call fdata2pyplot_add_others("plt.show()")
    call fdata2pyplot_plt()
end program test
```

After running this program:

1. Fortran arrays `x`, `y`, `z` will be saved to `X.txt`, `Y.txt` and `Gaussian.txt` respectively;

2. A Python script named `plt.py` will be generated, and it contains:
```python
import numpy as np
import matplotlib.pyplot as plt
X = np.loadtxt('X.txt')
Y = np.loadtxt('Y.txt')
Z = np.loadtxt('Gaussian.txt')
plt.contourf(X, Y, Z)
plt.axis('square')
plt.show()
```

3. The Python script `plt.py` will be run for making figure.

## Tips

1. `fdata2pyplot_pass_data` has three dummy arguments: `fortran_arr`, `py_arr_name`, and optional `txt_name`. For convenience, You can only associate `fortran_arr` and `py_arr_name` with effective arguments, and make `py_arr_name` be the name of `fortran_arr`. For example, you can run:
```fortran
    call fdata2pyplot_pass_data(x, "x")
    call fdata2pyplot_pass_data(y, "y")
    call fdata2pyplot_pass_data(z, "z")
```

2. In the above example, I called `fdata2pyplot_add_others` several times, which seems to be cumbersome. However, since different operating systems use different line-break characters, I have to do it like this to make the example be safely cross-platform. If you are sure that you will not meet with this trouble, you can call `fdata2pyplot_add_others` one time for adding all "others". For example, if you will only run the program on a Unix-like platform, you can run:
```fortran
    call fdata2pyplot_add_others("plt.contourf(X, Y, Z)"//new_line('A')// &
                                 "plt.axis('square')"//new_line('A')// &
                                 "plt.show()")
```

3. Usually we don't know how to make the clearest figure. You need not call `fdata2pyplot_add_others` and `fdata2pyplot_plt`. You can only call `fdata2pyplot_pass_data`, and then modify and run `plt.py` by hand to try different methods of making figure.

## See also

 * [Numpy](https://numpy.org/)
 * [Matplotlib](https://matplotlib.org)
