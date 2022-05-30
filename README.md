# plt-fortran
A simple Fortran module for passing Fortran output to a Python script for later using Matplotlib to make figure

### Overview

```
This module (named pyplot) now contains one public subroutine:

subroutine plt_add_data(py_arr_name, fortran_arr, txt_name)
Create a Python script which imports numpy as np and matplotlib.pyplot as plt, if there is no such script
Save Fortran array fortran_arr to file txt_name.txt
Add this in Python script: py_arr_name = np.loadtxt('txt_name.txt')
Parameters
    py_arr_name: character
    fortran_arr: 1d or 2d real array (any kind in iso_fortran_env)
    txt_name: character, optional (default: py_arr_name)
No Returns
```

### Example

```fortran
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
    call plt_add_data('X', x)
    call plt_add_data('Y', y)
    call plt_add_data('Z', z, 'Gaussian')
end program test
```

Fortran array x, y, z will be saved to X.txt, Y.txt and Gaussian.txt.

A Python script (named plt.py) will be generated:

```python
import numpy as np
import matplotlib.pyplot as plt
X = np.loadtxt('X.txt')
Y = np.loadtxt('Y.txt')
Z = np.loadtxt('Gaussian.txt')
```

For example, if a "axis square contourf" is wanted, these can be added:

```python
plt.contourf(X, Y, Z)
plt.axis('square')
plt.show()
```

Then it will be able to run Python script for making figure.

### See also

 * [Numpy](https://numpy.org/)
 * [Matplotlib](https://matplotlib.org)
