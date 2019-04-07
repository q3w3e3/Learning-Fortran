program main
implicit none

interface
    real function newtons(X0, uM, uTol)
    real :: X0
    real, optional :: uTol
    integer, optional :: uM
    end function newtons
end interface
    real :: a

    a = newtons(1.5)

end program main

real function func(x) result(val)
    implicit none

    real :: x
    val = tanh(x) - 3 * sin(x ** 2)
end function func

real function deriv(x) result(df)
    implicit none

    real :: x
    df = -6*x*cos(x**2)+1/(cosh(x)**2)
end function deriv

real function newtons(X0, uM, uTol) Result(NewtonsResult)
implicit none

    real, external :: func, deriv
    real :: X0
    real, optional :: uTol
    integer, optional :: uM
    real :: f,df,x,Xn
    real :: dx = 1
    integer :: n = 0
    integer :: M = 100
    real :: Tol = 0.000001

    if(present(uM)) M=uM
    if(present(uTol)) Tol=uTol

    x=X0

    write (*,*) ' Itteration          x           dx              f'
    write (*,*) 0, x, dx, f

    do while ((dx .ge. Tol .or. abs(f) .ge. Tol) .and. n .lt. M)
        n = n + 1.0
        df = deriv(x)
        Xn = x - (func(x)/deriv(x))
        dx = abs(x-Xn)
        x = Xn
        f = func(x)
        write (*,*) n, x, dx, f
    enddo
    NewtonsResult = n
end function newtons
