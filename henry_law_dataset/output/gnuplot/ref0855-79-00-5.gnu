# load "ref0855-79-00-5.gnu"
# chem = "1,1,2-trichloroethane"

set terminal postscript eps color
set title "ref = 855; chem = 1,1,2-trichloroethane; casrn = 79-00-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1164984E-01 * exp(  -3947.705    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1483697E-01 point
set label "" at    308.1500    ,   0.7330054E-02 point
set label "" at    323.1500    ,   0.4257070E-02 point
set label "" at    298.1500    ,   0.1164984E-01 point ps 2 pt 6

plot [290:330] H(T)
