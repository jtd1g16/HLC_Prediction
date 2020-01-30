# load "ref0855-75-25-2.gnu"
# chem = "tribromomethane"

set terminal postscript eps color
set title "ref = 855; chem = tribromomethane; casrn = 75-25-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2255567E-01 * exp(  -5681.409    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.2928143E-01 point
set label "" at    308.1500    ,   0.1390500E-01 point
set label "" at    323.1500    ,   0.4812340E-02 point
set label "" at    298.1500    ,   0.2255567E-01 point ps 2 pt 6

plot [290:330] H(T)
