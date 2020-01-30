# load "ref0855-74-95-3.gnu"
# chem = "dibromomethane"

set terminal postscript eps color
set title "ref = 855; chem = dibromomethane; casrn = 74-95-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1090392E-01 * exp(  -3925.608    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1369849E-01 point
set label "" at    308.1500    ,   0.7058917E-02 point
set label "" at    323.1500    ,   0.3952994E-02 point
set label "" at    298.1500    ,   0.1090392E-01 point ps 2 pt 6

plot [290:330] H(T)
