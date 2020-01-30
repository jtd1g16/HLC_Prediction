# load "ref3118-19240-91-6.gnu"
# chem = "4-bromo-2,6-dichloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 4-bromo-2,6-dichloroanisole; casrn = 19240-91-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1231568E-01 * exp(  -4897.212    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.1455342E-01 point
set label "" at    318.1500    ,   0.4385576E-02 point
set label "" at    298.1500    ,   0.1231568E-01 point ps 2 pt 6

plot [290:320] H(T)
