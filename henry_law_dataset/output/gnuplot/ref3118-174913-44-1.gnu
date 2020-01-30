# load "ref3118-174913-44-1.gnu"
# chem = "2,6-dibromo-4-chloroanisole"

set terminal postscript eps color
set title "ref = 3118; chem = 2,6-dibromo-4-chloroanisole; casrn = 174913-44-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1964649E-01 * exp(  -6710.542    *(1/   298.    -1/T))

set label "" at    295.1500    ,   0.2469671E-01 point
set label "" at    318.1500    ,   0.4773190E-02 point
set label "" at    298.1500    ,   0.1964649E-01 point ps 2 pt 6

plot [290:320] H(T)
