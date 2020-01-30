# load "ref1996-108-03-2.gnu"
# chem = "1-nitropropane"

set terminal postscript eps color
set title "ref = 1996; chem = 1-nitropropane; casrn = 108-03-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1310837     * exp(  -4677.223    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1745802     point
set label "" at    303.1500    ,   0.9900163E-01 point
set label "" at    313.1500    ,   0.6088219E-01 point
set label "" at    323.1500    ,   0.3967162E-01 point
set label "" at    298.1500    ,   0.1310837     point ps 2 pt 6

plot [290:330] H(T)
