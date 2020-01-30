# load "ref2825-67-64-1.gnu"
# chem = "propanone"

set terminal postscript eps color
set title "ref = 2825; chem = propanone; casrn = 67-64-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2400272     * exp(  -4280.154    *(1/   298.    -1/T))

set label "" at    323.0000    ,   0.7922559E-01 point
set label "" at    333.0000    ,   0.5390720E-01 point
set label "" at    343.0000    ,   0.3652586E-01 point
set label "" at    353.0000    ,   0.2581174E-01 point
set label "" at    298.1500    ,   0.2400272     point ps 2 pt 6

plot [320:360] H(T)
