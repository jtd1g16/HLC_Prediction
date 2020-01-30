# load "ref1943-7553-56-2.gnu"
# chem = "iodine (molecular)"

set terminal postscript eps color
set title "ref = 1943; chem = iodine (molecular); casrn = 7553-56-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2817071E-01 * exp(  -3936.052    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3780000E-01 point
set label "" at    298.1500    ,   0.2590000E-01 point
set label "" at    323.1500    ,   0.1030000E-01 point
set label "" at    298.1500    ,   0.2817071E-01 point ps 2 pt 6

plot [290:330] H(T)
