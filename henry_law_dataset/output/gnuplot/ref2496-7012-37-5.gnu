# load "ref2496-7012-37-5.gnu"
# chem = "2,4,4'-trichlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 2,4,4'-trichlorobiphenyl; casrn = 7012-37-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1847303E-01 * exp(  -2311.630    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2564103E-01 point
set label "" at    288.1500    ,   0.3125000E-01 point
set label "" at    298.1500    ,   0.2272727E-01 point
set label "" at    308.1500    ,   0.1136364E-01 point
set label "" at    298.1500    ,   0.1847303E-01 point ps 2 pt 6

plot [270:310] H(T)
