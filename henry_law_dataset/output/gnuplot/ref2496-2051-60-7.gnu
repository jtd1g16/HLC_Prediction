# load "ref2496-2051-60-7.gnu"
# chem = "2-chlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 2-chlorobiphenyl; casrn = 2051-60-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3012418E-01 * exp(  -5271.350    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1111111     point
set label "" at    288.1500    ,   0.5263158E-01 point
set label "" at    298.1500    ,   0.3030303E-01 point
set label "" at    308.1500    ,   0.1724138E-01 point
set label "" at    298.1500    ,   0.3012418E-01 point ps 2 pt 6

plot [270:310] H(T)
