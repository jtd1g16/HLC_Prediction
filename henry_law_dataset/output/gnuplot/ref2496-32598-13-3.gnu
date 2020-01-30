# load "ref2496-32598-13-3.gnu"
# chem = "3,3',4,4'-tetrachlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 3,3',4,4'-tetrachlorobiphenyl; casrn = 32598-13-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2935583E-01 * exp(  -12890.06    *(1/   298.    -1/T))

set label "" at    278.1500    ,    1.351351     point
set label "" at    288.1500    ,   0.4166667E-01 point
set label "" at    298.1500    ,   0.3125000E-01 point
set label "" at    308.1500    ,   0.1041667E-01 point
set label "" at    298.1500    ,   0.2935583E-01 point ps 2 pt 6

plot [270:310] H(T)
