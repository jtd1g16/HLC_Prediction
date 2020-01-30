# load "ref0477-106-97-8.gnu"
# chem = "butane"

set terminal postscript eps color
set title "ref = 477; chem = butane; casrn = 106-97-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1239258E-04 * exp(  -3057.242    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1788200E-04 point
set label "" at    293.1500    ,   0.1467592E-04 point
set label "" at    298.1500    ,   0.1225633E-04 point
set label "" at    303.1500    ,   0.1041023E-04 point
set label "" at    308.1500    ,   0.8984697E-05 point
set label "" at    298.1500    ,   0.1239258E-04 point ps 2 pt 6

plot [280:310] H(T)
