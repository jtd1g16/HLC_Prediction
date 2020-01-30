# load "ref0477-74-82-8.gnu"
# chem = "methane"

set terminal postscript eps color
set title "ref = 477; chem = methane; casrn = 74-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1404967E-04 * exp(  -1595.515    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1705181E-04 point
set label "" at    293.1500    ,   0.1532587E-04 point
set label "" at    298.1500    ,   0.1393857E-04 point
set label "" at    303.1500    ,   0.1281343E-04 point
set label "" at    308.1500    ,   0.1190677E-04 point
set label "" at    298.1500    ,   0.1404967E-04 point ps 2 pt 6

plot [280:310] H(T)
