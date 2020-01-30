# load "ref0477-7783-07-5.gnu"
# chem = "selenium hydride"

set terminal postscript eps color
set title "ref = 477; chem = selenium hydride; casrn = 7783-07-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8118739E-03 * exp(  -1654.201    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.9831279E-03 point
set label "" at    298.1500    ,   0.8138115E-03 point
set label "" at    308.1500    ,   0.6772659E-03 point
set label "" at    298.1500    ,   0.8118739E-03 point ps 2 pt 6

plot [280:310] H(T)
