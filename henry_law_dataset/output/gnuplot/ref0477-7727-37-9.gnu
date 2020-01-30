# load "ref0477-7727-37-9.gnu"
# chem = "nitrogen"

set terminal postscript eps color
set title "ref = 477; chem = nitrogen; casrn = 7727-37-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6504241E-05 * exp(  -1245.780    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.7570085E-05 point
set label "" at    293.1500    ,   0.6958361E-05 point
set label "" at    298.1500    ,   0.6461335E-05 point
set label "" at    303.1500    ,   0.6051699E-05 point
set label "" at    308.1500    ,   0.5718527E-05 point
set label "" at    298.1500    ,   0.6504241E-05 point ps 2 pt 6

plot [280:310] H(T)
