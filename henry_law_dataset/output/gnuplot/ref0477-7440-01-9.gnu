# load "ref0477-7440-01-9.gnu"
# chem = "neon"

set terminal postscript eps color
set title "ref = 477; chem = neon; casrn = 7440-01-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4476333E-05 * exp(  -469.8224    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.4752877E-05 point
set label "" at    293.1500    ,   0.4585199E-05 point
set label "" at    298.1500    ,   0.4452477E-05 point
set label "" at    303.1500    ,   0.4350887E-05 point
set label "" at    308.1500    ,   0.4276060E-05 point
set label "" at    298.1500    ,   0.4476333E-05 point ps 2 pt 6

plot [280:310] H(T)
