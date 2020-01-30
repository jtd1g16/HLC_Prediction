# load "ref0903-591-76-4.gnu"
# chem = "2-methylhexane"

set terminal postscript eps color
set title "ref = 903; chem = 2-methylhexane; casrn = 591-76-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1903042E-04 * exp(   3608.216    *(1/   298.    -1/T))

set label "" at    300.0500    ,   0.1927599E-04 point
set label "" at    308.1500    ,   0.3173394E-04 point
set label "" at    318.1500    ,   0.3855199E-04 point
set label "" at    298.1500    ,   0.1903042E-04 point ps 2 pt 6

plot [300:320] H(T)
