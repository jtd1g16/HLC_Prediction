# load "ref0903-108-87-2.gnu"
# chem = "methylcyclohexane"

set terminal postscript eps color
set title "ref = 903; chem = methylcyclohexane; casrn = 108-87-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9577147E-04 * exp(  -9412.371    *(1/   298.    -1/T))

set label "" at    300.4500    ,   0.7895152E-04 point
set label "" at    308.9500    ,   0.2885753E-04 point
set label "" at    318.1500    ,   0.1380319E-04 point
set label "" at    298.1500    ,   0.9577147E-04 point ps 2 pt 6

plot [300:320] H(T)
