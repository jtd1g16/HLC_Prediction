# load "ref2909-111-65-9.gnu"
# chem = "octane"

set terminal postscript eps color
set title "ref = 2909; chem = octane; casrn = 111-65-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2934567E-05 * exp(  -5391.646    *(1/   298.    -1/T))

set label "" at    287.9500    ,   0.5847588E-05 point
set label "" at    287.9500    ,   0.5680514E-05 point
set label "" at    293.2000    ,   0.3855935E-05 point
set label "" at    293.2000    ,   0.3896956E-05 point
set label "" at    298.2500    ,   0.2798626E-05 point
set label "" at    298.2500    ,   0.2826854E-05 point
set label "" at    303.2500    ,   0.2209125E-05 point
set label "" at    308.0700    ,   0.1702170E-05 point
set label "" at    298.1500    ,   0.2934567E-05 point ps 2 pt 6

plot [280:310] H(T)
