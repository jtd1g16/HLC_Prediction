# load "ref0484-75-07-0.gnu"
# chem = "ethanal"

set terminal postscript eps color
set title "ref = 484; chem = ethanal; casrn = 75-07-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1220145     * exp(  -6262.964    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.5467555     point
set label "" at    283.1500    ,   0.3878608     point
set label "" at    298.1500    ,   0.1125093     point
set label "" at    308.1500    ,   0.6474217E-01 point
set label "" at    298.1500    ,   0.1220145     point ps 2 pt 6

plot [270:310] H(T)
