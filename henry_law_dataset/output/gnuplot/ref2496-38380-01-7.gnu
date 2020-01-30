# load "ref2496-38380-01-7.gnu"
# chem = "2,2',4,4',5-pentachlorobiphenyl"

set terminal postscript eps color
set title "ref = 2496; chem = 2,2',4,4',5-pentachlorobiphenyl; casrn = 38380-01-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8829648E-02 * exp(  -8689.741    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1754386     point
set label "" at    288.1500    ,   0.8547009E-02 point
set label "" at    298.1500    ,   0.4329004E-02 point
set label "" at    308.1500    ,   0.8130081E-02 point
set label "" at    298.1500    ,   0.8829648E-02 point ps 2 pt 6

plot [270:310] H(T)
