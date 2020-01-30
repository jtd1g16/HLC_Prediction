# load "ref0484-100-52-7.gnu"
# chem = "benzaldehyde"

set terminal postscript eps color
set title "ref = 484; chem = benzaldehyde; casrn = 100-52-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3694832     * exp(  -5059.498    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.6641994     point
set label "" at    298.1500    ,   0.3691093     point
set label "" at    308.1500    ,   0.2151493     point
set label "" at    318.1500    ,   0.1263262     point
set label "" at    298.1500    ,   0.3694832     point ps 2 pt 6

plot [280:320] H(T)
