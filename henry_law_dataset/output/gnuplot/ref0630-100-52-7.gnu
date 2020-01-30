# load "ref0630-100-52-7.gnu"
# chem = "benzaldehyde"

set terminal postscript eps color
set title "ref = 630; chem = benzaldehyde; casrn = 100-52-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4188524     * exp(  -4555.915    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.9129040     point
set label "" at    298.1500    ,   0.4381939     point
set label "" at    303.1500    ,   0.3316062     point
set label "" at    308.1500    ,   0.2536393     point
set label "" at    318.1500    ,   0.1559339     point
set label "" at    298.1500    ,   0.4188524     point ps 2 pt 6

plot [280:320] H(T)
