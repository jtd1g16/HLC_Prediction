# load "ref0183-7790-92-3.gnu"
# chem = "hypochlorous acid"

set terminal postscript eps color
set title "ref = 183; chem = hypochlorous acid; casrn = 7790-92-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    4.746312     * exp(  -1633.084    *(1/   298.    -1/T))

set label "" at    215.0000    ,    39.47693     point
set label "" at    263.0000    ,    9.869233     point
set label "" at    298.1500    ,    4.746312     point ps 2 pt 6

plot [210:270] H(T)
