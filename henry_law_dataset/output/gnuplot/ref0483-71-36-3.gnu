# load "ref0483-71-36-3.gnu"
# chem = "1-butanol"

set terminal postscript eps color
set title "ref = 483; chem = 1-butanol; casrn = 71-36-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.236027     * exp(  -7205.739    *(1/   298.    -1/T))

set label "" at    273.0000    ,    11.45451     point
set label "" at    298.0000    ,    1.251155     point
set label "" at    298.1500    ,    1.236027     point ps 2 pt 6

plot [270:300] H(T)
