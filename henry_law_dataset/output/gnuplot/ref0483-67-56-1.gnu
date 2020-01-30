# load "ref0483-67-56-1.gnu"
# chem = "methanol"

set terminal postscript eps color
set title "ref = 483; chem = methanol; casrn = 67-56-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    2.200344     * exp(  -5212.349    *(1/   298.    -1/T))

set label "" at    273.0000    ,    11.01395     point
set label "" at    298.0000    ,    2.219792     point
set label "" at    298.1500    ,    2.200344     point ps 2 pt 6

plot [270:300] H(T)
