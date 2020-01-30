# load "ref0483-78-92-2.gnu"
# chem = "2-butanol"

set terminal postscript eps color
set title "ref = 483; chem = 2-butanol; casrn = 78-92-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.076448     * exp(  -7256.335    *(1/   298.    -1/T))

set label "" at    273.0000    ,    10.13284     point
set label "" at    298.0000    ,    1.089716     point
set label "" at    298.1500    ,    1.076448     point ps 2 pt 6

plot [270:300] H(T)
