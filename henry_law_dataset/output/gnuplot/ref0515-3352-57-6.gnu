# load "ref0515-3352-57-6.gnu"
# chem = "hydroxyl radical"

set terminal postscript eps color
set title "ref = 515; chem = hydroxyl radical; casrn = 3352-57-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2948795     * exp(  -4259.183    *(1/   298.    -1/T))

set label "" at    249.0000    ,    4.934616     point
set label "" at    275.0000    ,   0.9869233     point
set label "" at    298.0000    ,   0.2960770     point
set label "" at    298.1500    ,   0.2948795     point ps 2 pt 6

plot [240:300] H(T)
