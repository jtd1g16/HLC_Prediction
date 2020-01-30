# load "ref0800-15932-89-5.gnu"
# chem = "hydroxymethyl hydroperoxide"

set terminal postscript eps color
set title "ref = 800; chem = hydroxymethyl hydroperoxide; casrn = 15932-89-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    4688.922     * exp(  -1498.102    *(1/   298.    -1/T))

set label "" at    283.1500    ,    6118.924     point
set label "" at    295.1500    ,    4934.616     point
set label "" at    298.1500    ,    4688.922     point ps 2 pt 6

plot [280:300] H(T)
