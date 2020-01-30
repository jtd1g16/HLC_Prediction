# load "ref0477-7782-50-5.gnu"
# chem = "molecular chlorine"

set terminal postscript eps color
set title "ref = 477; chem = molecular chlorine; casrn = 7782-50-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9262785E-03 * exp(  -2068.855    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1354532E-02 point
set label "" at    293.1500    ,   0.1026823E-02 point
set label "" at    303.1500    ,   0.8192733E-03 point
set label "" at    313.1500    ,   0.6718041E-03 point
set label "" at    298.1500    ,   0.9262785E-03 point ps 2 pt 6

plot [280:320] H(T)
