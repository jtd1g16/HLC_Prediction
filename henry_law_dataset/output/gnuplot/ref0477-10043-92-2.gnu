# load "ref0477-10043-92-2.gnu"
# chem = "radon"

set terminal postscript eps color
set title "ref = 477; chem = radon; casrn = 10043-92-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9216993E-04 * exp(  -2573.315    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1255673E-03 point
set label "" at    293.1500    ,   0.1062324E-03 point
set label "" at    298.1500    ,   0.9126704E-04 point
set label "" at    303.1500    ,   0.7957874E-04 point
set label "" at    308.1500    ,   0.7034827E-04 point
set label "" at    298.1500    ,   0.9216993E-04 point ps 2 pt 6

plot [280:310] H(T)
