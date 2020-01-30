# load "ref2552-74-87-3.gnu"
# chem = "chloromethane"

set terminal postscript eps color
set title "ref = 2552; chem = chloromethane; casrn = 74-87-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9928537E-03 * exp(  -2483.496    *(1/   298.    -1/T))

set label "" at    283.4000    ,   0.1648162E-02 point
set label "" at    296.7000    ,   0.9661979E-03 point
set label "" at    310.1000    ,   0.7046632E-03 point
set label "" at    310.6000    ,   0.6819640E-03 point
set label "" at    332.4000    ,   0.4460893E-03 point
set label "" at    298.1500    ,   0.9928537E-03 point ps 2 pt 6

plot [280:340] H(T)
