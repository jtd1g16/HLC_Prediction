# load "ref0862-463-58-1.gnu"
# chem = "carbon oxide sulfide"

set terminal postscript eps color
set title "ref = 862; chem = carbon oxide sulfide; casrn = 463-58-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2085897E-03 * exp(  -3306.492    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.5869415E-03 point
set label "" at    283.1500    ,   0.3676640E-03 point
set label "" at    293.1500    ,   0.2470174E-03 point
set label "" at    303.1500    ,   0.1774474E-03 point
set label "" at    298.1500    ,   0.2085897E-03 point ps 2 pt 6

plot [270:310] H(T)
