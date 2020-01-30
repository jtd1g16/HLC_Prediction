# load "ref0759-13863-41-7.gnu"
# chem = "bromine chloride"

set terminal postscript eps color
set title "ref = 759; chem = bromine chloride; casrn = 13863-41-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4184433E-01 * exp(  -3958.691    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.7452027E-01 point
set label "" at    298.1500    ,   0.4482172E-01 point
set label "" at    318.1500    ,   0.1750170E-01 point
set label "" at    278.1500    ,   0.1137898     point
set label "" at    298.1500    ,   0.4690645E-01 point
set label "" at    303.1500    ,   0.3148747E-01 point
set label "" at    298.1500    ,   0.4184433E-01 point ps 2 pt 6

plot [270:320] H(T)
