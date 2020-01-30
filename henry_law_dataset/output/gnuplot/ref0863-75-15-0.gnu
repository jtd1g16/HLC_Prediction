# load "ref0863-75-15-0.gnu"
# chem = "carbon disulfide"

set terminal postscript eps color
set title "ref = 863; chem = carbon disulfide; casrn = 75-15-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5444330E-03 * exp(  -4260.601    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2009341E-02 point
set label "" at    283.1500    ,   0.1192641E-02 point
set label "" at    293.1500    ,   0.6611379E-03 point
set label "" at    303.1500    ,   0.4407586E-03 point
set label "" at    298.1500    ,   0.5444330E-03 point ps 2 pt 6

plot [270:310] H(T)
