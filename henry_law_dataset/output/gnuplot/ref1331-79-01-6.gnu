# load "ref1331-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 1331; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8979808E-03 * exp(  -5433.934    *(1/   298.    -1/T))

set label "" at    319.1500    ,   0.2672710E-03 point
set label "" at    314.6500    ,   0.3539275E-03 point
set label "" at    310.0500    ,   0.4408100E-03 point
set label "" at    298.1500    ,   0.8979808E-03 point ps 2 pt 6

plot [310:320] H(T)
