# load "ref0287-10025-85-1.gnu"
# chem = "nitrogen trichloride"

set terminal postscript eps color
set title "ref = 287; chem = nitrogen trichloride; casrn = 10025-85-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9920380E-03 * exp(  -4118.421    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1255591E-02 point
set label "" at    313.1500    ,   0.5118858E-03 point
set label "" at    298.1500    ,   0.9920380E-03 point ps 2 pt 6

plot [290:320] H(T)
