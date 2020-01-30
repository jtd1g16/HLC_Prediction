# load "ref2453-98-82-8.gnu"
# chem = "(2-propyl)-benzene"

set terminal postscript eps color
set title "ref = 2453; chem = (2-propyl)-benzene; casrn = 98-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8863276E-03 * exp(  -4740.706    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1581197E-02 point
set label "" at    298.1500    ,   0.8553618E-03 point
set label "" at    308.1500    ,   0.5220935E-03 point
set label "" at    318.1500    ,   0.3333850E-03 point
set label "" at    298.1500    ,   0.8863276E-03 point ps 2 pt 6

plot [280:320] H(T)
