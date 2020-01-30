# load "ref2485-75-72-9.gnu"
# chem = "chlorotrifluoromethane"

set terminal postscript eps color
set title "ref = 2485; chem = chlorotrifluoromethane; casrn = 75-72-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9230734E-05 * exp(  -1899.907    *(1/   298.    -1/T))

set label "" at    288.2200    ,   0.1173430E-04 point
set label "" at    298.0600    ,   0.9091244E-05 point
set label "" at    308.0200    ,   0.7305681E-05 point
set label "" at    318.0200    ,   0.6364958E-05 point
set label "" at    298.1500    ,   0.9230734E-05 point ps 2 pt 6

plot [280:320] H(T)
