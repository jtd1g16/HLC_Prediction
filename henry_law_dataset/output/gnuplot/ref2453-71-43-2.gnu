# load "ref2453-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2453; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1679347E-02 * exp(  -3893.132    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.4427353E-02 point
set label "" at    288.1500    ,   0.2574042E-02 point
set label "" at    298.1500    ,   0.1637335E-02 point
set label "" at    308.1500    ,   0.1100237E-02 point
set label "" at    318.1500    ,   0.7539770E-03 point
set label "" at    298.1500    ,   0.1679347E-02 point ps 2 pt 6

plot [270:320] H(T)
