# load "ref2645-637-92-3.gnu"
# chem = "ethyl {tert}-butyl ether"

set terminal postscript eps color
set title "ref = 2645; chem = ethyl {tert}-butyl ether; casrn = 637-92-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6285602E-02 * exp(  -6590.679    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3088579E-01 point
set label "" at    288.1500    ,   0.1346435E-01 point
set label "" at    298.1500    ,   0.6303054E-02 point
set label "" at    298.1500    ,   0.6285602E-02 point ps 2 pt 6

plot [270:300] H(T)
