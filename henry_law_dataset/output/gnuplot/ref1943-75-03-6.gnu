# load "ref1943-75-03-6.gnu"
# chem = "iodoethane"

set terminal postscript eps color
set title "ref = 1943; chem = iodoethane; casrn = 75-03-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1460846E-02 * exp(  -4179.785    *(1/   298.    -1/T))

set label "" at    273.2000    ,   0.5413710E-02 point
set label "" at    278.2000    ,   0.3988000E-02 point
set label "" at    283.2000    ,   0.3010940E-02 point
set label "" at    288.2000    ,   0.2313040E-02 point
set label "" at    293.2000    ,   0.1814540E-02 point
set label "" at    298.2000    ,   0.1455620E-02 point
set label "" at    303.2000    ,   0.1196400E-02 point
set label "" at    298.1500    ,   0.1460846E-02 point ps 2 pt 6

plot [270:310] H(T)
