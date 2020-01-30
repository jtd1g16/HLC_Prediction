# load "ref1026-75-09-2.gnu"
# chem = "dichloromethane"

set terminal postscript eps color
set title "ref = 1026; chem = dichloromethane; casrn = 75-09-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3311599E-02 * exp(  -4249.269    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.9200023E-02 point
set label "" at    283.1500    ,   0.7079426E-02 point
set label "" at    288.1500    ,   0.5420714E-02 point
set label "" at    293.1500    ,   0.4229648E-02 point
set label "" at    298.1500    ,   0.3306520E-02 point
set label "" at    298.1500    ,   0.3311599E-02 point ps 2 pt 6

plot [270:300] H(T)
