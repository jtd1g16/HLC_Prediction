# load "ref0764-75-73-0.gnu"
# chem = "tetrafluoromethane"

set terminal postscript eps color
set title "ref = 764; chem = tetrafluoromethane; casrn = 75-73-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1998043E-05 * exp(  -1537.254    *(1/   298.    -1/T))

set label "" at    279.8500    ,   0.2853469E-05 point
set label "" at    286.2500    ,   0.2458373E-05 point
set label "" at    292.1500    ,   0.2194976E-05 point
set label "" at    297.8500    ,   0.1975479E-05 point
set label "" at    311.6500    ,   0.1624282E-05 point
set label "" at    298.1500    ,   0.1998043E-05 point ps 2 pt 6

plot [270:320] H(T)
