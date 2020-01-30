# load "ref0921-811-97-2.gnu"
# chem = "1,1,1,2-tetrafluoroethane"

set terminal postscript eps color
set title "ref = 921; chem = 1,1,1,2-tetrafluoroethane; casrn = 811-97-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1809307E-03 * exp(  -2664.116    *(1/   298.    -1/T))

set label "" at    278.1600    ,   0.3575059E-03 point
set label "" at    278.1600    ,   0.3640915E-03 point
set label "" at    308.0800    ,   0.1220060E-03 point
set label "" at    308.0800    ,   0.1219791E-03 point
set label "" at    338.1200    ,   0.6727682E-04 point
set label "" at    338.1200    ,   0.6609568E-04 point
set label "" at    298.1500    ,   0.1809307E-03 point ps 2 pt 6

plot [270:340] H(T)
