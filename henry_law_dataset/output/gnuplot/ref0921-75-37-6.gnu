# load "ref0921-75-37-6.gnu"
# chem = "1,1-difluoroethane"

set terminal postscript eps color
set title "ref = 921; chem = 1,1-difluoroethane; casrn = 75-37-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5340794E-03 * exp(  -2567.257    *(1/   298.    -1/T))

set label "" at    278.1900    ,   0.1027896E-02 point
set label "" at    278.1900    ,   0.1033848E-02 point
set label "" at    308.1300    ,   0.3706759E-03 point
set label "" at    308.1300    ,   0.3694386E-03 point
set label "" at    338.1900    ,   0.2005868E-03 point
set label "" at    338.1900    ,   0.2038376E-03 point
set label "" at    298.1500    ,   0.5340794E-03 point ps 2 pt 6

plot [270:340] H(T)
