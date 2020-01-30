# load "ref2928-7783-06-4.gnu"
# chem = "hydrogen sulfide"

set terminal postscript eps color
set title "ref = 2928; chem = hydrogen sulfide; casrn = 7783-06-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9442967E-03 * exp(  -2288.962    *(1/   298.    -1/T))

set label "" at    300.6500    ,   0.9048519E-03 point
set label "" at    300.6500    ,   0.8951107E-03 point
set label "" at    299.6500    ,   0.9747132E-03 point
set label "" at    299.6500    ,   0.9447024E-03 point
set label "" at    296.6500    ,   0.1004823E-02 point
set label "" at    296.6500    ,   0.1006495E-02 point
set label "" at    297.1500    ,   0.9925230E-03 point
set label "" at    297.1500    ,   0.1000887E-02 point
set label "" at    298.1500    ,   0.8746443E-03 point
set label "" at    298.1500    ,   0.9046551E-03 point
set label "" at    298.1500    ,   0.9152819E-03 point
set label "" at    298.1500    ,   0.8601800E-03 point
set label "" at    333.1500    ,   0.4647255E-03 point
set label "" at    333.1500    ,   0.4238910E-03 point
set label "" at    332.1500    ,   0.4417992E-03 point
set label "" at    332.1500    ,   0.4394377E-03 point
set label "" at    332.1500    ,   0.4366826E-03 point
set label "" at    332.1500    ,   0.4401264E-03 point
set label "" at    334.1500    ,   0.4096236E-03 point
set label "" at    334.1500    ,   0.4115915E-03 point
set label "" at    333.1500    ,   0.3940770E-03 point
set label "" at    333.1500    ,   0.4004727E-03 point
set label "" at    333.1500    ,   0.3903379E-03 point
set label "" at    333.1500    ,   0.4293028E-03 point
set label "" at    298.1500    ,   0.9442967E-03 point ps 2 pt 6

plot [290:340] H(T)
