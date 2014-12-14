-- 
-- Problem 155: Counting Capacitor Circuits
-- (Published on Saturday, 19th May 2007, 02:00 am; Solved by 1970)
-- 
--     An electric circuit uses exclusively identical capacitors of the
-- 	same value C.
-- 
-- The capacitors can be connected in series or in parallel to form
-- 	sub-units, which can then be connected in series or in parallel
-- 	with other capacitors or other sub-units to form larger sub-units,
-- 	and so on up to a final circuit.
-- 
--     Using this simple procedure and up to n identical capacitors, we
-- 	can make circuits having a range of different total capacitances.
-- 	For example, using up to n=3 capacitors of 60
-- <img alt="" height="21" src="project/images/p155_capsmu.gif" style
-- 	="vertical-align:middle;" width="12"/>F each, we can obtain the
-- 	following 7 distinct total capacitance values:
-- 
-- <img alt="" height="557" src="project/images/p155_capacitors1.gif"
-- 	width="387"/>
--     If we denote by D(n) the number of distinct total capacitance
-- 	values we can obtain when using up to n equal-valued capacitors and
-- 	the simple procedure described above, we have: D(1)=1, D(2)=3,
-- 	D(3)=7 ...
-- 
--     Find D(18).
-- 
--     Reminder : When connecting capacitors C_{1}, C_{2} etc in
-- 	parallel, the total capacitance is C_{T} = C_{1} + C_{2} +...,
-- 
-- whereas when connecting them in series, the overall capacitance is
-- 	given by:
-- <img alt="" height="38" src="project/images/p155_capsform.gif" style
-- 	="vertical-align:middle;" width="127"/>
