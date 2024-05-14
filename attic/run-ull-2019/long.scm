(define wl (tokenize-text "Absolute pressure 117 Absolute zero 80 Accessibility of Babcock & Wilcox boiler 59 Acidity in boiler feed water 106 Actual evap. corresponding to boiler horse power 288 Advantages of Babcock & Wilcox boilers 61 Stoker firing 195 Water tube over fire tube boilers 61 Air, composition of 147 In boiler feed water 106 Properties of 147 Required for combustion 152, 156 Specific heat of 148 SupplieAbsolute pressure 117 Absolute zero 80 Accessibility of Babcock & Wilcox boiler 59 Acidity in boiler feed water 106 Actual evap.  corresponding to boiler horse power 288 Advantages of Babcock & Wilcox boilers 61 Stoker firing 195 Water tube over fire tube boilers 61 Air, composition of 147 In boiler feed water 106 Properties of 147 Required for combustion 152, 156 Specific heat of 148 Supplied for combustion 157 Vapor in 149 Volume of 147 Weight of 147 Alkalinity in boiler feed water 103 Testing feed for 103 Altitude, boiling point of water at 97 Chimney sizes corrected for 248 Alum in feed water treatment 106 A. S. M. E.  code for boiler testing 267 Analyses, comparison of proximate and ultimate 183 Proximate coal, and heating values 177 Analysis, coal, proximate, methods of 176 Coal, ultimate 173 Determination of heating value from 173 Analysis, Flue gas 155 Flue gas, methods of 160 Flue gas, object of 155 Anthracite coal 166 Combustion rates with 246 Distribution of 167 Draft required for 246 Firing 190 Grate ratio for 191 Semi 166 Sizes of 190 Steam as aid to burning 191 Thickness of fires with 191 Arches, fire brick, as aid to combustion 190 Fire brick, for 304 Fire brick, laying 305 Automatic stokers, advantages of 195 Overfeed 196 Traveling grate 197 Traveling grate, Babcock & Wilcox 194 Underfeed 196 Auxiliaries, exhaust from, in heating feed water 113 Superheated steam with 142 Auxiliary grates, with blast furnace gas 228 With oil fuel 225 With waste heat 235 Babcock, G. H., lecture on circulation of water in Boilers 28 Lecture on theory of steam making 92 Babcock & Wilcox Co., Works at Barberton, Ohio 7 Works at Bayonne, N. J. 6 Babcock & Wilcox boiler, accessibility of 59 Advantages of 61 Circulation of water in 57, 66 Construction of 49 Cross boxes 50 Cross drum 53 Cross drum, dry steam with 71 Drumheads 49 Drums 49 Durability 75 Evolution of 39 Fittings 55 Fixtures 55 Fronts 53 Handhole fittings 50, 51 Headers 50, 51 Inclined header, wrought steel 54 Inspection 75 Life of 76 Materials entering into the construction of 59 Mud drums 51 Path of gases in 57 Path of water in 57 Rear tube doors of 53, 74 Repairs 75 Safety of 66 Sections 50 Set for utilizing waste heat 236 Set with Babcock & Wilcox chain grate stoker 12 Set with bagasse furnace 208 Set with Peabody oil furnace 222 Supports, cross drum 53 Supports, longitudinal drum 52 Tube doors 53 Vertical header, cast iron 58 Vertical header, wrought steel 48 Babcock & Wilcox chain grate stoker 194 Babcock & Wilcox superheater 136 Bagasse, composition of 206 Furnace 209 Heat, value of 206 Tests of Babcock & Wilcox boilers with 210 Value of diffusion 207 Barium carbonate in feed water treatment 106 Barium hydrate in feed water treatment 106 Barrus draft gauge 254 Bituminous coal, "))

(define (mea num)

	(define short (take wl num))
	(define sent (string-join short " "))
	(report-avg-gc-cpu-time)
	(observe-mpg sent)
	(report-avg-gc-cpu-time)
)
(define (smea num)

	(define short (take wl num))
	(define sent (string-join short " "))
	(report-avg-gc-cpu-time)
	(observe-mst sent)
	(report-avg-gc-cpu-time)
)

(define (mksent len) (string-join (take wl len) " "))
(define (mkal len) (map WordNode (tokenize-text (mksent len))))
(define (mknu len) (atom-list->numa-list (mkal len) ))
(define (mktre len) (graph-add-mst '() (mknu len) ramp-scorer -1))

(define (meg len)

	(define nuli (mknu len))
	(define gra (mktre len))
	(define mpg (graph-add-mpg gra nuli ramp-scorer -1))
	(define disco (graph-add-linear mpg nuli))
	(report-avg-gc-cpu-time)
	(graph-add-bridges disco)
	(report-avg-gc-cpu-time)
)
