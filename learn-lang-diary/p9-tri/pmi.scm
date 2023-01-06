;
; pmi.scm - pointwise MI for three word sentences.
; Hackery for Chapter 9 exploration.

(define fff 10)        ; sunny calm dry
(define fft 10)        ; sunny calm wet
(define ftf 0.000041)  ; sunny rain dry
(define ftt 0.0041)    ; sunny rain wet
(define tff 5)         ; cloud calm dry
(define tft 5)         ; cloud calm wet
(define ttf 0.0000041) ; cloud rain dry
(define ttt 10)        ; cloud rain wet

(define N (+ fff fft ftf ftt tff tft ttf ttt)) 

(define pfff (/ fff N))
(define pfft (/ fft N))
(define pftf (/ ftf N))
(define pftt (/ ftt N))
(define ptff (/ tff N))
(define ptft (/ tft N))
(define pttf (/ ttf N))
(define pttt (/ ttt N))

(define (log2 x) (/ (log x) (log 2)))

(format #t "pfff = ~A ~A\n" pfff (log2 pfff))
(format #t "pfft = ~A ~A\n" pfft (log2 pfft))
(format #t "pftf = ~A ~A\n" pftf (log2 pftf))
(format #t "pftt = ~A ~A\n" pftt (log2 pftt))
(format #t "ptff = ~A ~A\n" ptff (log2 ptff))
(format #t "ptft = ~A ~A\n" ptft (log2 ptft))
(format #t "pttf = ~A ~A\n" pttf (log2 pttf))
(format #t "pttt = ~A ~A\n" pttt (log2 pttt))
(format #t "----\n")

(define pffs (+ pfff pfft))
(define pfts (+ pftf pftt))
(define ptfs (+ ptff ptft))
(define ptts (+ pttf pttt))
(format #t "pffs = ~A\n" pffs)
(format #t "pfts = ~A\n" pfts)
(format #t "ptfs = ~A\n" ptfs)
(format #t "ptts = ~A\n" ptts)
(format #t "----\n")

(define pfsf (+ pfff pftf))
(define pfst (+ pfft pftt))
(define ptsf (+ ptff pttf))
(define ptst (+ ptft pttt))

(define psff (+ pfff ptff))
(define psft (+ pfft ptft))
(define pstf (+ pftf pttf))
(define pstt (+ pftt pttt))

(define pfss (+ pffs pfts))
(define ptss (+ ptfs ptts))
(format #t "check ~A\n" (+ pfss ptss))

(define psfs (+ pffs ptfs))
(define psts (+ pfts ptts))
(format #t "check ~A\n" (+ psfs psts))

(define pssf (+ pfsf ptsf))
(define psst (+ pfst ptst))
(format #t "check ~A\n" (+ pssf psst))

(define mifff (log2 (/ (* psff pfsf pffs) (* pfff pfss psfs pssf))))
(format #t "mifff = ~A\n" mifff)

(define mifft (log2 (/ (* psft pfst pffs) (* pfft pfss psfs psst))))
(format #t "mifft = ~A\n" mifft)

(define miftf (log2 (/ (* pstf pfsf pfts) (* pftf pfss psts pssf))))
(format #t "miftf = ~A\n" miftf)

(define miftt (log2 (/ (* pstt pfst pfts) (* pftt pfss psts psst))))
(format #t "miftt = ~A\n" miftt)
(format #t "----\n")

(define mitff (log2 (/ (* psff ptsf ptfs) (* ptff ptss psfs pssf))))
(format #t "mitff = ~A\n" mitff)

(define mitft (log2 (/ (* psft ptst ptfs) (* ptft ptss psfs psst))))
(format #t "mitft = ~A\n" mitft)

(define mittf (log2 (/ (* pstf ptsf ptts) (* pttf ptss psts pssf))))
(format #t "mittf = ~A\n" mittf)

(define mittt (log2 (/ (* pstt ptst ptts) (* pttt ptss psts psst))))
(format #t "mittt = ~A\n" mittt)
(format #t "----\n")


(define tot (+
	(* pfff mifff)
	(* pfft mifft)
	(* pftf miftf)
	(* pftt miftt)
	(* ptff mitff)
	(* ptft mitft)
	(* pttf mittf)
	(* pttt mittt)))

(format #t "tot mi ~A\n" tot)
(format #t "----\n")

(define imfff (log2 (/ pfff (* pfss psfs pssf))))
(format #t "imfff = ~A\n" imfff)

(define imfft (log2 (/ pfft (* pfss psfs psst))))
(format #t "imfft = ~A\n" imfft)

(define imftf (log2 (/ pftf (* pfss psts pssf))))
(format #t "imftf = ~A\n" imftf)

(define imftt (log2 (/ pftt (* pfss psts psst))))
(format #t "imftt = ~A\n" imftt)
(format #t "----\n")

(define imtff (log2 (/ ptff (* ptss psfs pssf))))
(format #t "imtff = ~A\n" imtff)

(define imtft (log2 (/ ptft (* ptss psfs psst))))
(format #t "imtft = ~A\n" imtft)

(define imttf (log2 (/ pttf (* ptss psts pssf))))
(format #t "imttf = ~A\n" imttf)

(define imttt (log2 (/ pttt (* ptss psts psst))))
(format #t "imttt = ~A\n" imttt)
(format #t "----\n")

(define imtot (+
	(* pfff imfff)
	(* pfft imfft)
	(* pftf imftf)
	(* pftt imftt)
	(* ptff imtff)
	(* ptft imtft)
	(* pttf imttf)
	(* pttt imttt)))

(format #t "tot ic ~A\n" imtot)
(format #t "----\n")

; =========================================================
(define misff (log2 (/ psff (* pssf psfs))))
(define mifsf (log2 (/ pfsf (* pssf pfss))))
(define miffs (log2 (/ pffs (* pfss psfs))))

(define zsff (expt 2 misff))
(define zfsf (expt 2 mifsf))
(define zffs (expt 2 miffs))

(define zfff (+ zsff zfsf zffs))

(define wifff (/ (+ (* zsff misff) (* zfsf mifsf) (* zffs miffs)) zfff))
(format #t "zfff=~A and wifff=~A\n" zfff wifff)

; =========================================================
(define misft (log2 (/ psft (* psst psfs))))
(define mifst (log2 (/ pfst (* psst pfss))))

(define zsft (expt 2 misft))
(define zfst (expt 2 mifst))

(define zfft (+ zsft zfst zffs))

(define wifft (/ (+ (* zsft misft) (* zfst mifst) (* zffs miffs)) zfft))
(format #t "zfft=~A and wifft=~A\n" zfft wifft)

; =========================================================
(define mistf (log2 (/ pstf (* psts pssf))))
(define mifts (log2 (/ pfts (* pfss psts))))

(define zstf (expt 2 mistf))
(define zfts (expt 2 mifts))

(define zftf (+ zstf zfsf zfts))

(define wiftf (/ (+ (* zstf mistf) (* zfsf mifsf) (* zfts mifts)) zftf))
(format #t "zftf=~A and wiftf=~A\n" zftf wiftf)

; =========================================================
(define zftt (+ zsff zfst zfts))

(define wiftt (/ (+ (* zsff misff) (* zfst mifst) (* zfts mifts)) zftt))
(format #t "zftt=~A and wiftt=~A\n" zftt wiftt)
(format #t "----\n")

; =========================================================
(define mistt (log2 (/ pstt (* psst psts))))
(define mitst (log2 (/ ptst (* ptss psst))))
(define mitts (log2 (/ ptts (* ptss psts))))

(define mitfs (log2 (/ ptfs (* ptss psfs))))
(define mitsf (log2 (/ ptsf (* ptss pssf))))
; (define mistf (log2 (/ pstf (* psts pssf))))

; =========================================================
(format #t "miffs = ~A\n" miffs)
(format #t "mifts = ~A\n" mifts)
(format #t "mitfs = ~A\n" mitfs)
(format #t "mitts = ~A\n" mitts)
(format #t "----\n")

(format #t "mifsf = ~A\n" mifsf)
(format #t "mifst = ~A\n" mifst)
(format #t "mitsf = ~A\n" mitsf)
(format #t "mitst = ~A\n" mitst)
(format #t "----\n")

(format #t "misff = ~A\n" misff)
(format #t "misft = ~A\n" misft)
(format #t "mistf = ~A\n" mistf)
(format #t "mistt = ~A\n" mistt)
(format #t "----\n")

; =========================================================
; MST parses, by hand

(define tree-fff (+ mifsf misff))
(define tree-fft (+ miffs misft))
(define tree-ftf (+ mifts mifsf))
(define tree-ftt (+ mifst mistt))

(define tree-tff (+ mitfs mitfs))
(define tree-tft (+ mitst misft))
(define tree-ttf (+ mitts mitsf))
(define tree-ttt (+ mitts mistt))

(format #t "mst-fff = ~A\n" tree-fff)
(format #t "mst-fft = ~A\n" tree-fft)
(format #t "mst-ftf = ~A\n" tree-ftf)
(format #t "mst-ftt = ~A\n" tree-ftt)
(format #t "----\n")
(format #t "mst-tff = ~A\n" tree-tff)
(format #t "mst-tft = ~A\n" tree-tft)
(format #t "mst-ttf = ~A\n" tree-ttf)
(format #t "mst-ttt = ~A\n" tree-ttt)
(format #t "----\n")

(define norm-fff (+ pfsf psff))
(define norm-fft (+ pffs psft))
(define norm-ftf (+ pfts pfsf))
(define norm-ftt (+ pfst pstt))

(define norm-tff (+ ptfs ptfs))
(define norm-tft (+ ptst psft))
(define norm-ttf (+ ptts ptsf))
(define norm-ttt (+ ptts pstt))

(define wtree-fff (/ (+ (* pfsf mifsf) (* psff misff)) norm-fff))
(define wtree-fft (/ (+ (* pffs miffs) (* psft misft)) norm-fft))
(define wtree-ftf (/ (+ (* pfts mifts) (* pfsf mifsf)) norm-ftf))
(define wtree-ftt (/ (+ (* pfst mifst) (* pstt mistt)) norm-ftt))

(define wtree-tff (/ (+ (* ptfs mitfs) (* ptfs mitfs)) norm-tff))
(define wtree-tft (/ (+ (* ptst mitst) (* psft misft)) norm-tft))
(define wtree-ttf (/ (+ (* ptts mitts) (* ptsf mitsf)) norm-ttf))
(define wtree-ttt (/ (+ (* ptts mitts) (* pstt mistt)) norm-ttt))

(format #t "wmst-fff = ~A\n" wtree-fff)
(format #t "wmst-fft = ~A\n" wtree-fft)
(format #t "wmst-ftf = ~A\n" wtree-ftf)
(format #t "wmst-ftt = ~A\n" wtree-ftt)
(format #t "----\n")
(format #t "wmst-tff = ~A\n" wtree-tff)
(format #t "wmst-tft = ~A\n" wtree-tft)
(format #t "wmst-ttf = ~A\n" wtree-ttf)
(format #t "wmst-ttt = ~A\n" wtree-ttt)
(format #t "----\n")

; =========================================================
; Disjuncts, by hand. Ugh.  Accurate only for the given parse.

(define p-sunny:calm (* 0.5 (+ pfff pfft)))
(define p-sunny:dry (* 0.5 pfff))
(define p-sunny:calm-wet (* 0.5 pfft))
(define p-sunny:wet pftt)
(define p-sunny:rain-dry pftf)
(define p-sunny (+ p-sunny:calm p-sunny:dry p-sunny:calm-wet p-sunny:wet p-sunny:rain-dry))

(define p-cloudy:rain pttt)
(define p-cloudy:wet ptft)
(define p-cloudy:calm ptff)
(define p-cloudy:dry (* 0.5 ptff))
(define p-cloudy:rain-dry pttf)

(define p-cloudy (+ p-cloudy:rain p-cloudy:wet p-cloudy:calm p-cloudy:dry p-cloudy:rain-dry))

(define p-calm:dry (+ (* 0.5 pfff) (* 0.5 ptff)))
(define p-calm:wet ptft)
(define p-calm:sunny (* 0.5 pfft))
(define p-calm:sunny-dry (* 0.5 pfff))
(define p-calm:sunny-wet (* 0.5 pfft))
(define p-calm:cloudy-dry (* 0.5 ptff))

(define p-calm (+ p-calm:dry p-calm:wet p-calm:sunny p-calm:sunny-dry p-calm:sunny-wet p-calm:cloudy-dry))

(define p-rain:sunny pftf)
(define p-rain:cloudy pttf)
(define p-rain:wet pftt)
(define p-rain:cloudy-wet pttt)

(define p-rain (+ p-rain:sunny p-rain:cloudy p-rain:wet p-rain:cloudy-wet))

(define p-dry:cloudy pttf)
(define p-dry:calm (+ (* 0.5 pfff) (* 0.5 ptff)))
(define p-dry:sunny-calm (* 0.5 pfff))
(define p-dry:cloudy-calm (* 0.5 ptff))
(define p-dry:sunny pftf)

(define p-dry (+ p-dry:cloudy p-dry:calm p-dry:sunny-calm p-dry:cloudy-calm p-dry:sunny))

(define p-wet:cloudy-calm ptft)
(define p-wet:sunny (* 0.5 pfft))
(define p-wet:calm (* 0.5 pfft))
(define p-wet:rain pttt)
(define p-wet:sunny-rain pftt)

(define p-wet (+ p-wet:cloudy-calm p-wet:sunny p-wet:calm p-wet:rain p-wet:sunny-rain))

; -----------------------------------------
(define p-calm+ (+ p-sunny:calm p-cloudy:calm))
(define p-dry+ (+ p-sunny:dry p-cloudy:dry p-calm:dry))
(define p-wet+ (+ p-sunny:wet p-cloudy:wet p-calm:wet p-rain:wet))
(define p-calm+wet+ p-sunny:calm-wet)

(define p-rain+ p-cloudy:rain)
(define p-rain+dry+ (+ p-sunny:rain-dry p-cloudy:rain-dry))

(define p-sunny- (+ p-calm:sunny p-rain:sunny p-dry:sunny p-wet:sunny))
(define p-sunny-dry+ p-calm:sunny-dry)
(define p-sunny-wet+ p-calm:sunny-wet)
(define p-cloudy-dry+ p-calm:cloudy-dry)

(define p-cloudy- (+ p-rain:cloudy p-dry:cloudy))
(define p-cloudy-wet+ p-rain:cloudy-wet)

(define p-calm- (+ p-dry:calm p-wet:calm))
(define p-sunny-calm- p-dry:sunny)
(define p-cloudy-calm- (+ p-dry:cloudy-calm p-wet:cloudy-calm))

(define p-rain- p-wet:rain)
(define p-sunny-rain- p-wet:sunny-rain)

;===================

(define mi-sunny:calm (log2 (/ p-sunny:calm (* p-calm+ p-sunny))))
(format #t "mi-sunny:calm = ~A\n" mi-sunny:calm)

(define mi-sunny:dry (log2 (/ p-sunny:dry (* p-dry+ p-sunny))))
(format #t "mi-sunny:dry = ~A\n" mi-sunny:dry)

(define mi-sunny:calm-wet (log2 (/ p-sunny:calm-wet (* p-calm+wet+ p-sunny))))
(format #t "mi-sunny:calm-wet = ~A\n" mi-sunny:calm-wet)

(define mi-sunny:wet (log2 (/ p-sunny:wet (* p-wet+ p-sunny))))
(format #t "mi-sunny:wet = ~A\n" mi-sunny:wet)

(define mi-sunny:rain-dry (log2 (/ p-sunny:rain-dry (* p-rain+dry+ p-sunny))))
(format #t "mi-sunny:rain-dry = ~A\n" mi-sunny:rain-dry)
(format #t "----\n")

;===================
(define mi-cloudy:rain (log2 (/ p-cloudy:rain (* p-rain+ p-cloudy))))
(format #t "mi-cloudy:rain = ~A\n" mi-cloudy:rain)

(define mi-cloudy:calm (log2 (/ p-cloudy:calm (* p-calm+ p-cloudy))))
(format #t "mi-cloudy:calm = ~A\n" mi-cloudy:calm)

(define mi-cloudy:dry (log2 (/ p-cloudy:dry (* p-dry+ p-cloudy))))
(format #t "mi-cloudy:dry = ~A\n" mi-cloudy:dry)

(define mi-cloudy:wet (log2 (/ p-cloudy:wet (* p-wet+ p-cloudy))))
(format #t "mi-cloudy:wet = ~A\n" mi-cloudy:wet)

(define mi-cloudy:rain-dry (log2 (/ p-cloudy:rain-dry (* p-rain+dry+ p-cloudy))))
(format #t "mi-cloudy:rain-dry = ~A\n" mi-cloudy:rain-dry)
(format #t "----\n")
;===================

(define mi-calm:dry (log2 (/ p-calm:dry (* p-calm p-dry+))))
(format #t "mi-calm:dry = ~A\n" mi-calm:dry)

(define mi-calm:wet (log2 (/ p-calm:wet (* p-calm p-wet+))))
(format #t "mi-calm:wet = ~A\n" mi-calm:wet)

(define mi-calm:sunny (log2 (/ p-calm:sunny (* p-calm p-sunny-))))
(format #t "mi-calm:sunny = ~A\n" mi-calm:sunny)

(define mi-calm:sunny-dry (log2 (/ p-calm:sunny-dry (* p-calm p-sunny-dry+))))
(format #t "mi-calm:sunny-dry = ~A\n" mi-calm:sunny-dry)

(define mi-calm:sunny-wet (log2 (/ p-calm:sunny-wet (* p-calm p-sunny-wet+))))
(format #t "mi-calm:sunny-wet = ~A\n" mi-calm:sunny-wet)

(define mi-calm:cloudy-dry (log2 (/ p-calm:cloudy-dry (* p-calm p-cloudy-dry+))))
(format #t "mi-calm:cloudy-dry = ~A\n" mi-calm:cloudy-dry)
(format #t "----\n")
;===================

(define mi-rain:sunny (log2 (/ p-rain:sunny (* p-rain p-sunny-))))
(format #t "mi-rain:sunny = ~A\n" mi-rain:sunny)

(define mi-rain:cloudy (log2 (/ p-rain:cloudy (* p-rain p-cloudy-))))
(format #t "mi-rain:cloudy = ~A\n" mi-rain:cloudy)

(define mi-rain:wet (log2 (/ p-rain:wet (* p-rain p-wet+))))
(format #t "mi-rain:wet = ~A\n" mi-rain:wet)

(define mi-rain:cloudy-wet (log2 (/ p-rain:cloudy-wet (* p-rain p-cloudy-wet+))))
(format #t "mi-rain:cloudy-wet = ~A\n" mi-rain:cloudy-wet)

(format #t "----\n")
;===================

(define mi-dry:cloudy (log2 (/ p-dry:cloudy (* p-dry p-cloudy-))))
(format #t "mi-dry:cloudy = ~A\n" mi-dry:cloudy)

(define mi-dry:calm (log2 (/ p-dry:calm (* p-dry p-calm-))))
(format #t "mi-dry:calm = ~A\n" mi-dry:calm)

(define mi-dry:sunny-calm (log2 (/ p-dry:sunny-calm (* p-dry p-sunny-calm-))))
(format #t "mi-dry:sunny-calm = ~A\n" mi-dry:sunny-calm)

(define mi-dry:cloudy-calm (log2 (/ p-dry:cloudy-calm (* p-dry p-cloudy-calm-))))
(format #t "mi-dry:cloudy-calm = ~A\n" mi-dry:cloudy-calm)

(define mi-dry:sunny (log2 (/ p-dry:sunny (* p-dry p-sunny-))))
(format #t "mi-dry:sunny = ~A\n" mi-dry:sunny)

(format #t "----\n")
;===================

(define mi-wet:sunny (log2 (/ p-wet:sunny (* p-wet p-sunny-))))
(format #t "mi-wet:sunny = ~A\n" mi-wet:sunny)

(define mi-wet:calm (log2 (/ p-wet:calm (* p-wet p-calm-))))
(format #t "mi-wet:calm = ~A\n" mi-wet:calm)

(define mi-wet:rain (log2 (/ p-wet:rain (* p-wet p-rain-))))
(format #t "mi-wet:rain = ~A\n" mi-wet:rain)

(define mi-wet:sunny-rain (log2 (/ p-wet:sunny-rain (* p-wet p-sunny-rain-))))
(format #t "mi-wet:sunny-rain = ~A\n" mi-wet:sunny-rain)

(define mi-wet:cloudy-calm (log2 (/ p-wet:cloudy-calm (* p-wet p-cloudy-calm-))))
(format #t "mi-wet:cloudy-calm = ~A\n" mi-wet:cloudy-calm)

(format #t "----\n")
;===================
