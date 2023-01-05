;
; pmi.scm - pointwise MI for three word sentences.
; Hackery for the demo

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
(format #t "pfff = ~A\n" pfff)
(format #t "pfft = ~A\n" pfft)
(format #t "pftf = ~A\n" pftf)
(format #t "pftt = ~A\n" pftt)
(format #t "ptff = ~A\n" ptff)
(format #t "ptft = ~A\n" ptft)
(format #t "pttf = ~A\n" pttf)
(format #t "pttt = ~A\n" pttt)
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

(define (log2 x) (/ (log x) (log 2)))

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
; Disjuncts, by hand. Ugh.  Accurate only for the given parse.

(define p-sunny-calm (* 0.5 (+ pfff pfft)))
(define p-sunny-dry (* 0.5 pfff))
(define p-sunny-calm-wet (* 0.5 pfft))
(define p-sunny-wet pftt)
(define p-sunny-rain-dry pftf)

(define p-cloudy-calm ptff)
(define p-cloudy-dry (* 0.5 ptff))
(define p-cloudy-wet ptft)
(define p-cloudy-rain-dry pttf)

(define p-sunny (+ p-sunny-calm p-sunny-dry p-sunny-calm-wet p-sunny-wet p-sunny-rain-dry))
(define p-calm+ (+ p-sunny-calm p-cloudy-calm))
(define p-dry+ (+ p-sunny-dry p-cloudy-dry))
(define p-wet+ (+ p-sunny-wet p-cloudy-wet))
(define p-calm+wet+ p-sunny-calm-wet)
(define p-rain+dry+ (+ p-sunny-rain-dry p-cloudy-rain-dry))

(define mi-sunny-calm (log2 (/ p-sunny-calm (* p-calm+ p-sunny))))
(format #t "mi-sunny-calm = ~A\n" mi-sunny-calm)

(define mi-sunny-dry (log2 (/ p-sunny-dry (* p-dry+ p-sunny))))
(format #t "mi-sunny-dry = ~A\n" mi-sunny-dry)

(define mi-sunny-calm-wet (log2 (/ p-sunny-calm-wet (* p-calm+wet+ p-sunny))))
(format #t "mi-sunny-calm-wet = ~A\n" mi-sunny-calm-wet)

(define mi-sunny-wet (log2 (/ p-sunny-wet (* p-wet+ p-sunny))))
(format #t "mi-sunny-wet = ~A\n" mi-sunny-wet)

(define mi-sunny-rain-dry (log2 (/ p-sunny-rain-dry (* p-rain+dry+ p-sunny))))
(format #t "mi-sunny-rain-dry = ~A\n" mi-sunny-rain-dry)
