*****************************
* MATCHPATCH PROGRAM SOURCE *
*        VERSION 1.0        *
*     FINISHED 12-10-90     *
*           by SD           *
*****************************

	OPT	C-,D-
	SECTION	platform0,CODE_C

	incdir ":include/"
	include	headers/hardware.i
	include exec/exec_lib.i
	include	libraries/dosextens.i

OpenLib		equ	-552
CloseLib	equ	-414
Forbid		equ	-132
Permit		equ	-138
AllocMem	equ	-198
FreeMem		equ	-210
StartList	equ	38
Execbase	equ	4
width		equ	40		game screen width
high		equ	200		game screen height
top		equ	40		game screen off top
bot		equ	32		game screen off bottom
fscsize		equ	width*(top+bot+high)
bscsize		equ	width*(high+6)
pscsize		equ	width*56

COPPER	MACRO
	move.w	\1,\2			put addr. into copper list
	swap	\1
	move.w	\1,\3
	ENDM
UNUSED	MACRO
	dc.l	0,0,0,0,0,0,0,0
	dc.l	0,0,0,0,0,0,0,0,0,0,0
	dc.l	0,0,0,0,0,0
	dc.l	0,0,0,$ff000000,0,0
	ENDM
SOUND	MACRO
	tst.w	scount\1		if sound channel on no new sound
	bne.s	\@
	move.l	\2,aud\1lch(a5)
	move.w	\3,aud\1len(a5)
	move.w	\4,aud\1vol(a5)
	move.w	\5,aud\1per(a5)
	move.w	\6,dmacon(a5)
	move.w	\7,scount\1
\@
	ENDM

startup	movem.l	d0/a0,-(sp)		FROM WORKBENCH OR CLI
	clr.l	workmsg			save command line.clear variable
	sub.l	a1,a1			zero a1
	CALLEXEC	FindTask	find our task
	move.l	d0,a4			save
	tst.l	pr_CLI(a4)		from cli
	beq	workbench		no so workbench
	bra	kickoff
workbench
	lea	pr_MsgPort(a4),a0
	CALLEXEC	WaitPort	wait for startup message
	lea	pr_MsgPort(a4),a0
	CALLEXEC	GetMsg		get message
	move.l	d0,workmsg		save till end
kickoff	movem.l	(sp)+,d0/a0		restore command line
	bsr	prog
	move.l	d0,-(sp)		save your exit code
	tst.l	workmsg			was it cli
	beq	todos			yes
	CALLEXEC	Forbid
	move.l	workmsg,a1		get workbench's mesage
	CALLEXEC	ReplyMsg	return it
todos	move.l	(sp)+,d0		exit code.if any
	rts

prog	move.l	#GRname,a1		GET OLD COPPER ADDR.
	clr.l	d0
	move.l	Execbase,a6
	jsr	OpenLib(a6)
	move.l	d0,a1
	move.l	StartList(a1),oldcop
	move.l	Execbase,a6
	jsr	CloseLib(a6)

	bsr	tpoint			set up copper list

	move.l	Execbase,a6		SET COPPER/SCREEN 
	jsr	Forbid(a6)
	lea	$dff000,a5		base address always in a5
	move.w	#$0180,dmacon(a5)	bits 7-8 off
	move.l	#tlist,cop1lc(a5)
	clr.w	copjmp1(a5)
	move.w	#$0038,ddfstrt(a5)	SET OVERALL SCREEN MODE
	move.w	#$00d0,ddfstop(a5)
	move.l	#0,bpl1mod(a5)		clears bpl1/2mod
	move.w	#$83f0,dmacon(a5)	bits 4-9 on
*****************************************************************************
	move.w	#$8010,intena(a5)	SET INTERRUPT
	move.l	$6c,old			save old interrupt adrr.
	move.l	#inter,$6c		put in our addr.
wait	btst	#6,ciaapra		left mouse press pauses game
	bne.s	play
pause	bsr	soff			turns sounds off if paused
	btst	#10,potgor(a5)		right mouse press restarts game
	bne.s	pause
play	bsr	soff			turn sounds off
	bsr	vbgap			synchronize with blanking gap

	tst.b	mode			mode flags set?
	beq.s	xtest			no
	btst	#0,mode			set up game screen
	bne	drawsc
	btst	#2,mode			put on hero
	bne	heroon
	btst	#3,mode			parachute hero
	bne	float
	btst	#4,mode			extra points screen delay
	bne	exscdy
	btst	#5,mode			set up maze
	bne	setmaze
	btst	#6,mode			maze mode
	bne	mazed
	btst	#7,mode			next level
	bne	quickch
xtest	btst	#0,xmode		all dead.end game/hiscore routine
	bne	endall
	btst	#1,xmode		set up title screen
	bne	tsetup
	btst	#2,xmode		fire pressed
	bne	fwait

mainlp	bsr	blank
	bsr	alter
	bsr	detect			hit hero?
	bsr	strike			hit missile?
	bsr	mover
	bsr	control			alien movement routines
	bsr	missile			move missiles
	bsr	collect			bonus timer/detection
	bsr	extras
	bsr	testend
	bsr	put
	bsr	hfire			fire missile.display next frame
	bsr	disp			displays flipin screens
	bsr	switch			switches screens in flipin/out
	bsr	doscore
	bsr	counts			timer
*	move.w	#$444,color00(a5)	*** test only ***
	bra	wait

drawsc	bsr	gpoint			SET UP GAME SCREEN
	bsr	disp			display first screen
	bsr	switch			change to display second screen
	bsr	dosc1
	bsr	dolives
	bsr	dolevel
	bsr	copylvl			
	bsr	dcount			display level timer
	bsr	wipesc
	bsr	wipems
	bsr	setblks
	bsr	grow
	bsr	stsc			draw background screen
	bsr	disphw			set up hw sprites
	bsr	setord			set up order graphics
	bsr	newshot			set pointer pos.
	move.b	#4,mode			set bit 2
	bra	wait
heroon	bsr	chuteon			PUT PARACHUTE ON
	move.b	#8,mode			set bit 3
	bra	wait
float	bsr	chute			MOVE PARACHUTE DOWN
	bra	wait
exscdy	subq.w	#1,exsct		DELAY FOR BONUS POINTS SCREENS
	beq.s	exscdy1
	bsr	killall			ensures bullets are removed
	bsr	blank
	bsr	alter
	bsr	put
	bsr	disp			displays flipin screens
	bsr	switch			switches screens in flipin/out
	bra	wait
exscdy1	move.b	#128,mode		set bit 7
	bra	wait
setmaze	bsr	killall			SET UP MAZE
	bsr	wipesc
	bsr	wipems
	bsr	remhw
	bsr	whichm			select maze
	bsr	grow			put minimap into scr6
	bsr	stsc
	move.b	#64,mode		set bit 6
	bset	#0,bdhero+89		set baldy on
	bra	wait
mazed	bsr	blank			MAZE LOOP
	bsr	alter
	bsr	waddle
	bsr	put
	bsr	disp			displays flipin screens
	bsr	switch			switches screens in flipin/out
	bsr	doscore
	bsr	mcount			timer
*	move.w	#$444,color00(a5)	*** test only ***
	bra	wait
quickch	bsr	nextlvl
	bra	wait
endall	bsr	killall			END GAME/HISCORE ROUTINE
	bsr	wipesc
	bsr	wipems
	bsr	remhw
	bsr	cloneml			two loops of mainlp
	bsr	cloneml
	move.l	score,hsscore
	move.l	#0,score		reset score
	move.l	#0,plus
	bsr	dosc1
	move.l	#3,lives		reset lives
	bsr	dolives
	move.l	#screens,actscr		reset level data addr. start
	move.l	#0,level		reset level
	move.l	#0,elscore
	move.l	#20000,elfreq		reset score per extra life
	bsr	setord			set up order graphics
	move.b	#1,sphero+110
	bsr	newshot			set pointer pos.
***	all resets here 	***
	move.w	#$2ae,clistc+38		set hiscore colours
	move.w	#$fe6,clistc+42
	move.w	#$e86,clistc+46
	bsr	printit			hiscore routine
	move.b	#0,mode			title screen set up
	move.b	#2,xmode
	bra	wait
tsetup	bsr	delay2			SET UP TITLE SCREEN
	btst	#7,ciaapra
	beq.s	tsetup			do not exit until fire released
	bsr	tpoint			set up title copper list
	move.w	#150,iscount		set up swap delay (3 seconds+delay2)
	move.b	#0,mode			
	move.b	#4,xmode
	bra	wait
fwait	btst	#6,ciaapra		WAIT FOR BUTTON PRESS
	beq	goout			left mouse. exit
	btst	#7,ciaapra
	beq.s	fwait3			fire. game start
	subq.w	#1,iscount		change display?
	bne	wait			no
	tst.w	isonoff			which screen is on?
	beq.s	fwait2
fwait1	move.w	#0,isonoff		change to title screen
	move.w	#250,iscount		yes. update time
	bsr	tpoint
	bra	wait
fwait2	move.w	#1,isonoff		change to info screen
	move.w	#500,iscount		yes. update time
	bsr	ipoint
	bra	wait	
fwait3	move.w	joy2(a5),d6		any cheats?
	btst	#1,d6			<right joystick>
	beq	fwait4
	move.l	#10000,elfreq		yes. less score per extra
fwait4	btst	#9,d6			<left joystick>
	beq	fwait5
	move.l	#screens+72,actscr	yes. start on level 10
	move.l	#9,level
fwait5	move.b	#1,mode			game screen set up
	move.b	#0,xmode
	bra	wait

goout	move.l	old,$6c			restore old interrupt adrr.
	move.l	oldcop,cop1lc(a5)	RESET COPPER.EXIT
	clr.w	copjmp1(a5)
	move.w	#$002f,dmacon(a5)	all sound/sprites off
	move.w	#$83f0,dmacon(a5)	bits 4-9 on
	move.l	Execbase,a6
	jsr	Permit(a6)
goodbye	clr.l	d0
	rts
*****************************************************************************
inter	movem.l	d0-d7/a0-a6,-(sp)	INTERRUPT CODE.save all
	and	#$10,intreqr(a5)	interrupt from copper?
	beq	notcop			no
	move.w	#$10,intreq(a5)		clear interrupt

	*** is this needed ***

notcop	movem.l	(sp)+,d0-d7/a0-a6	restore all
	dc.w	$4ef9
old	dc.l	0			jump to interrupt code
*****************************************************************************
cloneml	bsr	blank			SIMULATE ONE LOOP OF MAINLP
	bsr	alter
	bsr	put
	bsr	disp
	bsr	switch
	rts
*****************************************************************************
nextlvl	move.l	actscr,a1		ADVANCE ONE LEVEL
	add.l	#8,a1
	tst.l	(a1)
	bne.s	nextl1
	lea	screens,a1
nextl1	move.l	a1,actscr
	move.b	#1,mode			set bit 0
	move.w	#0,windup
	move.w	#0,deadt
	rts
*****************************************************************************
copylvl	lea	minimap,a0		LEVEL DATA TO STRUCTS.
	move.l	actscr,a1
	move.l	(a1),a2
	move.l	4(a1),d0
	subq.l	#1,d0
copyl1	move.b	(a2)+,(a0)+
	dbra	d0,copyl1
	rts
*****************************************************************************
testend	tst.w	windup			TEST FOR END SEQUENCE
	beq.s	teste9
	subq.w	#1,windup
	bne.s	teste9
	bsr	killall
	bsr	put			simulate one loop of mainlp
	bsr	disp
	bsr	switch
	bsr	blank
	bsr	alter
	bsr	remhw
	bsr	wipesc
	move.l	#endlvlm,minimap
	bsr	stsc			display extra points screen
	add.l	#4000,plus
	move.w	#200,exsct
	move.b	#16,mode		set bit 4
teste9	rts
*****************************************************************************
cake	add.l	#1,lives		CAKE BOUNS
	bsr	dolives
	SOUND	3,#extram,#3688/2,#40,#352,#$8208,#18
	rts
range	move.b	#50,sphero+113		GLASS BONUS
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
	rts
stoptim	move.w	#400,deadt		CLOCK BONUS
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
	rts
bank	add.l	#1000,plus		COIN BONUS
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
	rts
flower	move.b	#32,mode		FLOWER BONUS
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
	rts				set bit 5
timer	add.l	#1000,number		TIMER BONUS
	cmpi.l	#10000,number
	bmi.s	timer1
	move.l	#9999,number
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
timer1	rts
alloff	lea	slist+16,a2		BOMB BONUS
all1	move.l	(a2)+,a4
	cmpa.l	#shot,a4
	beq.s	allend
	btst	#0,89(A4)		on?
	beq.s	all1			no
	btst	#6,110(a4)		dying already?
	bne.s	all1			yes
	bsr	smartb
	bra.s	all1
allend	rts
smartb	move.l	#$00030003,76(a4)	[remove aliens]
	move.l	#$00060006,84(a4)
	move.b	110(a4),d5
	bsr	offset
	lea	anfston,a0
	add.l	d6,a0
	move.l	a0,72(a4)		graphic start addr.
	add.l	#16,a0
	move.l	a0,80(a4)		mask start addr.
	ori.b	#$60,110(a4)		set special/dead bits
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
	rts
lswitch	bset	#6,sphero+89		SWITCH BONUS
	SOUND	2,#bonusm,#16236/2,#40,#352,#$8204,#66
	rts
fireoff	lea	slist+16,a2		TAP BONUS
fir1	move.l	(a2)+,a4
	cmpa.l	#shot,a4
	beq.s	firend
	btst	#0,89(A4)		on?
	beq.s	fir1			no
	btst	#6,110(a4)		dying already?
	bne.s	fir1			yes
	btst	#3,110(a4)		fire?
	beq.s	fir1			no
	bsr	smartb
	bra.s	fir1
firend	rts
*****************************************************************************
whichm	bsr	random			SELECT MAZE AT RANDOM
	moveq	#0,d0			clear
	move.w	rand,d0
	andi.w	#$00f0,d0		filter out (0-15)*16
	lea	mlist,a0
	add.l	d0,a0
	move.l	(a0),minimap		set map
	move.w	6(a0),bdhero+60		set x pos.
	move.l	8(a0),bdhero+64		set y pos.
	move.l	12(a0),number		reset count
	bsr	dcount
	rts
*****************************************************************************
waddle	lea	bdhero,a4		MOVE HERO
	btst	#0,89(A4)		on?
	beq	waddle9			no
	move.w	joy2(a5),d6
	btst	#1,d6			right?
	beq	waddle1			no
	lea	baldmr,a1
	bsr	hitbit
	beq.s	waddle1
	addq.w	#1,60(a4)		right 1 line
	move.l	#anrbald,72(a4)		set anim. start
	move.l	#anrbald+8,80(a4)	set mask start
	bsr	movera
waddle1	btst	#9,d6			left?
	beq	waddle2			no
	lea	baldml,a1
	bsr	hitbit
	beq.s	waddle2
	subq.w	#1,60(a4)		left 1 line
	move.l	#anlbald,72(a4)		set anim. start
	move.l	#anlbald+8,80(a4)	set mask start
	bsr	movera
waddle2	move.w	d6,d7			xor for up down test
	lsr.w	#1,d7
	eor.w	d7,d6
	btst	#8,d6			up?
	beq	waddle3			no
	lea	baldmu,a1
	bsr	hitbit
	beq.s	waddle3
	sub.l	#40,64(a4)		up 1 line
	move.l	#anubald,72(a4)		set anim. start
	move.l	#anubald+8,80(a4)	set mask start
	bsr	movera
waddle3	btst	#0,d6			down?
	beq	waddle5			no
	lea	baldmd,a1
	bsr	hitbit
	beq.s	waddle5
	add.l	#40,64(a4)		down 1 line
	cmpi.l	#8800,64(a4)
	bmi.s	waddle4
	bclr	#0,89(a4)		remove baldy
	move.b	#1,112(a4)
	bsr	wipesc			display extra points screen
	move.l	#endmazm,minimap
	bsr	stsc
	add.l	#8000,plus
	move.w	#200,exsct
	move.b	#16,mode		set bit 4
	rts
waddle4	move.l	#andbald,72(a4)		set anim. start
	move.l	#andbald+8,80(a4)	set mask start
	bsr	movera
waddle5	bsr	calc
	bsr	anim
waddle9	rts
*****************************************************************************
extras	bsr	random			DISPLAY BONUS GRAPHICS
	cmpi.w	#$006f,rand
	bcc	extra9
	bsr	random
	lea	bonpos,a0
	moveq	#0,d0
	move.w	rand,d0
	lsr.w	#8,d0
	andi.w	#$000f,d0
	lsr.w	#2,d0
	lsl.w	#4,d0
	add.l	d0,a0			struct.,on/off,x,y
	bset	#0,7(a0)		test and set
	bne.s	extra9			already on
	move.l	(a0),a4			struct.
	move.b	d0,115(a4)		offset to data
	move.w	10(a0),60(a4)
	move.l	12(a0),64(a4)
	moveq	#0,d0
	lea	bonrand,a1
	move.w	rand+2,d2
extra1	cmp.w	(a1)+,d2
	bcs.s	extra2
	add.w	#2,d0
	bra.s	extra1
extra2	lea	bonus,a1		graphics start
	add.l	d0,a1			graphics addr.
	move.l	a1,(a4)
	add.l	#1200,a1		mask addr.
	move.l	a1,4(a4)
	lsl.w	#1,d0			* 2 = 0-36
	lea	bonaddr,a3
	move.l	(a3,d0.w),116(a4)	routine addr.
	bset	#0,89(a4)		set struct. on bit
	move.w	#500,120(a4)		set remove timer
	bsr	calc
extra9	rts
*****************************************************************************
chute	btst	#0,chhero+89		MOVE PARACHUTE
	beq.s	chute2
	bsr	blank
	bsr	alter
	lea	chhero,a4
	move.l	stpos+4,d0		start y pos.
	sub.l	#920,d0			- parachute height
	add.l	#40,64(a4)		move parachute
	cmp.l	64(a4),d0		at start yet?
	bne.s	chute3			no
	bclr	#0,89(a4)		turn off parachute
	move.b	#1,112(a4)		set remove byte
	bra.s	chute1
chute3	bsr	calc
	bsr	anim
	bsr	movera
chute1	bsr	put
	bsr	disp			displays flipin screens
	bsr	switch			switches screens in flipin/out
*	move.w	#$444,color00(a5)	*** test only ***
	rts
chute2	tst.b	chhero+112		removed?
	bpl.s	chute4			yes
	move.b	#2,mode			set bit 1
	bset	#0,sphero+89		hero on
	rts
chute4	bsr	blank
	bsr	alter
	bra.s	chute1
	rts
chuteon	lea	chhero,a4		SETUP PARACHUTE VALUES
	move.l	stpos,36(a4)		screen offset
	move.l	stpos,40(a4)
	move.l	#0,64(a4)		y pos. =0
	move.w	stpos+10,60(a4)		x pos.
	sub.w	#16,60(a4)		16 pixels left for parachute width
	bset	#0,89(a4)
	rts
*****************************************************************************
stsc	move.l	minimap,a3		DRAW BACKGROUND SCREEN ROUTINE
	add.l	#(top/8)*6,a3
	lea	bkb1,a4
	moveq	#0,d2			screen offset
	moveq	#25-1,d5		blocks high
stsc2	move.l	(a3)+,d3		get first line in bit map
	move.w	(a3)+,d4
	moveq	#19-1,d6		blocks wide/2
	move.l	#bkbmsks,4(a4)		mask with depth
stsc1	bsr	stscbl			small blocks
	bsr	stscbr
	add.w	#2,d2
	dbra	d6,stsc1
	bsr	stscbl			right border
	move.l	#bkbmsk,4(a4)		mask without depth
	bsr	stscbr
	add.w	#2,d2
	add.w	#7*40,d2
	dbra	d5,stsc2
	rts
stscbl	move.w	#$0000,54(a4)		BLIT SMALL BLOCK LEFT
	lsl.w	#1,d4
	roxl.l	#1,d3			get bit
	bcc.s	stscbl1			block?
	bsr	setscb			blit left side block
stscbl1	rts
stscbr	move.w	#$8000,54(a4)		BLIT SMALL BLOCK RIGHT
	lsl.w	#1,d4			get bit
	roxl.l	#1,d3
	bcc.s	stscbr2			block?
	bsr	setscb			blit right side block
stscbr2	rts
setscb	move.l	56(a4),d7		BLIT BACKGROUND BLOCKS
	lea	scr4,a2			screen addr.
	add.l	d2,a2			screen pos.
	move.l	(a4),a1			data
	move.l	4(a4),a0		mask
setscb1	move.w	54(a4),d0		shift bits
	bsr	pblit2
	add.l	44(a4),a1		sprite plane size
	add.l	#bscsize,a2		screen size
	dbra	d7,setscb1
	rts
*****************************************************************************
grow	move.l	minimap,a3		DRAW DATA TO SCR6
	lea	blk,a4
	moveq	#0,d2			screen offset
	moveq	#34-1,d5		blocks high
gr2	move.l	(a3)+,d3		get first line in bit map
	move.w	(a3)+,d4
	moveq	#20-1,d6		blocks wide/2
gr1	bsr	grctl			small blocks
	bsr	grctr
	add.w	#2,d2
	dbra	d6,gr1
	add.w	#7*40,d2
	dbra	d5,gr2
	rts
grctl	lsl.w	#1,d4			BLIT SMALL BLOCK LEFT
	roxl.l	#1,d3			get bit
	bcc.s	grctl1			block?
	bsr	expl
grctl1	rts
grctr	lsl.w	#1,d4			BLIT SMALL BLOCK RIGHT
	roxl.l	#1,d3			get bit
	bcc.s	grctr1			block?
	bsr	expr
grctr1	rts
expl	move.w	#$0000,d0		LEFT BLOCK
	bra.s	expand
expr	move.w	#$8000,d0		RIGHT BLOCK
expand	lea	scr6+width*8,a2		BLIT DATA TO SCR6
	add.l	d2,a2
	move.l	(a4),a1			data
	move.l	4(a4),a0		mask
	bsr	pblit2
	rts
*****************************************************************************
printit	bsr	sorter			HISCORE ROUTINE
	tst.l	d7
	bmi.s	prt1
	bsr	showtab
	bsr	enter
	rts
prt1	rts
sorter	move.l	#10-1,d7		SORT HISCORE TABLE
	move.l	hsscore,d0		2147483647=max.number($efffffff)[2^31]
	lea	table,a6		start of table
sort1	move.l	(a6),d1
	cmp.l	d0,d1			new hiscore?
	bmi.s	sort2			yes
	add.l	#16,a6			no. next score in table
	dbra	d7,sort1
	move.l	#-1,d7
	rts
sort2	move.l	d7,tabline		lines to move -1
	lsl.l	#4,d7			*16
	add.l	#15,d7			+15=bytes to move -1 for dbra
sort3	move.b	(a6,d7.w),16(a6,d7.w)	a6=start of data to move
	dbra	d7,sort3		d6=offset (from end to stop overlap)
	move.l	d0,(a6)+		store new score in table
	move.l	a6,taboff		store new data addr. for later
	move.l	#$20202020,(a6)+	store 10 spaces
	move.l	#$20202020,(a6)+
	move.l	#$20200000,(a6)		and 2 zeros
	move.l	#1,d7
	rts
showtab	move.l	#10-1,d6		DISPLAY HISCORES
	move.l	#hsstpos,a3
	lea 	table,a6
shtab1	move.l	a3,-(sp)
	move.l	(a6)+,d0		get score in hex
	lea	strspc,a0
	lea	divider+16,a1
	bsr	hexdec			get decimal ascii
	lea	strspc,a0
	bsr	parse
	lea	gap,a0
	bsr	parse
	move.l	a6,a0			print name
	bsr	parse
	move.l	(sp)+,a3
	add.l	#width*17,a3
	add.l	#12,a6
	dbra	d6,shtab1
	rts
enter	move.l	tabline,a3		INPUT NEW NAME
	move.l	#10-1,d6
	sub.l	a3,d6			invert d7. result in d6
	move.l	#(6+2)*2,a3		width of score+gap in bytes
ent1	add.l	#width*17,a3		+ 1 line
	dbra	d6,ent1
	sub.l	#width*17,a3		- 1 line for correct pos.
	add.l	#hsstpos,a3		+ table start=new char. screen addr.
	move.b	#64,onechar		displayed "?" (back)
	move.l	taboff,a6		addr. in table for first new char.
	move.w	#10-1,d6		max. char. in entry
ent2	bsr	spinner
	rts
spinner	lea	onechar,a0
	bsr	parse
	subq.l	#2,a3			set to correct pos.
	bsr	delay2			slow to useable speed
	btst	#7,ciaapra		fire pressed?
	beq.s	nocyc			yes
	move.w	joy2(a5),d5
	btst	#1,d5
	beq	nospr
	addq.b	#1,onechar
	cmpi.b	#96,onechar
	bne.s	nospr
	move.b	#32,onechar
nospr	btst	#9,d5
	beq	nospl
	subq.b	#1,onechar
	cmpi.b	#31,onechar
	bne.s	nospl
	move.b	#95,onechar
nospl	bra.s	spinner
nocyc	bsr	delay2
	cmpi.b	#64,onechar		back space?
	bne.s	nobspc			no
	cmpi.w	#9,d6
	beq.s	spinner
	lea	onechar+2,a0		remove "?" (back) char.
	bsr	parse
	addq.w	#1,d6
	subq.l	#4,a3			set to correct pos.
	subq.l	#1,a6
	bra	spinner
nobspc	cmpi.b	#63,onechar		end?
	beq.s	noent			yes
	subq.w	#1,d6			all data entered?
	bmi.s	fullup			yes
	addq.l	#2,a3
	move.b	onechar,(a6)+
	move.b	#64,onechar		display "?" (back)
	bra	spinner
fullup	move.b	onechar,(a6)+		keep last shown char. when full up
noent	rts
*****************************************************************************
parse	moveq	#0,d0			PARSE AND PRINT ASCII STRING
	move.b	(a0)+,d0		a3=screen addr.
	beq.s	endstr			a0=string addr.
	cmpi.b	#31,d0
	bls.s	endstr			print space(32) to _(95)
	cmpi.b	#95,d0
	bhi.s	endstr
	subi.w	#32,d0			make from zero
	move.l	d0,d1			copy
	lsr.w	#3,d1			/ 8. characters wide in grid
	move.l	d1,d2			copy for y value
	lsl.w	#3,d1			* 8. removes remainder
	sub.w	d1,d0			=x value in words
	lsl.w	#1,d0			*2 =x value in bytes
	lsl.w	#8,d2			y * 256=y addr. offset
	add.w	d0,d2			+ x value=total adr. offset
	move.l	a3,a2
	bsr	lhsblit
	addq.l	#2,a3			text width=2 bytes
	bra.s	parse
endstr	rts
lhsblit	move.l	#2-1,d7			PUT TEXT BLITTER ROUTINE
	lea	text,a4
	add.l	d2,a4
lhsblt1	bsr	hsblit
	add.l	#bscsize,a2		screen size
	add.l	#2048,a4		sprite plane size
	dbra	d7,lhsblt1
	rts
hsblit	btst	#14,dmaconr(a5)		PUT TEXT BLIT
	bne.s	hsblit
	move.l	a4,bltapth(a5)
	move.l	a2,bltdpth(a5)
	move.l	#$ffffffff,bltafwm(a5)
	move.l	#$000e0026,bltamod(a5)
	clr.w	bltcon1(a5)
	move.w	#$09f0,bltcon0(a5)
	move.w	#$0401,bltsize(a5)	16*16
	rts
*****************************************************************************
mover	lea	sphero,a4		MOVE HERO
	btst	#4,89(a4)		dying?
	bne	fatal			yes
	btst	#0,89(A4)		on?
	beq	noton			no
	lea	heromd,a1		no.try to fall
	bsr	falling
notdn1	move.w	joy2(a5),d6
	btst	#1,d6
	beq	notrt
	lea	heromr,a1
	bsr	hitbit
	beq.s	animrt
	addq.w	#1,60(a4)		right 1 line
animrt	move.l	96(a4),72(a4)		set anim. start
	move.l	104(a4),80(a4)		set mask start
	move.w	#1,32(a4)		set right
	bsr	movera
notrt	btst	#9,d6
	beq	notlt
	lea	heroml,a1
	bsr	hitbit
	beq.s	animlt
	subq.w	#1,60(a4)		left 1 line
animlt	move.l	92(a4),72(a4)		set anim. start
	move.l	100(a4),80(a4)		set mask start
	move.w	#-1,32(a4)		set left
	bsr	movera
notlt	move.w	d6,d7			xor for up down test
	lsr.w	#1,d7
	eor.w	d7,d6
	btst	#1,88(a4)		jumping?
	bne.s	jump			yes
	btst	#8,d6			try to jump?
	beq	notup			no
	btst	#0,88(a4)		falling?
	bne.s	notup			yes.cannot jump
	bset	#1,88(a4)		set jump bit
jump	sub.l	#80,64(a4)		up 1 line
	subq.b	#1,91(a4)
	bne.s	notup
	move.b	90(a4),91(a4)
	bclr	#1,88(a4)		clear jumping bit
	bset	#2,88(a4)
notup	bsr	calc
	bsr	anim
noton	rts
fatal	bsr	calc
	bsr	anim
	subq.w	#1,86(a4)
	bpl	fatal9			delay
	move.w	84(a4),86(a4)		reset delay
	subq.w	#1,78(a4)		alter count
	bpl	fatal9
	subq.l	#1,lives
	bne.s	fatal0
	move.b	#1,xmode		all dead here.end game/hiscore routine
fatal0	bsr	dolives
	move.w	#$00000029,88(a4)	hero reborn
	move.l	#192,44(a4)		update single plane size
	move.l	#$00010001,76(a4)	anim. frame count
	move.l	#$00030003,84(a4)	delay count
	move.l	#$00040024,20(a4)	set modulos
	move.l	#$00240004,24(a4)
	move.b	90(a4),91(a4)		reset jump count
	move.l	stpos,36(a4)		screen offset
	move.l	stpos+4,64(a4)		y pos.
	move.w	stpos+10,60(a4)		x pos.
	move.w	stpos+6,32(a4)		facing direction
	bmi.s	fatal1
	move.l	#anrhero,72(a4)
	move.l	#anrhero+8,80(a4)
	bra.s	fatal9
fatal1	move.l	#anlhero,72(a4)
	move.l	#anlhero+8,80(a4)
fatal9	rts
*****************************************************************************
control	lea	slist+16,a6		MOVE MULTIPLE SPRITES
cont1	move.l	(a6)+,a4
	cmpa.l	#shot,a4		end of list?
	beq.s	contend			yes
	btst	#0,89(A4)		on?
	beq.s	cont1			no
	btst	#5,110(a4)		special?
	beq.s	cont2			no
	bsr	dora			dead or animate routine
	bra.s	cont1
cont2	btst	#4,110(a4)
	beq.s	cont3
	bsr	bouncer			bounce routine
	bra.s	cont1
cont3	bsr	romper			walk/jump routine
	bra.s	cont1
contend	rts
*****************************************************************************
romper	lea	alienmd,a1		WALK ALIENS
	bsr	falling
	beq.s	rompe
rompf4	btst	#1,89(A4)		allowed to jump?
	beq.s	rompm2			no
	btst	#1,88(a4)		already jumping?
	bne.s	rompm1			yes
	bsr	random			new random number
	cmpi.w	#$00ff,rand		try to jump
	bcc.s	rompm2			no jump
rompm1	bsr	rompu
	bra.s	rompe
rompm2	move.w	62(a4),d2		get x vel.
	subq.b	#1,109(a4)		left right delay
	bpl.s	rompe
	move.b	108(a4),109(a4)		reset delay
	btst	#2,89(a4)		change direction?
	beq.s	rompm3			no
	bsr	random			new random number
	cmpi.w	#$02ff,rand		change direction?
	bcc.s	rompm3			no
	bsr	rompch			yes
rompm3	tst.w	32(a4)
	bmi.s	rompm4
	bsr	rompr			try to go right
	bra.s	rompe
rompm4	bsr	rompl			try to go left
rompe	bsr	calc
	bsr	anim
	bsr	movera
	rts
rompr	lea	alienmr,a1		MOVE ALIEN RIGHT
	bsr	hitbit
	beq	rompch
	add.w	d2,60(a4)		right 1 line
	move.l	96(a4),72(a4)
	move.l	104(a4),80(a4)
	rts
rompl	lea	alienml,a1		MOVE ALIEN LEFT
	bsr	hitbit
	beq.s	rompch
	sub.w	d2,60(a4)		left 1 line
	move.l	92(a4),72(a4)
	move.l	100(a4),80(a4)
	rts
rompu	btst	#0,88(a4)		TRY TO JUMP.	falling?
	bne.s	rompu2			yes.cannot jump
	bset	#1,88(a4)		set jump bit
rompu1	sub.l	#80,64(a4)		up 1 line
	subq.b	#1,91(a4)
	beq.s	rompu2
	rts
rompu2	move.b	90(a4),91(a4)
	bclr	#1,88(a4)		clear jumping bit
	bset	#2,88(a4)		set end of jump bit
rompch	neg.w	32(a4)			CHANGE DIRECTION
	rts
*****************************************************************************
bouncer	move.w	62(a4),d0		BOUNCE ALIENS
	add.w	d0,60(a4)		add x,y velocity
	move.l	68(a4),d0
	add.l	d0,64(a4)
	cmpi.l	#400,64(a4)		hit top?
	bpl.s	bounce1			no
	neg.l	68(a4)			yes.make bounce down
bounce1	cmpi.l	#9600,64(a4)		down hole?
	bmi.s	bounce2			no
	bsr	hole
	sub.l	#9200,64(a4)		yes.make top of screen
bounce2	tst.l	68(a4)
	bmi.s	bounce3
	move.l	#840,d0			get bottom test position
	bra.s	bounce4
bounce3	move.l	#-40,d0			get top test pos.
bounce4	move.l	36(a4),a2		screen offset
	add.l	d0,a2			+test pos.
	add.l	#scr6+320,a2		+screen start=actual test pos.
	move.l	#$1ffe0000,d1		top/bottom mask
	move.w	52(a4),d2		get shift bits
	lsl.w	#1,d2
	roxl.w	#4,d2			move to lsb's
	lsr.l	d2,d1			shift mask
	and.l	(a2),d1			hit platform?
	beq.s	bounce5			no
	neg.l	68(a4)			yes.bounce
bounce5	tst.w	62(a4)
	bmi.s	bounce6
	lea	alienbr,a1		right mask
	bra.s	bounce7
bounce6	lea	alienbl,a1		left mask
bounce7	bsr	hitbit			hit side?
	bne.s	bounce9			no
	neg.w	62(a4)			yes.bounce
	bmi.s	bounce8
	move.l	96(a4),72(a4)		rotate right
	move.l	104(a4),80(a4)
	bra.s	bounce9
bounce8	move.l	92(a4),72(a4)		rotate left
	move.l	100(a4),80(a4)
bounce9	bsr	calc
	bsr	anim
	bsr	movera
	rts
*****************************************************************************
dora	btst	#1,88(a4)		SPECIAL BIT SET ROUTINE
	beq.s	dora1			not jumping
	sub.l	#80,64(a4)		up 2 lines
	subq.b	#1,91(a4)
	bne.s	dora2
	move.b	90(a4),91(a4)
	bclr	#1,88(a4)		clear jumping bit
	bset	#2,88(a4)		set end of jump bit
dora1	lea	alienmd,a1
	bsr	falling
dora2	bsr	calc
	bsr	anim
	subq.w	#1,86(a4)
	bpl	dora8			delay
	move.w	84(a4),86(a4)		reset delay
	subq.w	#1,78(a4)		alter count
	bpl	dora8
	move.w	76(a4),78(a4)		reset frame count
	btst	#6,110(a4)		dead?
	bne	dora7			yes
	bclr	#7,110(a4)		anim.? else clear bit
	beq.s	dora5			no.	[set up appear graphics]
	move.b	110(a4),d5		new type
	bsr	offset
	lea	angston,a0
	add.l	d6,a0
	move.l	a0,72(a4)		graphic start addr.
	add.l	#16,a0
	move.l	a0,80(a4)		mask start addr.
	move.b	110(a4),d5		get offset for struct. reset
	moveq	#0,d6
	bsr	mutate			set up new alien struct.
	rts
dora5	bclr	#5,110(a4)		[set new type graphics]
	tst.w	32(a4)			left/right?
	bmi.s	dora6
	move.l	96(a4),72(a4)		set right graphics
	move.l	104(a4),80(a4)
	rts
dora6	move.l	92(a4),72(a4)		set left graphics
	move.l	100(a4),80(a4)
	rts
dora7	bclr	#0,89(a4)		[dead]
	move.b	#1,112(a4)		remove count
	subq.w	#1,taliens		one less alien
	bne.s	dora8
	move.w	#100,windup		end of level
	move.w	#250,deadt		stop counter
dora8	rts
*****************************************************************************
detect	lea	slist+16,a6		COLLISION WITH HERO
	lea	sphero,a3
	btst	#4,89(a3)		dying?
	bne.s	detend			yes
	btst	#5,89(a3)		reborn?
	bne.s	detend			yes
	move.w	60(a3),d0		get hero x,y pos.
	move.w	d0,d1
	move.l	64(a3),d2
	move.l	d2,d3
	sub.w	#15,d0			limits for hitting alien
	add.w	#15,d1
	sub.l	#19*40,d2
	add.l	#23*40,d3
det1	move.l	(a6)+,a4
	cmpa.l	#shot,a4
	beq.s	detend
	btst	#0,89(A4)		on?
	beq.s	det1			no
	bsr	collide
	bra.s	det1
detend	rts
collide	cmp.w	60(a4),d1		test x pos.
	bmi	coll1
	cmp.w	60(a4),d0
	bpl	coll1
	cmp.l	64(a4),d3		test y pos.
	bmi.s	coll1
	cmp.l	64(a4),d2
	bpl.s	coll1
death	bset	#4,89(a3)		hero already hit? else set hit bit
	bne.s	coll1			yes
	move.l	#384,44(a3)		update single plane size
	move.l	#dielist,72(a3)		anim. list start
	move.l	#$00070007,76(a3)	anim. frame count
	move.l	#dielist+32,80(a3)	mask list start
	move.l	#$00060006,84(a3)	delay count
	move.l	#$000c0024,20(a3)	set modulos
	move.l	#$0024000c,24(a3)
	move.w	#150,deadt
	SOUND	3,#deadm,#3730/2,#40,#352,#$8208,#18
coll1	rts
strike	lea	slist+16,a6		COLLISION WITH MISSILE
	lea	shot,a3
	btst	#0,89(a3)		missile on?
	beq.s	strend			no
	move.w	60(a3),d0		get missile x,y pos.
	move.w	d0,d1
	move.l	64(a3),d2
	move.l	d2,d3
	sub.w	#15,d0			limits for hitting alien
	add.w	#15,d1
	sub.l	#19*40,d2
	add.l	#6*40,d3
str1	move.l	(a6)+,a4
	cmpa.l	#shot,a4
	beq.s	strend
	btst	#0,89(A4)		on?
	beq.s	str1			no
	btst	#5,110(a4)		special (anim./dying)?
	bne.s	str1			yes
	bsr	gotit
	bra.s	str1
strend	rts
gotit	cmp.w	60(a4),d1		test x pos.
	bmi	got4
	cmp.w	60(a4),d0
	bpl	got4
	cmp.l	64(a4),d3		test y pos.
	bmi	got4
	cmp.l	64(a4),d2
	bpl	got4
	btst	#6,sphero+89		long fire bit set?
	bne.s	got1			yes
	bclr	#0,shot+89		no. remove missile
	move.b	#1,shot+112
got1	move.l	#$00030003,76(a4)	frame count/reset
	move.l	#$00060006,84(a4)	delay count/reset
	move.b	110(a4),d5
	bsr	offset
	lea	anfston,a0
	add.l	d6,a0
	move.l	a0,72(a4)		graphic start addr.
	add.l	#16,a0
	move.l	a0,80(a4)		mask start addr.
	move.b	110(a4),d5
	cmp.b	sphero+110,d5		killed or mutated?
	beq.s	got3			killed
got2	bsr	pebble			get next in sequence
	and.b	sphero+110,d4		same as bullet?
	bne.s	got2			yes
	move.b	d5,110(a4)		save new type
	ori.b	#$a0,110(a4)		set special/anim. bits
	add.l	#10,plus
	SOUND	1,#changem,#8060/2,#40,#352,#$8202,#36
	rts
got3	ori.b	#$60,110(a4)		set special/dead bits
	add.l	#100,plus
	SOUND	1,#hitm,#1196/2,#40,#352,#$8202,#6
got4	rts
collect	lea	slist,a6		HERO HIT BONUS
	lea	sphero,a3
	btst	#4,89(a3)		dying?
	bne.s	coltend			yes
	move.w	60(a3),d0		get hero x,y pos.
	move.w	d0,d1
	move.l	64(a3),d2
	move.l	d2,d3
	sub.w	#15,d0			limits for hitting bonus
	add.w	#15,d1
	sub.l	#19*40,d2
	add.l	#23*40,d3
colt1	move.l	(a6)+,a4
	cmpa.l	#alien0,a4
	beq.s	coltend
	btst	#0,89(A4)		on?
	beq.s	colt1			no
	sub.w	#1,120(a4)
	bpl.s	colt2			end of timer?
	bsr	pickup			yes. remove
	bra.s	colt1
colt2	bsr	bounty
	bra.s	colt1
coltend	rts
bounty	cmp.w	60(a4),d1		test x pos.
	bmi.s	pick1
	cmp.w	60(a4),d0
	bpl.s	pick1
	cmp.l	64(a4),d3		test y pos.
	bmi.s	pick1
	cmp.l	64(a4),d2
	bpl.s	pick1
	bsr	pickup
	movem.l	d0-d3/a6,-(sp)
	move.l	116(a4),a2
	jsr	(a2)
	movem.l	(sp)+,d0-d3/a6
	rts
pickup	bclr	#0,89(a4)		remove hit bonus
	move.b	#1,112(a4)
	moveq	#0,d4
	lea	bonpos,a0		bonus pos. data
	move.b	115(a4),d4		offset to this subset
	bclr	#0,7(a0,d4.w)		clear set bit
pick1	rts
*****************************************************************************
hitbit	move.l	36(a4),a2		DETECT HIT PLATFORM BLITTER ROUTINE
	move.w	52(a4),d0
	subi.w	#$1000,d0
	bcc.s	hitbit1
	subq.l	#2,a2
hitbit1	add.l	#scr6+320,a2		screen start+1 line
	bsr	gtblit
	move.l	a1,bltapth(a5)		position mask
	move.l	a2,bltcpth(a5)		scr6 data
	move.l	#$ffff0000,bltafwm(a5)
	move.w	#$ffff,bltamod(a5)	A=-1
	move.w	#$0022,bltcmod(a5)	C=34
	ori.w	#$0aa0,d0
	move.w	d0,bltcon0(a5)		copy plus shift for mask.no output
	clr.w	bltcon1(a5)
	move.w	#$0603,bltsize(a5)	24*3
gtblit	btst	#14,dmaconr(a5)
	bne.s	gtblit
	btst	#13,dmaconr(a5)
	rts
calc	move.l	64(a4),d1		CALCULATE SCREEN OFFSET/SHIFT BITS
	move.w	60(a4),d0
	move.w	d0,d2			copy
	andi.w	#$000f,d0		shift bits
	lsr.w	#1,d0
	roxr.w	#4,d0			move to msb's
	move.w	d0,52(a4)		save in struct.
	andi.w	#$fff0,d2		x pos. - shift bits
	lsr.w	#3,d2			words * 2 in x axis
	add.w	d2,d1			offset in bytes
	move.l	d1,36(a4)		save in struct.
	rts
anim	move.l	72(a4),a3		UPDATE ANIMATION ADDR.
	move.l	80(a4),a2
	moveq	#0,d0
	move.w	78(a4),d0
	lsl.w	#2,d0			frame*4
	add.l	d0,a3			add to anim. start addr.
	move.l	(a3),(a4)		move data addr. to struct.
	add.l	d0,a2
	move.l	(a2),4(a4)
	rts
movera	subq.w	#1,86(a4)		INCREMENT ANIMATION FRAME
	bpl.s	movera1			delay
	move.w	84(a4),86(a4)		reset delay
	subq.w	#1,78(a4)		alter count
	bpl.s	movera1
	move.w	76(a4),78(a4)		reset count
movera1	rts
random	move.w	rand+2,d0		CREATE RANDOM NUMBER
	move.w	rand,d1
	roxr.w	#7,d0
	subx.w	d0,d1
	roxr.w	#6,d1
	move.w	d1,rand+2
	move.w	d0,rand
	rts
falling	btst	#1,88(a4)		GOING DOWN {expects mask in a1}
	bne.s	fall4			jumping
	bsr	hitbit
	beq.s	fall1
	bclr	#2,88(a4)		clear end of jump bit
	bra.s	fall2
fall1	btst	#2,88(a4)		end of jump bit?
	beq.s	fall4			no.landed
fall2	add.l	#40,64(a4)		down 1 line
	cmpi.l	#9600,64(a4)		width*(top+high)
	bmi.s	fall3
	bsr	hole
	sub.l	#9200,64(a4)
fall3	bset	#0,88(a4)		set falling bit
	clr.b	d0			set z bit
	rts
fall4	bclr	#0,88(a4)		clear falling bit
	move.b	#1,d0			clear z bit
	rts
offset	moveq	#0,d6			GET OFFSET TO GRAPHICS
off1	lsr.b	#1,d5
	bcs.s	off2
	add.l	#64,d6
	bra.s	off1
off2	rts
pebble	lsl.b	#1,d5			NEXT TYPE IN SEQUENCE
	cmpi.b	#$20,d5			over top of range?
	bne.s	pebb2			no
	move.b	#1,d5			yes.reset to start
pebb2	move.b	d5,d4
	and.b	cycle,d4		within range of types for screen?
	beq.s	pebble			no
	rts
mutate	lsr.b	#1,d5			SET UP NEW ALIEN STRUCTURE
	bcs.s	mut1
	add.l	#28,d6
	bra.s	mutate
mut1	lea	newston,a0
	add.l	d6,a0
	move.l	(a0)+,92(a4)		reset struct.
	move.l	(a0)+,96(a4)
	move.l	(a0)+,100(a4)
	move.l	(a0)+,104(a4)
	move.l	(a0)+,84(a4)
	move.w	(a0)+,62(a4)
	move.b	(a0)+,89(a4)
	add.l	#1,a0
	move.w	(a0),108(a4)
	rts
killall	lea	slist,a6		REMOVE ALL SPRITES
kill1	move.l	(a6)+,a4
	cmpa.l	#bdhero,a4
	beq.s	killend
	btst	#0,89(A4)		on?
	beq.s	kill1			no
	bclr	#0,89(a4)		remove
	move.b	#1,112(a4)
	bra.s	kill1
killend	rts
wipesc	move.l	#scr4,a0		CLEAR BACKGROUND SCREEN
	move.w	#0,d1
	move.w	#$3214,d2
	bsr	clear
	move.l	#scr5,a0
	move.w	#0,d1
	move.w	#$3214,d2
	bsr	clear
	rts
wipems	move.l	#scr6,a0		CLEAR MASK SCREEN
	move.w	#0,d1
	move.w	#$4614,d2
	bsr	clear
	rts
clear	btst	#14,dmaconr(a5)		CLEAR MEMORY
	bne.s	clear
	move.w	#0,bltddat(a5)		output zero's only
	move.l	a0,bltdpth(a5)		screen
	move.w	d1,bltdmod(a5)
	move.w	#$0100,bltcon0(a5)	straight copy
	clr.w	bltcon1(a5)
	move.w	d2,bltsize(a5)
	rts
gpoint	move.l	#scr4,d0		SET UP GAME COPPER LIST POINTERS
	COPPER	d0,clistn+14,clistn+10
	move.l	#scr5,d0
	COPPER	d0,clistn+30,clistn+26
	move.l	#scr0a,d0
	COPPER	d0,clist+6,clist+2
	move.l	#scr0b,d0
	COPPER	d0,clist+14,clist+10
	move.l	#scr0c,d0
	COPPER	d0,clist+22,clist+18
	lea	clists,a1
	bsr	spoint
	move.l	#clist,cop1lc(a5)	set new list and start
	clr.w	copjmp1(a5)
	rts
tpoint	move.l	#title,d0		SET UP TITLE COPPER LIST POINTERS
	COPPER	d0,tlist+6,tlist+2
	move.l	#title+10240,d0
	COPPER	d0,tlist+14,tlist+10
	move.l	#title+20480,d0
	COPPER	d0,tlist+22,tlist+18
	move.l	#tlist,cop1lc(a5)	set new list and start
	clr.w	copjmp1(a5)
	lea	tlists,a1
	bsr	spoint
	rts
spoint	lea	splist,a0		SET SPRITES IN COPPER LIST
	moveq	#8-1,d7			a1=copper list start
nextsp	move.l	(a0)+,d1
	COPPER	d1,6(a1),2(a1)
	add.l	#8,a1
	dbra	d7,nextsp
	rts
ipoint	move.l	#info,d0		SET UP INFOSCREEN COPPER LIST POINTERS
	COPPER	d0,tlist+6,tlist+2
	move.l	#info+10240,d0
	COPPER	d0,tlist+14,tlist+10
	move.l	#info+20480,d0
	COPPER	d0,tlist+22,tlist+18
	rts
setblks	move.l	blktype,bkb1		SET UP PLATFORM BLOCK TYPES AND COLOUR
	move.w	blkcol,clistc+38
	move.w	blkcol+2,clistc+42
	move.w	blkcol+4,clistc+46
	rts
soff	tst.w	scount0			TURN OFF SOUND CHANNELS
	beq.s	soff1			not on
	subi.w	#1,scount0		turn off yet?
	bne.s	soff1			no
	move.w	#$1,dmacon(a5)		yes
soff1	tst.w	scount1
	beq.s	soff2
	subi.w	#1,scount1
	bne.s	soff2
	move.w	#$2,dmacon(a5)
soff2	tst.w	scount2
	beq.s	soff3
	subi.w	#1,scount2
	bne.s	soff3
	move.w	#$4,dmacon(a5)
soff3	tst.w	scount3
	beq.s	soff4
	subi.w	#1,scount3
	bne.s	soff4
	move.w	#$8,dmacon(a5)
soff4	rts
delay	move.l	#$9,d0			DELAY
dlay1	move.l	#$1f,d1
dlay2	dbra	d1,dlay2
	dbra	d0,dlay1
	rts
delay2	move.l	#$16,d0			DELAY 2
dlay5	move.l	#$fff,d1
dlay6	dbra	d1,dlay6
	dbra	d0,dlay5
	rts
*****************************************************************************
hfire	btst	#7,ciaapra		FIRE MISSILE
	bne.s	hfire2			fire button pressed?
	lea	shot,a4			yes
	btst	#0,89(a4)
	bne.s	hfire2
	btst	#4,sphero+89		hero dying?
	bne.s	hfire2			yes
	SOUND	0,#firem,#2436/2,#40,#352,#$8201,#10
	bsr	fireit
hfire2	rts
fireit	lea	sphero,a3		INITIATE MISSILE
	bset	#0,89(a4)		set on
	move.b	113(a3),114(a4)		set range
	move.l	64(a3),64(a4)
	addi.l	#10*40,64(a4)		y pos. + 10 lines
	move.w	60(a3),60(a4)		x pos.
	move.b	110(a3),d0		get type for offset
	move.b	d0,110(a4)		set type
	moveq	#0,d1
fireit1	lsr.w	#1,d0			get offset to type graphic start
	bcs.s	fireit2
	addq.w	#2,d1
	bra.s	fireit1
fireit2	tst.w	32(a3)			left or right?
	bmi.s	fireit3
	lea	shotr,a2		right graphic start
	move.w	#3,62(a4)		x vel.
	bra.s	fireit4
fireit3	lea	shotl,a2		left graphic start
	move.w	#-3,62(a4)		x vel.
fireit4	add.l	d1,a2
	move.l	a2,(a4)			graphic start
	add.l	#210,a2
	move.l	a2,4(a4)		mask start
	bsr	calc
	add.l	#1,plus
	rts
missile	lea	shot,a4			MOVE MISSILE
	btst	#0,89(A4)		on?
	beq.s	miss2			no
	subq.b	#1,114(a4)
	bmi.s	miss1			end of range
	move.w	62(a4),d0
	add.w	d0,60(a4)		add x vel. to x pos.
	bsr	calc
	lea	shotmt,a1
	bsr	hitbit			hit platform?
	bne.s	miss2			no
miss1	bclr	#0,89(a4)		remove
	move.b	#1,112(a4)
miss2	rts
*****************************************************************************
hole	lea	xplist,a3		FALLEN DOWN HOLE
	move.w	60(a4),d0		get x pos.
	tst.w	(a3)
	bmi.s	hole3			*** bugged if exits here ***
hole1	cmp.w	2(a3),d0		must have fallen down a hole so only
	bmi.s	hole2			need to check top limit
	add.l	#24,a3
	bra.s	hole1
hole2	move.b	7(a3),d5		get type bit
	andi.b	#$e0,110(a4)		clear old type.preserve special flags
	or.b	d5,110(a4)		set new type
	bsr	pebble			get next in sequence
	btst	#3,89(a4)		hero?
	beq.s	hole4			no
	btst	#7,ciaapra		fire button pressed?
	bne.s	hole3			no
	move.b	d5,7(a3)		save new type
	bsr	disphw1			display new arrow
hole3	bsr	newshot			update pointer pos.
	rts
hole4	move.b	110(a4),d5
	moveq	#0,d6
	bsr	mutate			set new alien structure
	tst.w	32(a4)			left/right?
	bmi.s	hole5
	move.l	96(a4),72(a4)		set right graphics
	move.l	104(a4),80(a4)
	rts
hole5	move.l	92(a4),72(a4)		set left graphics
	move.l	100(a4),80(a4)
	rts
*****************************************************************************
put	lea	slist,a6		PUT MULTIPLE SPRITES
put1	move.l	(a6)+,a4
	cmpa.l	#0,a4
	beq.s	putend
	btst	#0,89(A4)		on?
	bne.s	put2			yes
	bsr	delay			no so delay instead
	bra.s	put1
put2	bsr	pblit
	bra.s	put1
putend	rts
pblit	move.l	56(a4),d7		PUT BLITTER ROUTINE	loop count
	move.l	flipin,a2		addr. in list
	move.l	(a2),a2			actual screen addr.
	add.l	36(a4),a2		screen pos.
	move.l	(a4),a1			data
	move.l	4(a4),a0		mask
pblit1	move.w	52(a4),d0		shift bits
	bsr	pblit2
	add.l	44(a4),a1		sprite plane size
	add.l	#fscsize,a2		screen size
	dbra	d7,pblit1
	rts
pblit2	btst	#14,dmaconr(a5)		PUT BLIT
	bne.s	pblit2
	move.l	a0,bltapth(a5)		mask
	move.l	a1,bltbpth(a5)		data
	move.l	a2,bltcpth(a5)		screen
	move.l	a2,bltdpth(a5)		screen
	move.l	16(a4),bltafwm(a5)
	move.l	20(a4),bltamod(a5)
	move.l	24(a4),bltcmod(a5)
	move.w	d0,bltcon1(a5)		shift for data
	ori.w	#$0fca,d0
	move.w	d0,bltcon0(a5)		straight copy plus shift for mask
	move.w	34(a4),bltsize(a5)
	rts
*****************************************************************************
blank	lea	slist,a6		BLANK MULTIPLE SPRITES
bnk1	move.l	(a6)+,a4
	cmpa.l	#0,a4
	beq.s	bnkend
	btst	#0,89(A4)		on?
	bne.s	bnk2			yes
	tst.b	112(a4)			remove count?
	bmi.s	bnk1			no
	subq.b	#1,112(a4)		yes	
bnk2	bsr	bblit
	bra.s	bnk1
bnkend	rts
bblit	move.l	56(a4),d7		BLANK BLITTER ROUTINE	loop count
	move.l	flipin,a2		addr. in list
	move.l	(a2),a2			actual screen addr.
	add.l	40(a4),a2		screen pos.
bblit1	bsr	bblit2
	add.l	#fscsize,a2		screen size
	dbra	d7,bblit1
	rts
bblit2	btst	#14,dmaconr(a5)		BLANK BLIT
	bne.s	bblit2
	move.w	#0,bltddat(a5)		output zero's only
	move.l	a2,bltdpth(a5)		screen
	move.w	22(a4),bltdmod(a5)
	move.w	#$0100,bltcon0(a5)	straight copy
	clr.w	bltcon1(a5)
	move.w	34(a4),bltsize(a5)
	rts
*****************************************************************************
vbgap	move.w	vposr(a5),d1		WAIT FOR UPDATE ON VBLANK
	move.w	vhposr(a5),d0
	lsr.w	#1,d1
	roxr.w	#1,d0
	lsr.w	#7,d0
	cmpi.w	#301,d0
	bne.s	vbgap
	rts
switch	move.l	flipin,a0		CHANGE SCREENS IN FLIPIN/OUT
	move.l	flipout,flipin
	move.l	a0,flipout		presently displayed screen in flipout
	rts
alter	lea	slist,a6		SWAP SCREEN ADDR. LOCATIONS
alt1	move.l	(a6)+,a4
	cmpa.l	#0,a4
	beq.s	altend
	move.l	36(a4),40(a4)
	bra.s	alt1
altend	rts
disp	moveq	#3-1,d7			SWITCH SCREENS
	move.l	#clistn,a0
	move.l	flipin,a1
makecl	move.l	(a1)+,d1
	add.l	#width*top,d1		offset to visible screen start
	COPPER	d1,6(a0),2(a0)
	add.l	#16,a0
	dbra	d7,makecl
	rts
*****************************************************************************
dolevel	add.l	#1,level		DISPLAY LEVEL
	cmpi.l	#100,level
	bmi.s	dolvl1
	move.l	#0,level
dolvl1	move.l	level,d0
	lea	strspc,a0
	lea	divider+32,a1
	bsr	hexdec			convert to decimal ascii
	lea	scr0a+996,a2
	lea	digits,a3
	move.l	#$00120026,d1		modulo
	move.l	#$00000401,d2		bltsize
	moveq	#2,d4
	bsr	led
	rts
*****************************************************************************
dolives	cmpi.l	#10,lives		DISPLAY LIVES
	bpl.s	doliv2			displays lives 0-9 only
	move.l	lives,d0		greater allowed but not displayed
	lea	strspc,a0
	lea	divider+36,a1
	bsr	hexdec			convert to decimal ascii
	lea	scr0a+670,a2
	lea	digits,a3
	move.l	#$00120026,d1		modulo
	move.l	#$00000401,d2		bltsize
	moveq	#2,d4
	bsr	led
doliv2	rts
*****************************************************************************
doscore	tst.l	plus			INCREMENT/DISPLAY SCORE/EXTRA LIFE
	beq.s	noscore
dosc1	move.l	plus,d0			[extra life?]
	add.l	d0,elscore
	move.l	elfreq,d1
	cmp.l	elscore,d1		is score a multiple of 20000/10000
	bpl.s	dosc2			no
	bsr	cake			yes.add extra life
	move.l	elfreq,d1
	sub.l	d1,elscore
dosc2	move.l	score,d0		get score
	add.l	plus,d0
	cmpi.l	#1000000,d0		max score+1
	bmi.s	dosc3
	moveq	#0,d0
dosc3	move.l	d0,score		add and re-save
	move.l	#0,plus
	lea	strspc,a0
	lea	divider+16,a1
	bsr	hexdec			convert to decimal ascii
	lea	scr0a+14,a2
	lea	digits,a3
	move.l	#$00120026,d1		modulo
	move.l	#$00000401,d2		bltsize
	moveq	#2,d4
	bsr	led
noscore	rts
*****************************************************************************
counts	btst	#4,sphero+89		DECLINE AND PRINT GAME COUNTER
	bne.s	ctout			dying? no count
	tst.w	deadt			stop timer?
	beq.s	count1			no
	subq.w	#1,deadt		yes
	bne.s	ctout
	bclr	#5,sphero+89		clear reborn bit
count1	tst.l	number
	beq.s	count2
	sub.l	#1,number
	bsr	dcount
ctout	rts
count2	lea	sphero,a3		kill hero
	bsr	death
	move.l	numbak,number		reset timer and display
	bsr	dcount
	rts
mcount	tst.l	number			DECLINE AND PRINT MAZE COUNTER
	beq.s	mcount1
	sub.l	#1,number
	bsr	dcount
	rts
mcount1	bclr	#0,bdhero+89		no. maze mode. end maze,time run out
	move.b	#1,bdhero+112		remove baldy
	bsr	wipesc			display extra points screen
	move.l	#endlvlm,minimap
	bsr	stsc
	add.l	#4000,plus
	move.w	#200,exsct
	move.b	#16,mode		set bit 4
	rts
dcount	move.l	number,d0		DISPLAY COUNT
	lea	strspc,a0
	lea	divider+24,a1
	bsr	hexdec
	lea	scr0a+692,a2
	lea	digitl,a3
	move.l	#$00240024,d1		modulo
	move.l	#$000009c2,d2		bltsize
	moveq	#4,d4
	bsr	led
	rts
*****************************************************************************
led	lea	strspc,a0		PRINT L.E.D. NUMBERS
	move.l	d4,d3			a0=string addr.
	lsr.l	#1,d3			a2=screen addr.
led1	move.l	a3,a1			d4=offset multiplication
	moveq	#0,d0			d1=modulo
	move.b	(a0)+,d0		d2=bltsize
	beq.s	endled
	cmpi.b	#48,d0			print 0(48) to 9(57)
	bmi.s	led1
	cmpi.b	#58,d0
	bpl.s	led1
	subi.w	#48,d0			make from zero
	lsl.w	d3,d0			* offset
	add.l	d0,a1
	bsr	tblit
	add.l	d4,a2			text width
	bra.s	led1
endled	rts
tblit	btst	#14,dmaconr(a5)		PUT TEXT BLITTER ROUTINE
	bne.s	tblit
	move.l	a1,bltapth(a5)
	move.l	a2,bltdpth(a5)
	move.l	#$ffffffff,bltafwm(a5)
	move.l	d1,bltamod(a5)
	clr.w	bltcon1(a5)
	move.w	#$09f0,bltcon0(a5)
	move.w	d2,bltsize(a5)
	rts
hexdec	clr.l	d2			CONVERT HEX TO ASCII/DEC
	move.l	(a1)+,d1		a0=output.a1=starting divider
	beq.s	ended			d0=hex
sub	cmp.l	d1,d0
	bmi.s	ascii
	addq.b	#1,d2
	sub.l	d1,d0
	bra.s	sub
ascii	add.b	#48,d2			make data ascii
	move.b	d2,(a0)+
	bra.s	hexdec
ended	move.b	#0,(a0)			zero terminate string
	rts
*****************************************************************************
setord	lea	scr0a+720,a2		DISPLAY ORDER GRAPHICS
	lea	order+10,a1
	move.l	#$000a001e,d1
	move.l	#$00000505,d2
	moveq	#3-1,d6			depth
	bsr	setord9			erase old order
	moveq	#5-1,d7			five types
	move.b	cycle,d0		which to display
	lea	scr0a+720,a2
	lea	order,a1
	move.l	#$00120026,d1		set new order
	move.l	#$00000501,d2
setord1	lsr.b	#1,d0
	bcc.s	setord2
	moveq	#3-1,d6			depth
	movem.l	a1-a2,-(sp)
	bsr	setord9
	movem.l	(sp)+,a1-a2
setord2	add.l	#2,a1
	add.l	#2,a2
	dbra	d7,setord1
	rts
setord9	bsr	tblit			PUT ON SCREEN
	add.l	#pscsize,a2
	add.l	#400,a1
	dbra	d6,setord9
	rts
*****************************************************************************
newshot	lea	scr0a+1680,a2		UPDATE BULLET POINTER
	lea	pointer+2,a1
	move.l	#$0002001e,d1
	move.l	#$00000385,d2
	moveq	#3-1,d7			depth
	bsr	nwsh3			erase old pointers
	lea	sphero,a4
	moveq	#0,d6
	move.b	110(a4),d5
nwsh1	lsr.b	#1,d5
	bcs.s	nwsh2
	add.l	#2,d6
	bra.s	nwsh1
nwsh2	lea	scr0a+1680,a2
	add.l	d6,a2
	lea	pointer,a1
	move.l	#$000a0026,d1		new pointer
	move.l	#$00000381,d2
	moveq	#3-1,d7			depth
	bsr	nwsh3
	rts
nwsh3	bsr	tblit			PUT ON SCREEN
	add.l	#pscsize,a2
	add.l	#168,a1
	dbra	d7,nwsh3
	rts
*****************************************************************************
disphw	lea	xplist,a3		SET UP HARDWARE SPRITES
dhw1	tst.w	(a3)			end of list?
	bmi.s	dhw2			yes
	bsr	disphw1
	add.l	#24,a3
	bra.s	dhw1
dhw2	rts
disphw1	moveq	#0,d1			clear
	moveq	#0,d2
	move.w	6(a3),d0		get offset for lists
dphw1	lsr.w	#1,d0
	bcs.s	dphw2
	add.w	#140,d1
	add.w	#8,d2
	bra.s	dphw1
dphw2	move.l	12(a3),a2		get sprite base addr.
	add.l	d1,a2			add offset
	move.l	8(a3),(a2)		put pos. into sprite command words
	move.l	a2,d0			save addr.
	move.l	16(a3),a2		get colour pos. in copper list
	lea	hwslist,a1		get colour list base addr.
	add.l	d2,a1			add offset
	move.w	2(a1),2(a2)		put colours into copper list
	move.w	4(a1),6(a2)
	move.w	6(a1),10(a2)
	move.l	20(a3),a2		get sprite pos. in copper list
	COPPER	d0,6(a2),2(a2)		put sprite addr. into copper list
	rts
remhw	lea	xplist,a3		REMOVE HARDWARE SPRITES
rem1	tst.w	(a3)			end of list?
	bmi.s	rem2			yes
	bsr	remhw1
	add.l	#24,a3
	bra.s	rem1
rem2	rts
remhw1	move.l	20(a3),a2		get sprite pos. in copper list
	move.l	#offscr,d0
	COPPER	d0,6(a2),2(a2)		put offscr addr. into copper list
	rts
*****************************************************************************
GRname	dc.b	"graphics.library",0
	even
scrlist	dc.l	scr1,scr2,scr3,scr1a,scr2a,scr3a
flipin	dc.l	scrlist
flipout	dc.l	scrlist+12
screens	dc.l	level1,level1-minimap,level2,level1-minimap
	dc.l	level3,level1-minimap,level4,level1-minimap
	dc.l	level5,level1-minimap,level6,level1-minimap
	dc.l	level7,level1-minimap,level8,level1-minimap
	dc.l	level9,level1-minimap,level10,level1-minimap
	dc.l	level11,level1-minimap,level12,level1-minimap
	dc.l	level13,level1-minimap,level14,level1-minimap
	dc.l	level15,level1-minimap,level16,level1-minimap
	dc.l	level17,level1-minimap,level18,level1-minimap
	dc.l	level19,level1-minimap,level20,level1-minimap
	dc.l	level21,level1-minimap,level22,level1-minimap
	dc.l	level23,level1-minimap,level24,level1-minimap,0
actscr	dc.l	screens
windup	dc.w	0			end of level stroll delay
strspc	ds.l	10
elscore	dc.l	0			extra life score count
elfreq	dc.l	20000			extra life frequency (50000/40000)
score	dc.l	0
hsscore	dc.l	0
plus	dc.l	0
lives	dc.l	3
level	dc.l	0
divider	dc.l	1000000000,100000000,10000000,1000000,100000,10000,1000,100,10,1,0
mode	dc.b	0,0
xmode	dc.b	4,0
deadt	dc.w	0			timer stop delay
exsct	dc.w	0
iscount	dc.w	250			info/title swap delay
isonoff	dc.w	0			0=title : 1=info
taboff	dc.l	0
tabline	dc.l	0
onechar	dc.b	0,0," ",0
scount0	dc.w	0
scount1	dc.w	0
scount2	dc.w	0
scount3	dc.w	0
workmsg	dc.l	0
rand	dc.l	$2579fca6
bonrand	dc.w	$690,$20d0,$4830,$89d0,$96f0,$b7c0,$be50,$d890,$ec40,$fff0
bonaddr	dc.l	cake,range,stoptim,bank,flower,timer,alloff,lswitch,fireoff,death
slist	dc.l	bon1,bon2,bon3,bon4,alien0,alien1,alien2,alien3,alien4,alien5
	dc.l	alien6,alien7,alien8,alien9,shot,sphero,bdhero,chhero,0
hwslist	dc.l	$00000eee,$0666000c,$00000eee,$00ce000c,$00000f10,$0ec00666
	dc.l	$00000ec0,$0f100eee,$00000ec0,$0eee0000
chhero	dc.l	para,para+3384,dummy,dummy,$ffff0000,$00100020,$00200010,$0fca0000
	dc.l	$00000bc4,0,0,1128,0,0,3-1,0,0,0,parlist,$00030003,parlist+16
	dc.l	$00050005,$00010000,0,0,0,0,0,0
bdhero	dc.l	bald,bald+816,dummy,dummy,$ffff0000,$000c0024,$0024000c,$0fca0000
	dc.l	$00010442,0,0,272,0,0,3-1,$00100001,4800,40,anlbald
	dc.l	$00010001,anlbald+8,$00030003,$00080000,0,0,0,0,0,0,0,0
mlist	dc.l	bsmap1,$0060,1920,1600,bsmap2,$00d0,1920,1600,bsmap3,$00b0,2880,1100,bsmap4,$0080,2880,1100
	dc.l	bsmap5,$0010,8320,1350,bsmap6,$0100,8320,1200,bsmap7,$0120,1920,1150,bsmap8,$0010,1920,1150
	dc.l	bsmap9,$0120,5760,1000,bsmap10,$0010,5760,1000,bsmap11,$0120,8320,950,bsmap12,$0010,8320,950
	dc.l	bsmap13,$0120,1920,1250,bsmap14,$0010,1920,1250,bsmap15,$0010,4800,950,bsmap16,$0120,4800,950
parlist	dc.l	para,para+6,para+12,para+18,para+3384,para+3390,para+3396,para+3402
dielist	dc.l	dying+14,dying+12,dying+10,dying+8,dying+6,dying+4,dying+2,dying
	dc.l	dying+1166,dying+1164,dying+1162,dying+1160
	dc.l	dying+1158,dying+1156,dying+1154,dying+1152
anlhero	dc.l	hero+6,hero+4,hero+582,hero+580
anrhero	dc.l	hero,hero+2,hero+576,hero+578
anlbald	dc.l	bald,bald+2,bald+816,bald+818
anrbald	dc.l	bald+4,bald+6,bald+820,bald+822
andbald	dc.l	bald+8,bald+10,bald+824,bald+826
anubald	dc.l	bald+12,bald+14,bald+828,bald+830
shot	dc.l	0,0,dummy,dummy,$ffff0000,$00060024,$00240006,$0fca0000
	dc.l	$000001c2,0,0,70,0,0,3-1,$0000ffff,0,0,0,0,0,0
	dc.l	$00000000,0,0,0,0,0,$00220000
bon1	dc.l	0,0,dummy,dummy,$ffffffff,$00120026,$00260012,$0fca0000
	dc.l	$00000501,0,0,400,0,0,3-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
bon2	dc.l	0,0,dummy,dummy,$ffffffff,$00120026,$00260012,$0fca0000
	dc.l	$00000501,0,0,400,0,0,3-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
bon3	dc.l	0,0,dummy,dummy,$ffffffff,$00120026,$00260012,$0fca0000
	dc.l	$00000501,0,0,400,0,0,3-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
bon4	dc.l	0,0,dummy,dummy,$ffffffff,$00120026,$00260012,$0fca0000
	dc.l	$00000501,0,0,400,0,0,3-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
newston	dc.l	anlalns,anralns,anlalns+16,anralns+16,$00090009,$00010100,$01010000
newice	dc.l	anlalni,anralni,anlalni+16,anralni+16,$00080008,$00010300,$00000000
newelec	dc.l	anlalne,anralne,anlalne+16,anralne+16,$000a000a,$00010500,$00000000
newfire	dc.l	anlalnf,anralnf,anlalnf+16,anralnf+16,$00030003,$00020300,$00000000
newligh	dc.l	anlalnl,anralnl,anlalnl+16,anralnl+16,$00030003,$00010100,$00000000
anlalns	dc.l	stonl+6,stonl+4,stonl+2,stonl,stonl+486,stonl+484,stonl+482,stonl+480
anralns	dc.l	stonr,stonr+2,stonr+4,stonr+6,stonr+480,stonr+482,stonr+484,stonr+486
anlalni	dc.l	ice+6,ice+4,ice+2,ice,ice+486,ice+484,ice+482,ice+480
anralni	dc.l	ice,ice+2,ice+4,ice+6,ice+480,ice+482,ice+484,ice+486
anlalne	dc.l	elec+6,elec+4,elec+2,elec,elec+486,elec+484,elec+482,elec+480
anralne	dc.l	elec,elec+2,elec+4,elec+6,elec+480,elec+482,elec+484,elec+486
anlalnf	dc.l	fire+6,fire+4,fire+2,fire,fire+486,fire+484,fire+482,fire+480
anralnf	dc.l	fire,fire+2,fire+4,fire+6,fire+480,fire+482,fire+484,fire+486
anlalnl	dc.l	light+6,light+4,light+2,light,light+486,light+484,light+482,light+480
anralnl	dc.l	light,light+2,light+4,light+6,light+480,light+482,light+484,light+486
anfston	dc.l	stonf+6,stonf+4,stonf+2,stonf,stonf+486,stonf+484,stonf+482,stonf+480
angston	dc.l	stonf,stonf+2,stonf+4,stonf+6,stonf+480,stonf+482,stonf+484,stonf+486
anfice	dc.l	icef+6,icef+4,icef+2,icef,icef+486,icef+484,icef+482,icef+480
angice	dc.l	icef,icef+2,icef+4,icef+6,icef+480,icef+482,icef+484,icef+486
anfelec	dc.l	elecf+6,elecf+4,elecf+2,elecf,elecf+486,elecf+484,elecf+482,elecf+480
angelec	dc.l	elecf,elecf+2,elecf+4,elecf+6,elecf+480,elecf+482,elecf+484,elecf+486
anffire	dc.l	firef+6,firef+4,firef+2,firef,firef+486,firef+484,firef+482,firef+480
angfire	dc.l	firef,firef+2,firef+4,firef+6,firef+480,firef+482,firef+484,firef+486
anfligh	dc.l	lightf+6,lightf+4,lightf+2,lightf,lightf+486,lightf+484,lightf+482,lightf+480
angligh	dc.l	lightf,lightf+2,lightf+4,lightf+6,lightf+480,lightf+482,lightf+484,lightf+486
bkb1	dc.l	0,bkbmsk,dummy,dummy,$ffff0000,$ffff0024,$0024000c,$0fca0000
	dc.l	$000002c2,0,0,528,0,0,2-1
blk	dc.l	bkbmsk,bkbmsk,dummy,dummy,$ffff0000,$ffff0024,$0024ffff,$0fca0000
	dc.l	$00000202,0,0,0,0,0,0
point	dc.l	pointer,0,dummy,dummy,$ffffffff,$00000026,$00000000,$05cc0000
	dc.l	$00000381,0,0,28,0,0,3-1
*****************************************************************************
lvmap1	dc.w	%1111111100111111,%1100001111111100,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111111110,%0000000001111111,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000011000000,%0011110000000011,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111000000,%0111111000000011,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000001111,%0000000011110000,%0000011100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111100000000000,%0000000000000000,%0001111100000000
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1111111000000000,%0000000000000000,%0111111100000000	end
	dc.w	%1111111100111111,%1100001111111100,%1111111100000000
	dc.w	%1111111100111111,%1100001111111100,%1111111100000000
	dc.w	%1111111100111111,%1100001111111100,%1111111100000000
	dc.w	%1111111100111111,%1100001111111100,%1111111100000000
	dc.w	%1111111100111111,%1100001111111100,%1111111100000000
lvmap2	dc.w	%1111111000000001,%1111111110000000,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000111111000,%0000000000011111,%1000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100100001000010,%0000000001000010,%0001001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000001111,%1100001111110000,%0000011100000000
	dc.w	%1100000000001000,%0100001000010000,%0000001100000000
	dc.w	%1100000000001000,%0100001000010000,%0000001100000000
	dc.w	%1100000000001000,%0100001000010000,%0000001100000000
	dc.w	%1100000000001000,%0100001000010000,%0000001100000000
	dc.w	%1100000111111000,%0110011000011111,%1000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111111000000001,%1111111110000000,%0111111100000000
	dc.w	%1111111000000001,%1111111110000000,%0111111100000000
	dc.w	%1111111000000001,%1111111110000000,%0111111100000000
	dc.w	%1111111000000001,%1111111110000000,%0111111100000000
	dc.w	%1111111000000001,%1111111110000000,%0111111100000000
lvmap3	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111000000,%0011110000000011,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000111100,%0000000000111100,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000011,%1111111111000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
lvmap4	dc.w	%1110000111111000,%0111111000011111,%1000011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000001100000,%0001100000000110,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011000000011,%0000000011000000,%0110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000001100000,%0001100000000110,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000000000011,%0000000011000000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000001100000,%0001100000000110,%0000001100000000
	dc.w	%1100000011110000,%0011110000001111,%0000001100000000	end
	dc.w	%1110000111111000,%0111111000011111,%1000011100000000
	dc.w	%1110000111111000,%0111111000011111,%1000011100000000
	dc.w	%1110000111111000,%0111111000011111,%1000011100000000
	dc.w	%1110000111111000,%0111111000011111,%1000011100000000
	dc.w	%1110000111111000,%0111111000011111,%1000011100000000
lvmap5	dc.w	%1111111000000000,%0000000000000000,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011000011000,%0110011000011000,%0110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000011000011,%0000000011000011,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100100100100100,%1001100100100100,%1001001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111000011,%0011110011000011,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111111000000000,%0000000000000000,%0111111100000000
	dc.w	%1111111000000000,%0000000000000000,%0111111100000000
	dc.w	%1111111000000000,%0000000000000000,%0111111100000000
	dc.w	%1111111000000000,%0000000000000000,%0111111100000000
	dc.w	%1111111000000000,%0000000000000000,%0111111100000000
lvmap6	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000110000,%0000000000001100,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000011000000,%0000000000000011,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001100000000,%0000000000000000,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
lvmap7	dc.w	%1111111111111000,%0100001000011111,%1111111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000	start
	dc.w	%1111000000000000,%0000000000000000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111111100001111,%1110011111110000,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1111100000111111,%1100001111111100,%0001111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000011111111,%0001100011111111,%0000111100000000
	dc.w	%1111000000000000,%0001100000000000,%0000111100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011000010000,%0100001000001000,%0110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000	end
	dc.w	%1111111111111000,%0100001000011111,%1111111100000000
	dc.w	%1111111111111000,%0100001000011111,%1111111100000000
	dc.w	%1111111111111000,%0100001000011111,%1111111100000000
	dc.w	%1111111111111000,%0100001000011111,%1111111100000000
	dc.w	%1111111111111000,%0100001000011111,%1111111100000000
lvmap8	dc.w	%1111111111000000,%1111111100000011,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111001111111,%1111111111111110,%0111001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100111111001111,%1111111111110011,%1111001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100111111111111,%0011110011111111,%1111001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100111111110011,%1111111111001111,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111111111000000,%1111111100000011,%1111111100000000
	dc.w	%1111111111000000,%1111111100000011,%1111111100000000
	dc.w	%1111111111000000,%1111111100000011,%1111111100000000
	dc.w	%1111111111000000,%1111111100000011,%1111111100000000
	dc.w	%1111111111000000,%1111111100000011,%1111111100000000
lvmap9	dc.w	%1111110000001111,%0000000011110000,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110011001001100,%1001100100110010,%0110011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111001001001001,%0010010010010010,%0100111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110010010010010,%0101101001001001,%0010011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100110011001100,%1100001100110011,%0011001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111110000001111,%0000000011110000,%0011111100000000
	dc.w	%1111110000001111,%0000000011110000,%0011111100000000
	dc.w	%1111110000001111,%0000000011110000,%0011111100000000
	dc.w	%1111110000001111,%0000000011110000,%0011111100000000
	dc.w	%1111110000001111,%0000000011110000,%0011111100000000
lvmap10	dc.w	%1111110000000000,%0011110000000000,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000001111,%1111111111110000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111110000,%0000000000001111,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000011111,%1000000111111000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111110000000000,%0111111000000000,%0011101100000000
	dc.w	%1110000000000000,%0001100000000000,%0010111100000000
	dc.w	%1110000000000000,%0000000000000000,%0010011100000000
	dc.w	%1111110000000000,%0000000000000000,%0010011100000000
	dc.w	%1100010000000000,%0000000000000000,%0010011100000000
	dc.w	%1100010000000000,%0001100000000000,%0010111100000000	end
	dc.w	%1111110000000000,%0011110000000000,%0011101100000000
	dc.w	%1111110000000000,%0011110000000000,%0011111100000000
	dc.w	%1111110000000000,%0011110000000000,%0011111100000000
	dc.w	%1111110000000000,%0011110000000000,%0011111100000000
	dc.w	%1111110000000000,%0011110000000000,%0011111100000000
lvmap11	dc.w	%1100111100111100,%1110011100111100,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000011111111,%0000000011111111,%0000001100000000
	dc.w	%1100000010000000,%0000000000000001,%0000001100000000
	dc.w	%1100011110000000,%0000000000000001,%1110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000001,%1110011110000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011000000000,%0000000000000000,%0110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100100111100000,%0111111000000111,%1001001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111100111100000,%0000000000000111,%1001111100000000
	dc.w	%1100100000100000,%0000000000000100,%0001001100000000
	dc.w	%1100100000100000,%0001100000000100,%0001001100000000
	dc.w	%1100100000100000,%0011110000000100,%0001001100000000
	dc.w	%1100100000100000,%0111111000000100,%0001001100000000	end
	dc.w	%1111111100111100,%1111111100111100,%1111111100000000
	dc.w	%1100111100111100,%1110011100111100,%1111001100000000
	dc.w	%1100111100111100,%1110011100111100,%1111001100000000
	dc.w	%1100111100111100,%1110011100111100,%1111001100000000
	dc.w	%1100111100111100,%1110011100111100,%1111001100000000
lvmap12	dc.w	%1111111000011111,%0000000011111000,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000111110000,%0011110000001111,%1000001100000000
	dc.w	%1100011100000000,%0000000000000000,%1110001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000001111,%1110011111110000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111111111110000,%0000000000001111,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000010001,%0000000010001000,%0000001100000000
	dc.w	%1100001110011111,%0001100011111001,%1100001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000	end
	dc.w	%1111111000011111,%0000000011111000,%0111111100000000
	dc.w	%1111111000011111,%0000000011111000,%0111111100000000
	dc.w	%1111111000011111,%0000000011111000,%0111111100000000
	dc.w	%1111111000011111,%0000000011111000,%0111111100000000
	dc.w	%1111111000011111,%0000000011111000,%0111111100000000
lvmap13	dc.w	%1111111100001111,%1111111111110000,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000011111111,%0000000011111111,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111000011,%1100001111000011,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111111000110000,%1111111100001100,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111001111,%1100001111110011,%1100001100000000
	dc.w	%1100000000001000,%0000000000010000,%0000001100000000
	dc.w	%1100000000001000,%0000000000010000,%0000001100000000
	dc.w	%1100000000001000,%0000000000010000,%0000001100000000
	dc.w	%1100000000001000,%0000000000010000,%0000001100000000	end
	dc.w	%1111111100001111,%1111111111110000,%1111111100000000
	dc.w	%1111111100001111,%1111111111110000,%1111111100000000
	dc.w	%1111111100001111,%1111111111110000,%1111111100000000
	dc.w	%1111111100001111,%1111111111110000,%1111111100000000
	dc.w	%1111111100001111,%1111111111110000,%1111111100000000
lvmap14	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000001100000,%0000000000000110,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111111111111111,%1100001111111111,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000000011000,%0001100000011000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011111100111,%1110011111100111,%1110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
lvmap15	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000001000,%0000000000010000,%0000001100000000
	dc.w	%1111000100111110,%0111111001111100,%1000111100000000
	dc.w	%1100000100001000,%0100001000010000,%1000001100000000
	dc.w	%1100000100001000,%0000000000010000,%1000001100000000
	dc.w	%1100000100001000,%0000000000010000,%1000001100000000
	dc.w	%1100000100001000,%0100001000010000,%1000001100000000
	dc.w	%1100011111001001,%1111111110010011,%1110001100000000
	dc.w	%1100000100001000,%0100001000010000,%1000001100000000
	dc.w	%1100000100001000,%0000000000010000,%1000001100000000
	dc.w	%1100000100001000,%0000000000010000,%1000001100000000
	dc.w	%1100000100001000,%0100001000010000,%1000001100000000
	dc.w	%1111000100111110,%0111111001111100,%1000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000111111000,%1111111100011111,%1000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
	dc.w	%1111110000000000,%0000000000000000,%0011111100000000
lvmap16	dc.w	%1111000000000000,%0011110000000000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0011110000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000000000110,%0000000001100000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011001100000,%0000000000000110,%0110001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000000000110,%0000000001100000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111000000000000,%0011110000000000,%0000111100000000
	dc.w	%1111000000000000,%0011110000000000,%0000111100000000
	dc.w	%1111000000000000,%0011110000000000,%0000111100000000
	dc.w	%1111000000000000,%0011110000000000,%0000111100000000
	dc.w	%1111000000000000,%0011110000000000,%0000111100000000
lvmap17	dc.w	%1111001110000000,%0000000000000001,%1100111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111000000000,%0011110000000000,%0111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000011000000,%0000000000000011,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000011000,%0000000000011000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0001100000000000,%0000001100000000
	dc.w	%1100000000000000,%0001100000000000,%0000001100000000
	dc.w	%1100000000000000,%0001100000000000,%0000001100000000
	dc.w	%1111110111101111,%0001100011110111,%1011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%1110011100000000,%0000001100000000
	dc.w	%1100000000000011,%1000000111000000,%0000001100000000
	dc.w	%1100000000001110,%0000000001110000,%0000001100000000
	dc.w	%1100000000111000,%0000000000011100,%0000001100000000
	dc.w	%1100000011100000,%0000000000000111,%0000001100000000	end
	dc.w	%1111001110000000,%0000000000000001,%1100111100000000
	dc.w	%1111001110000000,%0000000000000001,%1100111100000000
	dc.w	%1111001110000000,%0000000000000001,%1100111100000000
	dc.w	%1111001110000000,%0000000000000001,%1100111100000000
	dc.w	%1111001110000000,%0000000000000001,%1100111100000000
lvmap18	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000011,%1100001111000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111000000,%0000000000000011,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000011,%1100001111000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111000000,%0000000000000011,%1100001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	end
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
	dc.w	%1111110000111100,%0011110000111100,%0011111100000000
lvmap19	dc.w	%1111111100000111,%1000000111100000,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111111000000000,%0110011000000000,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000011110000,%0011110000001111,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001100001111,%1100001111110000,%1100001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100111100000011,%0000000011000000,%1111001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000
	dc.w	%1100000000000011,%0000000011000000,%0000001100000000	end
	dc.w	%1111111100000111,%1000000111100000,%1111111100000000
	dc.w	%1111111100000111,%1000000111100000,%1111111100000000
	dc.w	%1111111100000111,%1000000111100000,%1111111100000000
	dc.w	%1111111100000111,%1000000111100000,%1111111100000000
	dc.w	%1111111100000111,%1000000111100000,%1111111100000000
lvmap20	dc.w	%1111111111100100,%1111111100100111,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000011111,%0000000011111000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000001111100,%0000000000100001,%1111111100000000
	dc.w	%1100011110000000,%1001100100100000,%0000001100000000
	dc.w	%1100000000000000,%0000000000100000,%0000001100000000
	dc.w	%1100000000000000,%0000000000100000,%0000001100000000
	dc.w	%1100000000000000,%0000000000111111,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111111111100000,%1100001100000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000111,%1111111100000000
	dc.w	%1100000000000100,%0000000000000000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000001111101,%0010010010100000,%0000001100000000
	dc.w	%1100011110000100,%0000000000111111,%1111001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000	end
	dc.w	%1111111111100100,%1111111100100111,%1111111100000000
	dc.w	%1111111111100100,%1111111100100111,%1111111100000000
	dc.w	%1111111111100100,%1111111100100111,%1111111100000000
	dc.w	%1111111111100100,%1111111100100111,%1111111100000000
	dc.w	%1111111111100100,%1111111100100111,%1111111100000000
lvmap21	dc.w	%1111111100111111,%1111111111111100,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1111111111111100,%1111111100111111,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111100111111111,%1110011111111111,%1001111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1111111111100111,%1111111111100111,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100010000000000,%0000000000000000,%0010001100000000
	dc.w	%1111110011111110,%0111111001111111,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0001100000000000,%0000001100000000	end
	dc.w	%1111111100111111,%1111111111111100,%1111111100000000
	dc.w	%1111111100111111,%1111111111111100,%1111111100000000
	dc.w	%1111111100111111,%1111111111111100,%1111111100000000
	dc.w	%1111111100111111,%1111111111111100,%1111111100000000
	dc.w	%1111111100111111,%1111111111111100,%1111111100000000
lvmap22	dc.w	%1100111111110000,%0011110000001111,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100011111000000,%0000000000000011,%1110001100000000
	dc.w	%1100000000000011,%1111111111000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111000000011000,%0000000000011000,%0000111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000110000001,%1100001110000001,%1000001100000000
	dc.w	%1100001111000011,%1110011111000011,%1100001100000000
	dc.w	%1100000110000001,%1100001110000001,%1000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1111100000011110,%0001100001111000,%0001111100000000
	dc.w	%1100000000000000,%0011110000000000,%0000001100000000
	dc.w	%1100000000000000,%0001100000000000,%0000001100000000
	dc.w	%1100000110000000,%0011110000000001,%1000001100000000
	dc.w	%1100001111000000,%0001100000000011,%1100001100000000	end
	dc.w	%1100111111110000,%0011110000001111,%1111001100000000
	dc.w	%1100111111110000,%0011110000001111,%1111001100000000
	dc.w	%1100111111110000,%0011110000001111,%1111001100000000
	dc.w	%1100111111110000,%0011110000001111,%1111001100000000
	dc.w	%1100111111110000,%0011110000001111,%1111001100000000
lvmap23	dc.w	%1111110011111100,%0000000000111111,%0011111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100001111110000,%1111111100001111,%1100001100000000
	dc.w	%1100001000010000,%1000000100001000,%0100001100000000
	dc.w	%1100000000010000,%0000000000001000,%0000001100000000
	dc.w	%1100000000010000,%0000000000001000,%0000001100000000
	dc.w	%1100000000010000,%0000000000001000,%0000001100000000
	dc.w	%1111111000011111,%1000000111111000,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0001100000000000,%0000001100000000
	dc.w	%1100000111100000,%0010010000000111,%1000001100000000
	dc.w	%1100001000000000,%0100001000000000,%0100001100000000
	dc.w	%1100010000000000,%1000000100000000,%0010001100000000
	dc.w	%1100100000000001,%0000000010000000,%0001001100000000
	dc.w	%1100000000000010,%0000000001000000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000110000,%0000000000001100,%0000001100000000	end
	dc.w	%1111110011111100,%0000000000111111,%0011111100000000
	dc.w	%1111110011111100,%0000000000111111,%0011111100000000
	dc.w	%1111110011111100,%0000000000111111,%0011111100000000
	dc.w	%1111110011111100,%0000000000111111,%0011111100000000
	dc.w	%1111110011111100,%0000000000111111,%0011111100000000
lvmap24	dc.w	%1111111111001111,%1111111111110011,%1111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110011111100000,%0001100000000111,%1110011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000100,%0000000000100000,%0000001100000000
	dc.w	%1110011111111100,%0011110000111111,%1110011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1111111011111000,%1111111100011111,%0111111100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000111,%1111111111100000,%0000001100000000
	dc.w	%1111000000001100,%0001100000110000,%0000111100000000
	dc.w	%1111000000000000,%0001100000000000,%0000111100000000
	dc.w	%1111100000000000,%0001100000000000,%0001111100000000
	dc.w	%1111110000000000,%0011110000000000,%0011111100000000	end
	dc.w	%1111111111001111,%1100001111110011,%1111111100000000
	dc.w	%1111111111001111,%1111111111110011,%1111111100000000
	dc.w	%1111111111001111,%1111111111110011,%1111111100000000
	dc.w	%1111111111001111,%1111111111110011,%1111111100000000
	dc.w	%1111111111001111,%1111111111110011,%1111111100000000

endlvlm	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111000001000001,%0111010111010000,%0110111100000000
	dc.w	%1111011101011101,%0011010111010111,%1110111100000000
	dc.w	%1111000011011101,%0101010111010000,%0110111100000000
	dc.w	%1111011101011101,%0110010111011111,%0111111100000000
	dc.w	%1111000001000001,%0111010000010000,%0110111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1100110011110000,%0011100000011100,%0000111100000000
	dc.w	%1100110011100011,%0001000110001000,%1100011100000000
	dc.w	%1100110011100111,%1001001111001001,%1110011100000000
	dc.w	%1100000000100111,%1001001111001001,%1110011100000000
	dc.w	%1111110011100111,%1001001111001001,%1110011100000000
	dc.w	%1111110011100011,%0001000110001000,%1100011100000000
	dc.w	%1111110011110000,%0011100000011100,%0000111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1100000100000100,%0001011101000001,%0000011100000000
	dc.w	%1101110101110111,%0111001101110111,%0111111100000000
	dc.w	%1100000101110111,%0111010101110111,%0000011100000000
	dc.w	%1101111101110111,%0111011001110111,%1111011100000000
	dc.w	%1101111100000100,%0001011101110111,%0000011100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	end
	ds.w	12
endmazm	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111000001000001,%0111010111010000,%0110111100000000
	dc.w	%1111011101011101,%0011010111010111,%1110111100000000
	dc.w	%1111000011011101,%0101010111010000,%0110111100000000
	dc.w	%1111011101011101,%0110010111011111,%0111111100000000
	dc.w	%1111000001000001,%0111010000010000,%0110111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1110000001110000,%0011100000011100,%0000111100000000
	dc.w	%1100011000100011,%0001000110001000,%1100011100000000
	dc.w	%1100111100100111,%1001001111001001,%1110011100000000
	dc.w	%1110000001100111,%1001001111001001,%1110011100000000
	dc.w	%1100111100100111,%1001001111001001,%1110011100000000
	dc.w	%1100011000100011,%0001000110001000,%1100011100000000
	dc.w	%1110000001110000,%0011100000011100,%0000111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1100000100000100,%0001011101000001,%0000011100000000
	dc.w	%1101110101110111,%0111001101110111,%0111111100000000
	dc.w	%1100000101110111,%0111010101110111,%0000011100000000
	dc.w	%1101111101110111,%0111011001110111,%1111011100000000
	dc.w	%1101111100000100,%0001011101110111,%0000011100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap1	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000010000,%0001100000001000,%0000001100000000
	dc.w	%1100000000010000,%0000100000001000,%0000001100000000
	dc.w	%1100111110010011,%1100100111001001,%1110001100000000
	dc.w	%1100100000010010,%0100000100001001,%0001001100000000
	dc.w	%1100100000010010,%0100000100001001,%0001001100000000
	dc.w	%1100111110010010,%0100100111001001,%0001001100000000
	dc.w	%1100000010000010,%0000100000001001,%0001001100000000
	dc.w	%1100000010000010,%0000110000001001,%0001001100000000
	dc.w	%1100111110010010,%0100111111000001,%1110001100000000
	dc.w	%1100000000010000,%0100000000000000,%0000001100000000
	dc.w	%1100000000110000,%1100000000011100,%0000001100000000
	dc.w	%1111100111110011,%1100110011111110,%0111001100000000
	dc.w	%1100100000001100,%0000010010000000,%0001001100000000
	dc.w	%1100100000001100,%0000010010000000,%0001001100000000
	dc.w	%1100100111110011,%1111110010011111,%1001001100000000
	dc.w	%1100000001100000,%0000010000000000,%0001001100000000
	dc.w	%1100000001000000,%0000010000000000,%0001001100000000
	dc.w	%1111111001001111,%0010011110010011,%1001001100000000
	dc.w	%1100000001001000,%0010000000010010,%0001001100000000
	dc.w	%1100000001001000,%0010000000110010,%0001001100000000
	dc.w	%1100111111001001,%0010011111110010,%0111001100000000
	dc.w	%1100000000000001,%0000000000000000,%0001001100000000
	dc.w	%1100000000000001,%0000000000000000,%0001001100000000
	dc.w	%1111111111111111,%1111111111111111,%1111001100000000	end
	ds.w	12
bsmap2	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000010000,%0001100000001000,%0000001100000000
	dc.w	%1100000000010000,%0001000000001000,%0000001100000000
	dc.w	%1100111110010011,%1001001111001001,%1110001100000000
	dc.w	%1100100000010000,%1000001001001001,%0001001100000000
	dc.w	%1100100000010000,%1000001001001001,%0001001100000000
	dc.w	%1100111110010011,%1001001001001001,%0001001100000000
	dc.w	%1100000010010000,%0001000001000001,%0001001100000000
	dc.w	%1100000010010000,%0011000001000001,%0001001100000000
	dc.w	%1100111110000011,%1111001001001001,%1110001100000000
	dc.w	%1100000000000000,%0000001000001000,%0000001100000000
	dc.w	%1100000000111000,%0000001100001100,%0000001100000000
	dc.w	%1100111001111111,%0011001111001111,%1001111100000000
	dc.w	%1100100000000001,%0010000000110000,%0001001100000000
	dc.w	%1100100000000001,%0010000000110000,%0001001100000000
	dc.w	%1100100111111001,%0011111111001111,%1001001100000000
	dc.w	%1100100000000000,%0010000000000110,%0000001100000000
	dc.w	%1100100000000000,%0010000000000010,%0000001100000000
	dc.w	%1100100111001001,%1110010011110010,%0111111100000000
	dc.w	%1100100001001000,%0000010000010010,%0000001100000000
	dc.w	%1100100001001100,%0000010000010010,%0000001100000000
	dc.w	%1100110001001111,%1110010010010011,%1111001100000000
	dc.w	%1100100000000000,%0000000010000000,%0000001100000000
	dc.w	%1100100000000000,%0000000010000000,%0000001100000000
	dc.w	%1100111111111111,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap3	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100000000000000,%0010010000000000,%0000001100000000
	dc.w	%1100000000000000,%0010010000000000,%0000001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100111000000000,%0000000000000000,%0111001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100110011100111,%1111111111100111,%0011001100000000
	dc.w	%1100000011100100,%0001100000100111,%0000001100000000
	dc.w	%1100000010000100,%0001100000100001,%0000001100000000
	dc.w	%1111001110000100,%1001100100100001,%1100111100000000
	dc.w	%1111001000011100,%1001100100111000,%0100111100000000
	dc.w	%1111111000010000,%1001100100001000,%0111111100000000
	dc.w	%1111100001110000,%1001100100001110,%0001111100000000
	dc.w	%1111100001000011,%1001100111000010,%0001111100000000
	dc.w	%1110000111000010,%0001100001000011,%1000011100000000
	dc.w	%1110000100001110,%0001100001110000,%1000011100000000
	dc.w	%1100011100001000,%0111111000010000,%1110001100000000
	dc.w	%1100010000111000,%0100001000011100,%0010001100000000
	dc.w	%1100110000100001,%1010010110001100,%0011001100000000
	dc.w	%1100000011100001,%0001100010000111,%0000001100000000
	dc.w	%1100000011100111,%0010010011100111,%0000001100000000
	dc.w	%1111111111100111,%1111111111100111,%1111111100000000	end
	ds.w	12
bsmap4	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100000000000000,%0010010000000000,%0000001100000000
	dc.w	%1100000000000000,%0010010000000000,%0000001100000000
	dc.w	%1100111111111111,%1110011111111111,%1111001100000000
	dc.w	%1100111000000000,%0000000000000000,%0111001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100110011100111,%1111111111100111,%0011001100000000
	dc.w	%1100000011100100,%0001100000100111,%0000001100000000
	dc.w	%1100000010000100,%0001100000100001,%0000001100000000
	dc.w	%1111001110000100,%1001100100100001,%1100111100000000
	dc.w	%1111001000011100,%1001100100111000,%0100111100000000
	dc.w	%1111111000010000,%1001100100001000,%0111111100000000
	dc.w	%1111100001110000,%1001100100001110,%0001111100000000
	dc.w	%1111100001000011,%1001100111000010,%0001111100000000
	dc.w	%1110000111000010,%0001100001000011,%1000011100000000
	dc.w	%1110000100001110,%0001100001110000,%1000011100000000
	dc.w	%1100011100001000,%0111111000010000,%1110001100000000
	dc.w	%1100010000111000,%0100001000011100,%0010001100000000
	dc.w	%1100110000110001,%1010010110000100,%0011001100000000
	dc.w	%1100000011100001,%0001100010000111,%0000001100000000
	dc.w	%1100000011100111,%0010010011100111,%0000001100000000
	dc.w	%1111111111100111,%1111111111100111,%1111111100000000	end
	ds.w	12
bsmap5	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000100000100,%0000000011000001,%0010011100000000
	dc.w	%1100000100000100,%0000000001000001,%0000011100000000
	dc.w	%1100100100100100,%1111001001001001,%0000011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100000100100,%1000001001111000,%0010011100000000
	dc.w	%1100100000100000,%1000001000011000,%0010011100000000
	dc.w	%1100100111100000,%1001111000001001,%1110011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1000001001001001,%0010011100000000
	dc.w	%1100100100100100,%1000001001000000,%0010011100000000
	dc.w	%1100100100100100,%1111001001000000,%0010011100000000
	dc.w	%1100100100100100,%0011001001001001,%0010011100000000
	dc.w	%1100000100000100,%0001001001001001,%0010011100000000
	dc.w	%1100001100000100,%1001001001001001,%0010011100000000
	dc.w	%1100111100111100,%1001000001001001,%0010011100000000
	dc.w	%1100100100100100,%1001000011001001,%0010011100000000
	dc.w	%1100100100100100,%1001001111001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100111,%1001001001001001,%1110011100000000
	dc.w	%1100100000100000,%0000000000001000,%0000011100000000
	dc.w	%1100100000100000,%0000000000001100,%0000011100000000
	dc.w	%1111111111111111,%1111111111111111,%0011111100000000	end
	ds.w	12
bsmap6	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100100100000110,%0000000001000001,%0000011100000000
	dc.w	%1100000100000100,%0000000001000001,%0000011100000000
	dc.w	%1100000100100100,%1001111001001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100000111100,%1000001000001000,%0010011100000000
	dc.w	%1100100000110000,%1000001000001000,%0010011100000000
	dc.w	%1100111100100000,%1111001001001111,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1000001001001001,%0010011100000000
	dc.w	%1100100000000100,%1000001001001001,%0010011100000000
	dc.w	%1100100000000100,%1001111001001001,%0010011100000000
	dc.w	%1100100100100100,%1001100001001001,%0010011100000000
	dc.w	%1100100100100100,%1001000001000001,%0000011100000000
	dc.w	%1100100100100100,%1001001001000001,%1000011100000000
	dc.w	%1100100100100100,%0001001001111001,%1110011100000000
	dc.w	%1100100100100110,%0001001001001001,%0010011100000000
	dc.w	%1100100100100111,%1001001001001001,%0010011100000000
	dc.w	%1100100100100100,%1001001001001001,%0010011100000000
	dc.w	%1100111100100100,%1001001111001001,%0010011100000000
	dc.w	%1100000000100000,%0000000000001000,%0010011100000000
	dc.w	%1100000001100000,%0000000000001000,%0010011100000000
	dc.w	%1111100111111111,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap7	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111111111,%1001111111111111,%1111001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100111111100,%1111111111111111,%1001001100000000
	dc.w	%1100100110000100,%0000000000000000,%1001001100000000
	dc.w	%1100100100000100,%0000000000000000,%1001001100000000
	dc.w	%1100100100100111,%1111111111111100,%1001001100000000
	dc.w	%1100100100100100,%0000000000000000,%1001001100000000
	dc.w	%1100100100100100,%0000000000000000,%1001001100000000
	dc.w	%1100100100100111,%1111111111111100,%1001001100000000
	dc.w	%1100100100100000,%0000000000000000,%1001001100000000
	dc.w	%1100100100100000,%0000000000000000,%1001001100000000
	dc.w	%1100100100100111,%1111111111111100,%1001001100000000
	dc.w	%1100100100100000,%0000000000000000,%1001001100000000
	dc.w	%1100100100100000,%0000000000000001,%1001001100000000
	dc.w	%1100100100111111,%1111111111111001,%1001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100111100111111,%1111111111111111,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1111111100111111,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap8	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111111111,%1111100111111111,%1111001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100111111111,%1111111100111111,%1001001100000000
	dc.w	%1100100100000000,%0000000000100001,%1001001100000000
	dc.w	%1100100100000000,%0000000000100000,%1001001100000000
	dc.w	%1100100100111111,%1111111111100100,%1001001100000000
	dc.w	%1100100100000000,%0000000000100100,%1001001100000000
	dc.w	%1100100100000000,%0000000000100100,%1001001100000000
	dc.w	%1100100100111111,%1111111111100100,%1001001100000000
	dc.w	%1100100100000000,%0000000000000100,%1001001100000000
	dc.w	%1100100100000000,%0000000000000100,%1001001100000000
	dc.w	%1100100100111111,%1111111111100100,%1001001100000000
	dc.w	%1100100100000000,%0000000000000100,%1001001100000000
	dc.w	%1100100110000000,%0000000000000100,%1001001100000000
	dc.w	%1100100110011111,%1111111111111100,%1001001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100111111111111,%1111111111111100,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1110000000000000,%0000000000000000,%0000011100000000
	dc.w	%1111111111111111,%1111111111111100,%1111111100000000	end
	ds.w	12
bsmap9	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000001111,%0000000000000000,%0000011100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111000000,%0011100111111111,%0011001100000000
	dc.w	%1100000001111111,%0010100110000001,%0011001100000000
	dc.w	%1100000000000000,%0010100100000001,%0011001100000000
	dc.w	%1100111000000000,%0010100100111001,%0011001100000000
	dc.w	%1100001111111111,%1110100000001001,%0000001100000000
	dc.w	%1110000000000001,%0000100000001001,%0000001100000000
	dc.w	%1111000000000001,%1111100111111001,%1111001100000000
	dc.w	%1101001111111000,%0000000100000000,%0001001100000000
	dc.w	%1101001000001000,%0000000100000000,%0001001100000000
	dc.w	%1101001000001110,%0111111100111111,%1111001100000000
	dc.w	%1111001111000000,%0000100000000000,%0000001100000000
	dc.w	%1100000011000000,%0000100000000000,%0000001100000000
	dc.w	%1100000001111111,%1100111100100111,%1111111100000000
	dc.w	%1100111000000001,%0100100000100000,%0000001100000000
	dc.w	%1100101000000001,%0100100000100000,%0000001100000000
	dc.w	%1100101001001001,%1100100111100100,%1111001100000000
	dc.w	%1100101001001000,%0000000100000100,%0001001100000000
	dc.w	%1100101001001100,%0000001100001100,%0001001100000000
	dc.w	%1100111001001111,%1111111100111111,%1111001100000000
	dc.w	%1100000001000000,%0000000000000000,%0000001100000000
	dc.w	%1100000001000000,%0000000000000000,%0000011100000000
	dc.w	%1111111111111111,%0011111111111111,%1111111100000000	end
	ds.w	12
bsmap10	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1110000000000000,%0000000011110000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100110011111111,%1001110000000011,%1111001100000000
	dc.w	%1100110010000001,%1001010011111110,%0000001100000000
	dc.w	%1100110010000000,%1001010000000000,%0000001100000000
	dc.w	%1100110010011100,%1001010000000000,%0111001100000000
	dc.w	%1100000010010000,%0001011111111111,%1100001100000000
	dc.w	%1100000010010000,%0001000010000000,%0000011100000000
	dc.w	%1100111110011111,%1001111110000000,%0000111100000000
	dc.w	%1100100000000000,%1000000000011111,%1100101100000000
	dc.w	%1100100000000000,%1000000000010000,%0100101100000000
	dc.w	%1100111111111100,%1111111001110000,%0100101100000000
	dc.w	%1100000000000000,%0001000000000011,%1100111100000000
	dc.w	%1100000000000000,%0001000000000011,%0000001100000000
	dc.w	%1111111111100100,%1111001111111110,%0000001100000000
	dc.w	%1100000000000100,%0001001010000000,%0111001100000000
	dc.w	%1100000000000100,%0001001010000000,%0101001100000000
	dc.w	%1100111100100111,%1001001110010010,%0101001100000000
	dc.w	%1100100000100000,%1000000000010010,%0101001100000000
	dc.w	%1100100000110000,%1100000000110010,%0101001100000000
	dc.w	%1100111111111100,%1111111111110010,%0111001100000000
	dc.w	%1100000000000000,%0000000000000010,%0000001100000000
	dc.w	%1110000000000000,%0000000000000010,%0000001100000000
	dc.w	%1111111111111111,%1111110011111111,%1111111100000000	end
	ds.w	12
bsmap11	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000001110,%0000000000000000,%0000001100000000
	dc.w	%1100000000001100,%0000000000000000,%0000001100000000
	dc.w	%1111110011001100,%1111111111001111,%1111001100000000
	dc.w	%1100000001001100,%0000000001000000,%0001001100000000
	dc.w	%1100000000000000,%0000000001000000,%0000001100000000
	dc.w	%1100111000000000,%1111001001001111,%1000001100000000
	dc.w	%1100001001111100,%0001001001000000,%0001001100000000
	dc.w	%1100001000000100,%0000001001000000,%0011001100000000
	dc.w	%1111001000000100,%1000001000001001,%0011001100000000
	dc.w	%1100000001100100,%1001001000001001,%0010001100000000
	dc.w	%1100000001100100,%0001001001001001,%0000011100000000
	dc.w	%1111001001100100,%0001001111001001,%0000011100000000
	dc.w	%1111000000000000,%1000000000000001,%0010011100000000
	dc.w	%1111000000000001,%1000000000000011,%0010011100000000
	dc.w	%1111001111001111,%1001001111110011,%0010011100000000
	dc.w	%1100000011000000,%1000000001000000,%0000001100000000
	dc.w	%1100000001000000,%1000000001000000,%0000001100000000
	dc.w	%1100111001001100,%1111111001001001,%1111001100000000
	dc.w	%1100001000001000,%0000001001001000,%0000001100000000
	dc.w	%1100001000001000,%0000001001001000,%0000001100000000
	dc.w	%1100111001001111,%1110011001001111,%1111001100000000
	dc.w	%1100000001000000,%0000000001000000,%0000001100000000
	dc.w	%1100000001100000,%0000000011100000,%0000001100000000
	dc.w	%1111111111111001,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap12	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000001110000,%0000001100000000
	dc.w	%1100000000000000,%0000000000110000,%0000001100000000
	dc.w	%1100111111110011,%1111111100110011,%0011111100000000
	dc.w	%1100100000000010,%0000000000110010,%0000001100000000
	dc.w	%1100000000000010,%0000000000000000,%0000001100000000
	dc.w	%1100000111110010,%0100111100000000,%0111001100000000
	dc.w	%1100100000000010,%0100100000111110,%0100001100000000
	dc.w	%1100110000000010,%0100000000100000,%0100001100000000
	dc.w	%1100110010010000,%0100000100100000,%0100111100000000
	dc.w	%1100010010010000,%0100100100100110,%0000001100000000
	dc.w	%1110000010010010,%0100100000100110,%0000001100000000
	dc.w	%1110000010010011,%1100100000100110,%0100111100000000
	dc.w	%1110010010000000,%0000000100000000,%0000111100000000
	dc.w	%1110010011000000,%0000000110000000,%0000111100000000
	dc.w	%1110010011001111,%1100100111110011,%1100111100000000
	dc.w	%1100000000000010,%0000000100000011,%0000001100000000
	dc.w	%1100000000000010,%0000000100000010,%0000001100000000
	dc.w	%1100111110010010,%0111111100110010,%0111001100000000
	dc.w	%1100000000010010,%0100000000010000,%0100001100000000
	dc.w	%1100000000010010,%0100000000010000,%0100001100000000
	dc.w	%1100111111110010,%0110011111110010,%0111001100000000
	dc.w	%1100000000000010,%0000000000000010,%0000001100000000
	dc.w	%1100000000000111,%0000000000000110,%0000001100000000
	dc.w	%1111111111111111,%1111111110011111,%1111111100000000	end
	ds.w	12
bsmap13	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111111111,%1111111111111111,%1111101100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1101111111111111,%1111111111111111,%1111001100000000
	dc.w	%1100000010000000,%0000000000000001,%0000001100000000
	dc.w	%1100000010000000,%0000000000000001,%0000001100000000
	dc.w	%1111110010011111,%0011110011111001,%0011111100000000
	dc.w	%1100000000010001,%0010010010001000,%0000001100000000
	dc.w	%1100000000110001,%0010010010001000,%0000001100000000
	dc.w	%1100111111110011,%0010010011001111,%1111001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100111111100,%1110011100111111,%1001001100000000
	dc.w	%1100000100000100,%1000000100100001,%1000001100000000
	dc.w	%1100000100000100,%1000000100100000,%1000001100000000
	dc.w	%1100111100100100,%1001100100100100,%1111001100000000
	dc.w	%1100000000100100,%1000000100100100,%0000001100000000
	dc.w	%1100000000100000,%1000000100000100,%0000001100000000
	dc.w	%1100111100100000,%1001100100000100,%1111001100000000
	dc.w	%1100000000100100,%0001100000100100,%0000001100000000
	dc.w	%1100000001100100,%0011110000100100,%0000011100000000
	dc.w	%1111100111111111,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap14	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1101111111111111,%1111111111111111,%1111001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100000000000000,%0000000000000000,%0000001100000000
	dc.w	%1100111111111111,%1111111111111111,%1111101100000000
	dc.w	%1100000010000000,%0000000000000001,%0000001100000000
	dc.w	%1100000010000000,%0000000000000001,%0000001100000000
	dc.w	%1111110010011111,%0011110011111001,%0011111100000000
	dc.w	%1100000000010001,%0010010010001000,%0000001100000000
	dc.w	%1100000000010001,%0010010010001100,%0000001100000000
	dc.w	%1100111111110011,%0010010011001111,%1111001100000000
	dc.w	%1100110000000000,%0000000000000000,%0011001100000000
	dc.w	%1100100000000000,%0000000000000000,%0001001100000000
	dc.w	%1100100111111100,%1110011100111111,%1001001100000000
	dc.w	%1100000110000100,%1000000100100000,%1000001100000000
	dc.w	%1100000100000100,%1000000100100000,%1000001100000000
	dc.w	%1100111100100100,%1001100100100100,%1111001100000000
	dc.w	%1100000000100100,%1000000100100100,%0000001100000000
	dc.w	%1100000000100000,%1000000100000100,%0000001100000000
	dc.w	%1100111100100000,%1001100100000100,%1111001100000000
	dc.w	%1100000000100100,%0001100000100100,%0000001100000000
	dc.w	%1110000000100100,%0011110000100110,%0000001100000000
	dc.w	%1111111111111111,%1111111111111111,%1001111100000000	end
	ds.w	12
bsmap15	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000100100000,%0000000001000000,%0000001100000000
	dc.w	%1100000100100000,%0000000001000000,%0000001100000000
	dc.w	%1100111100100111,%1001111001001001,%1111001100000000
	dc.w	%1100000000000000,%0001100000001000,%0000001100000000
	dc.w	%1100000000000000,%0001000000001000,%0000001100000000
	dc.w	%1100111100111100,%1001001001111001,%0010011100000000
	dc.w	%1100100000100000,%1000000000000001,%0010011100000000
	dc.w	%1100100001100000,%1000000000000001,%0010011100000000
	dc.w	%1100100111100100,%1111001111111111,%0010011100000000
	dc.w	%1100000000000100,%0000001000000000,%0000001100000000
	dc.w	%1100000000000100,%0000001000000000,%0000001100000000
	dc.w	%1111100111100100,%1001001001001111,%1111001100000000
	dc.w	%1100000100100000,%1001000001000000,%0001001100000000
	dc.w	%1100000100100000,%1001000001000000,%0001001100000000
	dc.w	%1100100100100111,%1001111001001111,%1001111100000000
	dc.w	%1100100000100000,%0001000001001000,%0000001100000000
	dc.w	%1100100000100000,%0001000011001000,%0000001100000000
	dc.w	%1100100111100111,%1001001111111001,%1111001100000000
	dc.w	%1100000100000100,%0001000000000001,%0000001100000000
	dc.w	%1100000100000100,%0011000000000011,%0000001100000000
	dc.w	%1100100111111100,%1111001111001111,%0011001100000000
	dc.w	%1100100000000000,%0000001000000000,%0010001100000000
	dc.w	%1100110000000000,%0000001000000000,%0010001100000000
	dc.w	%1111111100111111,%1111111111111111,%1111111100000000	end
	ds.w	12
bsmap16	ds.w	15
	dc.w	%1111111111111111,%1111111111111111,%1111111100000000	start
	dc.w	%1100000000000010,%0000000000000100,%1000001100000000
	dc.w	%1100000000000010,%0000000000000100,%1000001100000000
	dc.w	%1100111110010010,%0111100111100100,%1111001100000000
	dc.w	%1100000000010000,%0001100000000000,%0000001100000000
	dc.w	%1100000000010000,%0000100000000000,%0000001100000000
	dc.w	%1110010010011110,%0100100100111100,%1111001100000000
	dc.w	%1110010010000000,%0000000100000100,%0001001100000000
	dc.w	%1110010010000000,%0000000100000110,%0001001100000000
	dc.w	%1110010011111111,%1100111100100111,%1001001100000000
	dc.w	%1100000000000000,%0100000000100000,%0000001100000000
	dc.w	%1100000000000000,%0100000000100000,%0000001100000000
	dc.w	%1100111111110010,%0100100100100111,%1001111100000000
	dc.w	%1100100000000010,%0000100100000100,%1000001100000000
	dc.w	%1100100000000010,%0000100100000100,%1000001100000000
	dc.w	%1111100111110010,%0111100111100100,%1001001100000000
	dc.w	%1100000000010010,%0000100000000100,%0001001100000000
	dc.w	%1100000000010011,%0000100000000100,%0001001100000000
	dc.w	%1100111110011111,%1100100111100111,%1001001100000000
	dc.w	%1100000010000000,%0000100000100000,%1000001100000000
	dc.w	%1100000011000000,%0000110000100000,%1000001100000000
	dc.w	%1100110011110011,%1100111100111111,%1001001100000000
	dc.w	%1100010000000000,%0100000000000000,%0001001100000000
	dc.w	%1100010000000000,%0100000000000000,%0011001100000000
	dc.w	%1111111111111111,%1111111111111100,%1111111100000000	end
	ds.w	12
*****************************************************************************
minimap	ds.l	1
number	ds.l	1
numbak	ds.l	1
stpos	ds.l	3
bonpos	ds.l	16
xplist	ds.l	25
sphero	ds.l	31
alien0	ds.l	31
alien1	ds.l	31
alien2	ds.l	31
alien3	ds.l	31
alien4	ds.l	31
alien5	ds.l	31
alien6	ds.l	31
alien7	ds.l	31
alien8	ds.l	31
alien9	ds.l	31
blktype	ds.l	1
blkcol	ds.w	3
cycle	ds.w	1
taliens	ds.w	1
*****************************************************************************
level1	dc.l	lvmap1,1800,1800,12,6720,$00010060
	dc.l	bon1,0,$40,3680,bon2,0,$f0,3680,bon3,0,$30,6880,bon4,0,$100,6880
	dc.l	$00400050,$00000001,$0a602b07,hwsp1,clisthw,clists
	dc.l	$009000b0,$00000002,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00f00100,$00000004,$0ab82b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,6732,6732,192,0,0,3-1,$00600001,6720,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5298,5298,160,0,0,3-1,$00900001,5280,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5300,5300,160,0,0,3-1,$00a00001,5280,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,8492,8492,160,0,0,3-1,$00600001,8480,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,8494,8494,160,0,0,3-1,$00700001,8480,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,8504,8504,160,0,0,3-1,$00c00001,8480,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,8506,8506,160,0,0,3-1,$00d00001,8480,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	UNUSED
	UNUSED
	UNUSED
	UNUSED
	dc.l	block1
	dc.w	$fe0,$ea0,$666,$0700,6
level2	dc.l	lvmap2,3000,3000,19,8320,$00010098
	dc.l	bon1,0,$30,2080,bon2,0,$100,2080,bon3,0,$70,6880,bon4,0,$c0,6880
	dc.l	$00380078,$00000001,$0a682b07,hwsp1,clisthw,clists
	dc.l	$00c80108,$00000001,$0ab02b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,8338,8338,192,0,$80000000,3-1,$00980001,8320,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2090,2090,160,0,0,3-1,$00580001,2080,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2108,2108,160,0,0,3-1,$00e00001,2080,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5292,5292,160,0,$80000000,3-1,$00680001,5280,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5296,5296,160,0,0,3-1,$00800001,5280,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5302,5302,160,0,0,3-1,$00b00001,5280,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5304,5304,160,0,$80000000,3-1,$00c80001,5280,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6886,6886,160,0,$80000000,3-1,$00380001,6880,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6910,6910,160,0,$80000000,3-1,$00f80001,6880,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,8482,8482,160,0,0,3-1,$00100001,8480,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,8516,8516,160,0,0,3-1,$01200001,8480,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	block2
	dc.w	$fdd,$f66,$b00,$0300,10
level3	dc.l	lvmap3,3000,3000,19,3520,$00010098
	dc.l	bon1,0,$40,6040,bon2,0,$f0,6040,bon3,0,$40,8480,bon4,0,$f0,8480
	dc.l	$00100020,$00000004,$0a482b07,hwsp1,clisthw,clists
	dc.l	$009800a8,$00000010,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$01200130,$00000008,$0ad02b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,3538,3538,192,0,$80000000,3-1,$00980001,3520,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000800,$ff220000,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2082,2082,160,0,$80000000,3-1,$00180001,2080,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2114,2114,160,0,$80000000,3-1,$01180001,2080,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3686,3686,160,0,$80000000,3-1,$00380001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3710,3710,160,0,$80000000,3-1,$00f80001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5290,5290,160,0,$80000000,3-1,$00580001,5280,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5306,5306,160,0,$80000000,3-1,$00d80001,5280,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6896,6896,160,0,0,3-1,$00800002,6880,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6898,6898,160,0,0,3-1,$00900002,6880,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6900,6900,160,0,0,3-1,$00a00002,6880,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6902,6902,160,0,0,3-1,$00b00002,6880,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	block3
	dc.w	$eee,$888,$008,$1c00,10
level4	dc.l	lvmap4,2400,2400,19,7680,$00010098
	dc.l	bon1,0,$20,2480,bon2,0,$110,2480,bon3,0,$10,6880,bon4,0,$120,6880
	dc.l	$00180038,$00000004,$0a502b07,hwsp1,clisthw,clists
	dc.l	$00680088,$00000010,$0a782b07,hwsp2,clisthw+12,clists+16
	dc.l	$00b800d8,$00000010,$0aa02b07,hwsp3,clisthw+24,clists+32
	dc.l	$01080128,$00000004,$0ac82b07,hwsp4,clisthw+36,clists+48
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,6798,6798,192,0,$80000000,3-1,$00980001,7680,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000400,$ff220000,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2088,2088,160,0,$80000000,3-1,$00480001,2080,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2098,2098,160,0,$80000000,3-1,$00980001,2080,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2098,2098,160,0,$80000000,3-1,$00980001,2080,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2108,2108,160,0,$80000000,3-1,$00e80001,2080,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3694,3694,160,0,$80000000,3-1,$00700001,3680,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3704,3704,160,0,$80000000,3-1,$00c00001,3680,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5288,5288,160,0,0,3-1,$00480001,5280,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5308,5308,160,0,0,3-1,$00e80001,5280,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	UNUSED
	UNUSED
	dc.l	block6
	dc.w	$7ef,$f38,$47f,$1400,8
level5	dc.l	lvmap5,3000,3000,19,5120,$00010098
	dc.l	bon1,0,$10,3680,bon2,0,$120,3680,bon3,0,$60,5280,bon4,0,$d0,5280
	dc.l	$00380108,$00000008,$0a8c2b07,hwsp1,clisthw,clists
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,5138,5138,192,0,$80000000,3-1,$00980001,5120,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000400,$ff220000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2096,2096,160,0,$80000000,3-1,$00880001,2080,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2100,2100,160,0,$80000000,3-1,$00a80001,2080,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3688,3688,160,0,0,3-1,$00400001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3694,3694,160,0,0,3-1,$00700001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3704,3704,160,0,0,3-1,$00c00001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3710,3710,160,0,0,3-1,$00f00001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6886,6886,160,0,$80000000,3-1,$00380001,6880,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6894,6894,160,0,0,3-1,$00700001,6880,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6904,6904,160,0,0,3-1,$00c00001,6880,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6910,6910,160,0,$80000000,3-1,$00f80001,6880,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	block8
	dc.w	$777,$fa0,$eee,$0e00,10
level6	dc.l	lvmap6,3000,3000,19,4800,$00010098
	dc.l	bon1,0,$20,5280,bon2,0,$110,5280,bon3,0,$10,6880,bon4,0,$120,6880
	dc.l	$00200120,$00000008,$0a8c2b07,hwsp1,clisthw,clists
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,4818,4818,192,0,$80000000,3-1,$00980001,4800,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00001000,$ff220000,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2090,2090,160,0,0,3-1,$00500001,2080,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2108,2108,160,0,0,3-1,$00e00001,2080,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3688,3688,160,0,0,3-1,$00400001,3680,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3710,3710,160,0,0,3-1,$00f00001,3680,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5286,5286,160,0,0,3-1,$00300001,5280,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5312,5312,160,0,0,3-1,$01000001,5280,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6884,6884,160,0,0,3-1,$00200001,6880,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6914,6914,160,0,0,3-1,$01100001,6880,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,8482,8482,160,0,0,3-1,$00100001,8480,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,8516,8516,160,0,0,3-1,$01200001,8480,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	block16
	dc.w	$0d3,$77f,$e55,$1800,10
level7	dc.l	lvmap7,3000,3000,5,6720,$00010028
	dc.l	bon1,0,$20,2080,bon2,0,$110,2080,bon3,0,$40,8480,bon4,0,$f0,8480
	dc.l	$00680088,$00000008,$0a782b07,hwsp1,clisthw,clists
	dc.l	$009000b0,$00000002,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00b800d8,$00000008,$0aa02b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,6724,6724,192,0,$80000000,3-1,$00280001,6720,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2096,2096,160,0,$80000000,3-1,$00880002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2100,2100,160,0,$80000000,3-1,$00a80002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3690,3690,160,0,0,3-1,$00500001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3696,3696,160,0,0,3-1,$00800001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3702,3702,160,0,0,3-1,$00b00001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3708,3708,160,0,0,3-1,$00e00001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5288,5288,160,0,0,3-1,$00400002,5280,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5294,5294,160,0,0,3-1,$00700002,5280,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5304,5304,160,0,0,3-1,$00c00002,5280,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5310,5310,160,0,0,3-1,$00f00002,5280,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	block5
	dc.w	$cb8,$41f,$f75,$0a00,10
level8	dc.l	lvmap8,2400,2400,19,8320,$00010098
	dc.l	bon1,0,$70,5280,bon2,0,$c0,5280,bon3,0,$50,6880,bon4,0,$e0,6880
	dc.l	$00500080,$00000008,$0a702b07,hwsp1,clisthw,clists
	dc.l	$00c000f0,$00000008,$0aa82b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,8338,8338,192,0,$80000000,3-1,$00980001,8320,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00001000,$ff220000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3688,3688,160,0,0,3-1,$00400001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3692,3692,160,0,0,3-1,$00600001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3702,3702,160,0,0,3-1,$00d00001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3710,3710,160,0,0,3-1,$00f00001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5284,5284,160,0,$80000000,3-1,$00280001,5280,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5312,5312,160,0,$80000000,3-1,$01080001,5280,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,8482,8482,160,0,0,3-1,$00100002,8480,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,8516,8516,160,0,0,3-1,$01200002,8480,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	UNUSED
	UNUSED
	dc.l	block15
	dc.w	$ccc,$a72,$666,$1a00,8
level9	dc.l	lvmap9,2400,2400,19,1920,$00010098
	dc.l	bon1,0,$60,2080,bon2,0,$d0,2080,bon3,0,$20,6880,bon4,0,$110,6880
	dc.l	$00300060,$00000001,$0a602b07,hwsp1,clisthw,clists
	dc.l	$008000c0,$00000008,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00e00110,$00000002,$0ab82b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,1938,1938,192,0,$80000000,3-1,$00980001,1920,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2084,2084,160,0,$80000000,3-1,$00280002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2112,2112,160,0,$80000000,3-1,$01080002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3682,3682,160,0,0,3-1,$00100002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3716,3716,160,0,0,3-1,$01200002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5296,5296,160,0,$80000000,3-1,$00880001,5280,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5300,5300,160,0,$80000000,3-1,$00a80001,5280,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6892,6892,160,0,0,3-1,$00600001,6880,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6906,6906,160,0,0,3-1,$00d00001,6880,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	UNUSED
	UNUSED
	dc.l	block19
	dc.w	$ffc,$09f,$d00,$0b00,8
level10	dc.l	lvmap10,2400,2400,19,8000,$00010098
	dc.l	bon1,0,$30,3360,bon2,0,$100,3360,bon3,0,$10,6560,bon4,0,$120,6560
	dc.l	$00300090,$00000010,$0a6c2b07,hwsp1,clisthw,clists
	dc.l	$00b00110,$00000010,$0aac2b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,8018,8018,192,0,$80000000,3-1,$00980001,8000,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,1774,1774,160,0,0,3-1,$00700001,1760,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,1784,1784,160,0,0,3-1,$00c00001,1760,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3370,3370,160,0,0,3-1,$00500001,3360,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3388,3388,160,0,0,3-1,$00e00001,3360,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,4974,4974,160,0,$80000000,3-1,$00780001,4960,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,4982,4982,160,0,$80000000,3-1,$00b80001,4960,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6564,6564,160,0,0,3-1,$00200001,6560,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6594,6594,160,0,0,3-1,$01100001,6560,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	UNUSED
	UNUSED
	dc.l	block20
	dc.w	$08f,$444,$fb0,$1400,8
level11	dc.l	lvmap11,2400,2400,19,7360,$00010098
	dc.l	bon1,0,$30,2720,bon2,0,$100,2720,bon3,0,$40,6880,bon4,0,$f0,6880
	dc.l	$00400050,$00000001,$0a602b07,hwsp1,clisthw,clists
	dc.l	$00700080,$00000002,$0a782b07,hwsp2,clisthw+12,clists+16
	dc.l	$00c000d0,$00000002,$0aa02b07,hwsp3,clisthw+24,clists+32
	dc.l	$00f00100,$00000001,$0ab82b07,hwsp4,clisthw+36,clists+48
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,7378,7378,192,0,$80000000,3-1,$00980001,7360,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000400,$ff220000,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3694,3694,160,0,$80000000,3-1,$00780001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3702,3702,160,0,$80000000,3-1,$00b80001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3694,3694,160,0,$80000000,3-1,$00780001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3702,3702,160,0,$80000000,3-1,$00b80001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,4324,4324,160,0,$80000000,3-1,$00280001,4320,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,4352,4352,160,0,$80000000,3-1,$01080001,4320,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5296,5296,160,0,$80000000,3-1,$00880001,5280,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5300,5300,160,0,$80000000,3-1,$00a80001,5280,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	UNUSED
	UNUSED
	dc.l	block11
	dc.w	$0fc,$f8f,$090,$0700,8
level12	dc.l	lvmap12,2400,2400,19,3520,$00010098
	dc.l	bon1,0,$50,2080,bon2,0,$e0,2080,bon3,0,$20,5280,bon4,0,$110,5280
	dc.l	$00380058,$00000004,$0a602b07,hwsp1,clisthw,clists
	dc.l	$008000c0,$00000010,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00e80108,$00000004,$0ab82b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,3538,3538,192,0,$80000000,3-1,$00980001,3520,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000400,$ff220000,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2098,2098,160,0,0,3-1,$00900001,2080,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2100,2100,160,0,0,3-1,$00a00001,2080,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2404,2404,160,0,$80000000,3-1,$00280001,2400,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2432,2432,160,0,$80000000,3-1,$01080001,2400,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5290,5290,160,0,0,3-1,$00500001,5280,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5308,5308,160,0,0,3-1,$00e00001,5280,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6886,6886,160,0,0,3-1,$00380001,6880,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6910,6910,160,0,0,3-1,$00f80001,6880,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	UNUSED
	UNUSED
	dc.l	block22
	dc.w	$ae3,$f66,$01f,$1600,8
level13	dc.l	lvmap13,2400,2400,19,8320,$00010098
	dc.l	bon1,0,$10,5280,bon2,0,$120,5280,bon3,0,$30,8480,bon4,0,$100,8480
	dc.l	$00400060,$00000001,$0a642b07,hwsp1,clisthw,clists
	dc.l	$00e00100,$00000002,$0ab42b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,8338,8338,192,0,$80000000,3-1,$00980001,8320,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000100,$ff220000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3688,3688,160,0,0,3-1,$00400001,3680,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3710,3710,160,0,0,3-1,$00f00001,3680,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3696,3696,160,0,0,3-1,$00800001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3702,3702,160,0,0,3-1,$00b00001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5296,5296,160,0,0,3-1,$00800001,5280,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5302,5302,160,0,0,3-1,$00b00001,5280,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6892,6892,160,0,0,3-1,$00600001,6880,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6906,6906,160,0,0,3-1,$00d00001,6880,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	UNUSED
	UNUSED
	dc.l	block7
	dc.w	$fff,$c00,$7cf,$0700,8
level14	dc.l	lvmap14,2400,2400,19,5120,$00010098
	dc.l	bon1,0,$40,2080,bon2,0,$f0,2080,bon3,0,$10,8480,bon4,0,$120,8480
	dc.l	$00300050,$00000004,$0a5c2b07,hwsp1,clisthw,clists
	dc.l	$00700090,$00000008,$0a7c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00b000d0,$00000002,$0a9c2b07,hwsp3,clisthw+24,clists+32
	dc.l	$00f00110,$00000001,$0abc2b07,hwsp4,clisthw+36,clists+48
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,5138,5138,192,0,$80000000,3-1,$00980001,5120,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3690,3690,160,0,0,3-1,$00500002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3692,3692,160,0,0,3-1,$00600001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3694,3694,160,0,0,3-1,$00700001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3696,3696,160,0,0,3-1,$00800001,3680,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3702,3702,160,0,0,3-1,$00b00001,3680,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3704,3704,160,0,0,3-1,$00c00001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3706,3706,160,0,0,3-1,$00d00001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3708,3708,160,0,0,3-1,$00e00002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	UNUSED
	UNUSED
	dc.l	block4
	dc.w	$fff,$8df,$0af,$0f00,8
level15	dc.l	lvmap15,2400,2400,19,6720,$00010098
	dc.l	bon1,0,$50,2080,bon2,0,$e0,2080,bon3,0,$10,5280,bon4,0,$120,5280
	dc.l	$00300110,$00000002,$0a8c2b07,hwsp1,clisthw,clists
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,6738,6738,192,0,$80000000,3-1,$00980001,6720,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000800,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2082,2082,160,0,0,3-1,$00100002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2116,2116,160,0,0,3-1,$01200002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3698,3698,160,0,0,3-1,$00900001,3680,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3700,3700,160,0,0,3-1,$00a00001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5298,5298,160,0,0,3-1,$00900001,5280,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5300,5300,160,0,0,3-1,$00a00001,5280,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0	
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6886,6886,160,0,$80000000,3-1,$00380002,6880,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6910,6910,160,0,$80000000,3-1,$00f80002,6880,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	UNUSED
	UNUSED
	dc.l	block14
	dc.w	$0ce,$ee0,$00f,$0a00,8
level16	dc.l	lvmap16,3000,3000,19,8320,$00010098
	dc.l	bon1,0,$90,2080,bon2,0,$a0,2080,bon3,0,$10,6880,bon4,0,$120,6880
	dc.l	$00200090,$00000010,$0a682b07,hwsp1,clisthw,clists
	dc.l	$00b00120,$00000008,$0ab02b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,8338,8338,192,0,$80000000,3-1,$00980001,8320,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00001000,$ff220000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3682,3682,160,0,0,3-1,$00100001,3680,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3716,3716,160,0,0,3-1,$01200001,3680,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3692,3692,160,0,$80000000,3-1,$00680001,3680,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3704,3704,160,0,$80000000,3-1,$00c80001,3680,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5284,5284,160,0,$80000000,3-1,$00280002,5280,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5312,5312,160,0,$80000000,3-1,$01080002,5280,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5288,5288,160,0,$80000000,3-1,$00480001,5280,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5308,5308,160,0,$80000000,3-1,$00e80001,5280,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6892,6892,160,0,$80000000,3-1,$00680001,6880,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6904,6904,160,0,$80000000,3-1,$00c80001,6880,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	block18
	dc.w	$06c,$db6,$700,$1c00,10
level17	dc.l	lvmap17,2400,2400,2,5120,$00010010
	dc.l	bon1,0,$70,5280,bon2,0,$c0,5280,bon3,0,$30,8480,bon4,0,$100,8480
	dc.l	$00200030,$00000001,$0a502b07,hwsp1,clisthw,clists
	dc.l	$004800f8,$00000002,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$01100120,$00000004,$0ac82b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,5122,5122,192,0,0,3-1,$00100001,5120,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,1764,1764,160,0,$80000000,3-1,$00280001,1760,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,1792,1792,160,0,$80000000,3-1,$01080001,1760,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3694,3694,160,0,0,3-1,$00700001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3704,3704,160,0,0,3-1,$00c00001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5288,5288,160,0,0,3-1,$00400001,5280,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5310,5310,160,0,0,3-1,$00f00001,5280,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6896,6896,160,0,$80000000,3-1,$00880001,6880,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6900,6900,160,0,$80000000,3-1,$00a80001,6880,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	UNUSED
	UNUSED
	dc.l	block9
	dc.w	$eee,$f80,$99f,$0700,8
level18	dc.l	lvmap18,3000,3000,19,8320,$00010098
	dc.l	bon1,0,$30,6880,bon2,0,$100,6880,bon3,0,$20,8480,bon4,0,$110,8480
	dc.l	$00300050,$00000010,$0a5c2b07,hwsp1,clisthw,clists
	dc.l	$00700090,$00000008,$0a7c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00b000d0,$00000008,$0a9c2b07,hwsp3,clisthw+24,clists+32
	dc.l	$00f00110,$00000010,$0abc2b07,hwsp4,clisthw+36,clists+48
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,8338,8338,192,0,$80000000,3-1,$00980001,8320,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00001000,$ff220000,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2094,2094,160,0,0,3-1,$00700001,2080,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2104,2104,160,0,0,3-1,$00c00001,2080,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3686,3686,160,0,0,3-1,$00300002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3712,3712,160,0,0,3-1,$01000002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5294,5294,160,0,0,3-1,$00700001,5280,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5304,5304,160,0,0,3-1,$00c00001,5280,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5296,5296,160,0,0,3-1,$00800001,5280,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5302,5302,160,0,0,3-1,$00b00001,5280,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6888,6888,160,0,0,3-1,$00400002,6880,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6910,6910,160,0,0,3-1,$00f00002,6880,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	block24
	dc.w	$dff,$b66,$77f,$1800,10
level19	dc.l	lvmap19,3000,3000,19,5120,$00010098
	dc.l	bon1,0,$50,3680,bon2,0,$e0,3680,bon3,0,$30,5280,bon4,0,$100,5280
	dc.l	$00400068,$00000010,$0a662b07,hwsp1,clisthw,clists
	dc.l	$008800b8,$00000008,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$00d80100,$00000010,$0ab22b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero,hero+576,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0602,5138,5138,192,0,$80000000,3-1,$00980001,5120,40,anlhero
	dc.l	$00010001,anlhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00001000,$ff220000,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2084,2084,160,0,0,3-1,$00200001,2080,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2114,2114,160,0,0,3-1,$01100001,2080,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2096,2096,160,0,$80000000,3-1,$00880002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2100,2100,160,0,$80000000,3-1,$00a80002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3698,3698,160,0,0,3-1,$00900002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3700,3700,160,0,0,3-1,$00a00002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5292,5292,160,0,0,3-1,$00600001,5280,40,anlalnl
	dc.l	$00030003,anlalnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	light,light+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5306,5306,160,0,0,3-1,$00d00001,5280,40,anralnl
	dc.l	$00030003,anralnl+16,$00030003,$00011616,anlalnl,anralnl
	dc.l	anlalnl+16,anralnl+16,$00001000,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6884,6884,160,0,0,3-1,$00200002,6880,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6914,6914,160,0,0,3-1,$01100002,6880,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	block12
	dc.w	$777,$fa0,$eee,$1800,10
level20	dc.l	lvmap20,2400,2400,19,3520,$00010098
	dc.l	bon1,0,$80,3680,bon2,0,$b0,3680,bon3,0,$20,8480,bon4,0,$110,8480
	dc.l	$00580068,$00000008,$0a6c2b07,hwsp1,clisthw,clists
	dc.l	$00700080,$00000001,$0a782b07,hwsp2,clisthw+12,clists+16
	dc.l	$00c000d0,$00000001,$0aa02b07,hwsp3,clisthw+24,clists+32
	dc.l	$00d800e8,$00000008,$0aac2b07,hwsp4,clisthw+36,clists+48
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,3538,3538,192,0,$80000000,3-1,$00980001,3520,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2090,2090,160,0,$80000000,3-1,$00580002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2426,2426,160,0,$80000000,3-1,$00d80002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2094,2094,160,0,0,3-1,$00700002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2104,2104,160,0,0,3-1,$00c00002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3370,3370,160,0,$80000000,3-1,$00580001,3360,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3396,3396,160,0,0,3-1,$01200001,3360,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5296,5296,160,0,0,3-1,$00800001,5280,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5302,5302,160,0,0,3-1,$00b00001,5280,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	UNUSED
	UNUSED
	dc.l	block23
	dc.w	$dcf,$000,$23f,$0b00,8
level21	dc.l	lvmap21,1800,1800,19,1920,$00010098
	dc.l	bon1,0,$10,6880,bon2,0,$120,6880,bon3,0,$50,6880,bon4,0,$e0,6880
	dc.l	$00400050,$00000008,$0a602b07,hwsp1,clisthw,clists
	dc.l	$00f00100,$00000002,$0ab82b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,1938,1938,192,0,$80000000,3-1,$00980001,1920,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000100,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3682,3682,160,0,$80000000,3-1,$00180002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3714,3714,160,0,$80000000,3-1,$01180002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5292,5292,160,0,$80000000,3-1,$00680001,5280,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5304,5304,160,0,$80000000,3-1,$00c80001,5280,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6896,6896,160,0,$80000000,3-1,$00880001,6880,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6900,6900,160,0,$80000000,3-1,$00a80001,6880,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	UNUSED
	UNUSED
	UNUSED
	UNUSED
	dc.l	block21
	dc.w	$297,$ff6,$fa7,$0b00,6
level22	dc.l	lvmap22,3000,3000,19,2240,$00010098
	dc.l	bon1,0,$10,3680,bon2,0,$120,3680,bon3,0,$30,6880,bon4,0,$100,6880
	dc.l	$00100020,$00000001,$0a482b07,hwsp1,clisthw,clists
	dc.l	$00600090,$00000004,$0a782b07,hwsp2,clisthw+12,clists+16
	dc.l	$00b000e0,$00000002,$0aa02b07,hwsp3,clisthw+24,clists+32
	dc.l	$01200130,$00000008,$0ad02b07,hwsp4,clisthw+36,clists+48
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,2258,2258,192,0,$80000000,3-1,$00980001,2240,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2414,2414,160,0,0,3-1,$00700001,2400,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2424,2424,160,0,0,3-1,$00c00001,2400,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3690,3690,160,0,$80000000,3-1,$00580001,3680,40,anralne
	dc.l	$00030003,anralne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	elec,elec+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3706,3706,160,0,$80000000,3-1,$00d80001,3680,40,anlalne
	dc.l	$00030003,anlalne+16,$000a000a,$00051616,anlalne,anralne
	dc.l	anlalne+16,anralne+16,$00000400,0,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,4966,4966,160,0,$80000000,3-1,$00380001,4960,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,4990,4990,160,0,$80000000,3-1,$00f80001,4960,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,4974,4974,160,0,$80000000,3-1,$00780002,4960,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,4982,4982,160,0,$80000000,3-1,$00b80002,4960,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,6890,6890,160,0,$80000000,3-1,$00580001,6880,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,6906,6906,160,0,$80000000,3-1,$00d80001,6880,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	block13
	dc.w	$55e,$d7f,$dd0,$0f00,10
level23	dc.l	lvmap23,3000,3000,19,1920,$00010098
	dc.l	bon1,0,$40,2080,bon2,0,$f0,2080,bon3,0,$40,5280,bon4,0,$f0,3680
	dc.l	$00300040,$00000001,$0a582b07,hwsp1,clisthw,clists
	dc.l	$007000d0,$00000002,$0a8c2b07,hwsp2,clisthw+12,clists+16
	dc.l	$01000110,$00000001,$0ac02b07,hwsp3,clisthw+24,clists+32
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,1938,1938,192,0,$80000000,3-1,$00980001,1920,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000200,$ff220000,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2096,2096,160,0,0,3-1,$00800002,2080,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2102,2102,160,0,0,3-1,$00b00002,2080,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3682,3682,160,0,0,3-1,$00100002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3716,3716,160,0,0,3-1,$01200002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,4978,4978,160,0,$80000000,3-1,$00980001,4960,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,4978,4978,160,0,$80000000,3-1,$00980001,4960,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,4978,4978,160,0,$80000000,3-1,$00980001,4960,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,4978,4978,160,0,$80000000,3-1,$00980001,4960,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,8170,8170,160,0,0,3-1,$00500001,8160,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,8188,8188,160,0,0,3-1,$00e00001,8160,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	block17
	dc.w	$fbc,$a96,$851,$0b00,10
level24	dc.l	lvmap24,3000,3000,19,6720,$00010098
	dc.l	bon1,0,$50,5280,bon2,0,$e0,5280,bon3,0,$30,8480,bon4,0,$100,8480
	dc.l	$00500060,$00000008,$0a682b07,hwsp1,clisthw,clists
	dc.l	$00e000f0,$00000002,$0ab02b07,hwsp2,clisthw+12,clists+16
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff,$ffffffff
	dc.l	$ffffffff
	dc.l	hero+2,hero+578,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010602,6738,6738,192,0,$80000000,3-1,$00980001,6720,40,anrhero
	dc.l	$00010001,anrhero+8,$00030003,$00081616,anlhero,anrhero
	dc.l	anlhero+8,anrhero+8,$00000100,$ff220000,0,0
	dc.l	stonr,stonr+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2084,2084,160,0,$80000000,3-1,$00280001,2080,40,anralns
	dc.l	$00030003,anralns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	stonl,stonl+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,2112,2112,160,0,$80000000,3-1,$01080001,2080,40,anlalns
	dc.l	$00030003,anlalns+16,$00090009,$00011616,anlalns,anralns
	dc.l	anlalns+16,anralns+16,$01010100,$ff000000,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,2098,2098,160,0,$80000000,3-1,$00980001,2080,40,anralni
	dc.l	$00030003,anralni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	ice,ice+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3698,3698,160,0,$80000000,3-1,$00980001,3680,40,anlalni
	dc.l	$00030003,anlalni+16,$00080008,$00031616,anlalni,anralni
	dc.l	anlalni+16,anralni+16,$00000200,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3686,3686,160,0,0,3-1,$00300002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,3688,3688,160,0,$80000000,3-1,$00480002,3680,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3708,3708,160,0,$80000000,3-1,$00e80002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,3712,3712,160,0,0,3-1,$01000002,3680,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$ffff0502,5296,5296,160,0,$80000000,3-1,$00880002,5280,40,anlalnf
	dc.l	$00030003,anlalnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	fire,fire+480,dummy,dummy,$ffff0000,$00040024,$00240004,$0fca0000
	dc.l	$00010502,5300,5300,160,0,$80000000,3-1,$00a80002,5280,40,anralnf
	dc.l	$00030003,anralnf+16,$00030003,$00031616,anlalnf,anralnf
	dc.l	anlalnf+16,anralnf+16,$00000800,0,0,0
	dc.l	block10
	dc.w	$480,$eee,$b20,$0b00,10
*****************************************************************************
finish	dc.l	0
ypos	dc.w	0
table	dc.l	250000
	dc.b	"-*------*-",0,0
	dc.l	225000
	dc.b	" MATCH    ",0,0
	dc.l	200000
	dc.b	"    PATCH ",0,0
	dc.l	175000
	dc.b	"-*------*-",0,0
	dc.l	150000
	dc.b	" PROGRAM  ",0,0
	dc.l	125000
	dc.b	" CODE,    ",0,0
	dc.l	100000
	dc.b	" GRAPHICS,",0,0
	dc.l	75000
	dc.b	" SOUND   ",0,0
	dc.l	50000
	dc.b	"    BY    ",0,0
	dc.l	25000
	dc.b	"    SD.   ",0,0
	dc.l	0
	dc.b	"..........",0,0
	even
gap	dc.b	"  ",0
	even
*****************************************************************************
	SECTION	platform1,DATA_C

oldcop	dc.l	0
offscr	dc.l	$29fe2d00,0,0,0,0,0,0,0,0,0,0
splist	dc.l	offscr,offscr,offscr,offscr,offscr,offscr,offscr,offscr
dummy	dc.l	0

tlist	dc.w	bpl1pth,0,bpl1ptl,0,bpl2pth,0,bpl2ptl,0,bpl3pth,0,bpl3ptl,0
	dc.w	diwstrt,$2981,diwstop,$29c1,bplcon0,$3200,bplcon1,0,bplcon2,0
	dc.w	color00,$0000,color01,$0eee,color02,$0666,color03,$00ce
	dc.w	color04,$0f10,color05,$0ec0,color06,$00b0,color07,$000c
tlists	dc.w	spr0pth,0,spr0ptl,0,spr1pth,0,spr1ptl,0,spr2pth,0,spr2ptl,0
	dc.w	spr3pth,0,spr3ptl,0,spr4pth,0,spr4ptl,0,spr5pth,0,spr5ptl,0
	dc.w	spr6pth,0,spr6ptl,0,spr7pth,0,spr7ptl,0
	dc.w	intreq,$8010,$ffff,$fffe
* sets up 320*256*3 screen to display title

clist	dc.w	bpl1pth,0,bpl1ptl,0,bpl2pth,0,bpl2ptl,0,bpl3pth,0,bpl3ptl,0
	dc.w	diwstrt,$2c81,diwstop,$63c1,bplcon0,$3200,bplcon1,0,bplcon2,0
	dc.w	color00,$0000,color01,$0e10,color02,$0666,color03,$00b0
	dc.w	color04,$0eee,color05,$0ec0,color06,$000c,color07,$00ce
clisthw	dc.w	color17,$0000,color18,$0000,color19,$0000,color21,$0000
	dc.w	color22,$0000,color23,$0000,color25,$0000,color26,$0000
	dc.w	color27,$0000,color29,$0000,color30,$0000,color31,$0000
clists	dc.w	spr0pth,0,spr0ptl,0,spr1pth,0,spr1ptl,0,spr2pth,0,spr2ptl,0
	dc.w	spr3pth,0,spr3ptl,0,spr4pth,0,spr4ptl,0,spr5pth,0,spr5ptl,0
	dc.w	spr6pth,0,spr6ptl,0,spr7pth,0,spr7ptl,0
	dc.w	$6401,$fffe
clistn	dc.w	bpl1pth,0,bpl1ptl,0,bpl2pth,0,bpl2ptl,0,bpl3pth,0,bpl3ptl,0
	dc.w	bpl4pth,0,bpl4ptl,0,bpl5pth,0,bpl5ptl,0,diwstrt,$6481
	dc.w	diwstop,$2cc1,bplcon0,$5600,bplcon1,0,bplcon2,$0020
clistc	dc.w	color00,$0000,color01,$0eee,color02,$0666,color03,$00ce
	dc.w	color04,$0f10,color05,$0ec0,color06,$00b0,color07,$000c
	dc.w	color08,$0f0f,color09,$0777,color10,$0bb0,color11,$00f0
	dc.w	color12,$0f0f,color13,$0777,color14,$0bb0,color15,$00f0
clistp	dc.w	intreq,$8010,$ffff,$fffe
* sets up 320*30*3 panel and a 320*256*2 background/320*256*3 foreground dual
* playfield double buffered display
block1	INCBIN	"code/blocks"
block2	equ	block1+2
block3	equ	block2+2
block4	equ	block3+2
block5	equ	block4+2
block6	equ	block5+2
block7	equ	block6+2
block8	equ	block7+2
block9	equ	block1+176
block10	equ	block9+2
block11	equ	block10+2
block12	equ	block11+2
block13	equ	block12+2
block14	equ	block13+2
block15	equ	block14+2
block16	equ	block15+2
block17	equ	block9+176
block18	equ	block17+2
block19	equ	block18+2
block20	equ	block19+2
block21	equ	block20+2
block22	equ	block21+2
block23	equ	block22+2
block24	equ	block23+2
bkbmsks	dc.w	$ff80,$ffc0,$ffe0,$ffe0,$ffe0,$ffe0,$ffe0,$ffe0,$ffe0,$7fe0,$3fe0
bkbmsk	dc.w	$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$ff00,$0000,$0000,$0000
hero	INCBIN	"code/hero" 
bald	INCBIN	"code/bald"
stonl	INCBIN	"code/siefl"
stonr	equ	stonl+640
ice	equ	stonr+640
elec	equ	ice+640
fire	equ	elec+640
light	equ	fire+640
stonf	INCBIN	"code/fades"
icef	equ	stonf+640
elecf	equ	icef+640
firef	equ	elecf+640
lightf	equ	firef+640
shotl	INCBIN	"code/shotl"
shotr	INCBIN	"code/shotr"
hwsp1	INCBIN	"code/hwsp1"
hwsp2	INCBIN	"code/hwsp2"
hwsp3	INCBIN	"code/hwsp3"
hwsp4	INCBIN	"code/hwsp4"
digitl	INCBIN	"code/digitl"
digits	INCBIN	"code/digits"
pointer	INCBIN	"code/pointer"
order	INCBIN	"code/order"
para	INCBIN	"code/para"
dying	INCBIN	"code/dying"
bonus	INCBIN	"code/bonus"
text	INCBIN	"code/text"
title	INCBIN	"code/title"
info	INCBIN	"code/infoscreen"
hitm	INCBIN	"code/music/hitmusic"		1196	352
changem	INCBIN	"code/music/changemusic"	8060	352
bonusm	INCBIN	"code/music/bonusmusic"		16236	352
firem	INCBIN	"code/music/firemusic"		2436	352
deadm	INCBIN	"code/music/deadmusic"		3730	352
extram	INCBIN	"code/music/extramusic"		3688	352
heromd	dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$7fff0000
heroml	dc.l	0
	dcb.l	22,$80000000
	dc.l	0
heromr	dc.l	0
	dcb.l	22,$00004000
	dc.l	0
alienmd	dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$7fff0000,0,0,0
alienml	dc.l	0
	dcb.l	19,$80000000
	dc.l	0,0,0,0
alienmr	dc.l	0
	dcb.l	19,$00004000
	dc.l	0,0,0,0
alienbl	dc.l	0,0
	dcb.l	17,$80000000
	dc.l	0,0,0,0,0
alienbr	dc.l	0,0
	dcb.l	17,$00004000
	dc.l	0,0,0,0,0
shotmt	dc.l	0,$7fff0000,$7fff0000,$7fff0000,$7fff0000,$7fff0000,$7fff0000,$7fff0000
	ds.l	16
baldml	dc.l	0,0
	dcb.l	14,$80000000
	dc.l	0,0,0,0,0,0,0,0,0
baldmr	dc.l	0,0
	dcb.l	14,$00004000
	dc.l	0,0,0,0,0,0,0,0,0
baldmd	dc.l	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$3fff0000,0,0,0,0,0,0
baldmu	dc.l	$3fff0000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
scr0a	INCBIN	"code/panel"		panel
scr0b	equ	scr0a+2240
scr0c	equ	scr0b+2240
*****************************************************************************
	SECTION	platform2,BSS_C
scr1	ds.b	width*(top+high+bot)		game screen 1
scr2	ds.b	width*(top+high+bot)
scr3	ds.b	width*(top+high+bot)
scr1a	ds.b	width*(top+high+bot)		game screen 2
scr2a	ds.b	width*(top+high+bot)
scr3a	ds.b	width*(top+high+bot)
scr4	ds.b	8000				background screen
	ds.b	40*6				spurious graphics buffer
scr5	ds.b	8000
	ds.b	40*6				spurious graphics buffer
scr6	ds.b	(width*(top+high+bot+8))	platform mask+1 blank line

hsstpos	equ	scr4			screen start addr.
	END





