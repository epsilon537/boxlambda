/*-----------------------------------------------------------------------------

	ST-Sound ( YM files player library )

	BoxLambda wrapper class for Extended YM-2149 Emulator,
	with ATARI music demos effects.
	(SID-Like, Digidrum, Sync Buzzer, Sinus SID and Pattern SID)

	Original Ym2149Ex.cpp by Arnaud Carre, see copyright below.
	BoxLambda version by Ruben Lysens / Epsilon537.

-----------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
* ST-Sound, ATARI-ST Music Emulator
* Copyright (c) 1995-1999 Arnaud Carre ( http://leonard.oxg.free.fr )
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
* OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
* LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
* SUCH DAMAGE.
*
-----------------------------------------------------------------------------*/

#ifndef __YM2149BL__
#define __YM2149BL__

#define ATARI_CLOCK 2000000L
#define MFP_CLOCK 2457600L

enum
{
	VOICE_A = 0,
	VOICE_B = 1,
	VOICE_C = 2,
};

struct YmSpecialEffect
{

	ymbool bDrum;
	ymu32 drumSize;
	ymu8 *drumData;
	ymu32 drumPos;
	ymu32 drumStep;

	ymbool bSid;
	ymu32 sidPos;
	ymu32 sidStep;
	ymint sidVol;
};

class CYm2149Ex
{
public:
	CYm2149Ex(volatile ymint *_ym2149BaseAddr, ymu32 masterClock = ATARI_CLOCK, ymint prediv = 1, ymu32 playRate = 44100);
	~CYm2149Ex();

	void reset(void);

	void setClock(ymu32 _clock);
	void writeRegister(ymint reg, ymint value);
	ymint readRegister(ymint reg);
	void drumStart(ymint voice, ymu8 *drumBuffer, ymu32 drumSize, ymint drumFreq);
	void drumStop(ymint voice);
	void sidStart(ymint voice, ymint freq, ymint vol);
	void sidSinStart(ymint voice, ymint timerFreq, ymint sinPattern);
	void sidStop(ymint voice);
	void syncBuzzerStart(ymint freq, ymint envShape);
	void syncBuzzerStop(void);

	void setFilter(ymbool bFilter) { (void)bFilter; }

	ymsample nextSample(void);

private:
	void sidVolumeCompute(ymint voice);

	volatile ymint *ym2149BaseAddr;
	ymint replayFrequency;
	ymu32 internalClock;

	ymint registers[14];

	struct YmSpecialEffect specialEffect[3];
};

#endif
