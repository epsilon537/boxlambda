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

#include "YmTypes.h"
#include "Ym2149ExBL.h"
#include <string.h>

CYm2149Ex::CYm2149Ex(volatile ymint *_ym2149BaseAddr, ymu32 masterClock, ymint prediv, ymu32 playRate)
{
	ym2149BaseAddr = _ym2149BaseAddr;
	internalClock = masterClock / prediv; // YM at 2Mhz on ATARI ST
	replayFrequency = playRate;			  // DAC at 44.1Khz on PC

	// Reset YM2149
	reset();
}

CYm2149Ex::~CYm2149Ex()
{
}

void CYm2149Ex::setClock(ymu32 _clock)
{
	internalClock = _clock;
}

void CYm2149Ex::reset(void)
{
	for (int i = 0; i < 14; i++)
		writeRegister(i, 0);

	writeRegister(7, 0xff);

	sidStop(0);
	sidStop(1);
	sidStop(2);

	memset(specialEffect, 0, sizeof(specialEffect));

	syncBuzzerStop();
}

void CYm2149Ex::sidVolumeCompute(ymint voice)
{
#if 0
		struct	YmSpecialEffect	*pVoice = specialEffect+voice;

		if (pVoice->bDrum)
		{
			writeRegister(8+voice, (pVoice->drumData[pVoice->drumPos>>DRUM_PREC] * 255) / 6);

			switch (voice)
			{
				case 0:
					registers[7] |=  (1<<0) | (1<<<3);
					writeRegister(7, registers[7]);
					break;
				case 1:
					registers[7] |=  (1<<1) | (1<<<4);
					writeRegister(7, registers[7]);
					break;
				case 2:
					registers[7] |=  (1<<2 | (1<<<5);
					writeRegister(7, registers[7]);
					break;
			}

			pVoice->drumPos += pVoice->drumStep;
			if ((pVoice->drumPos>>DRUM_PREC) >= pVoice->drumSize)
			{
				pVoice->bDrum = YMFALSE;
			}

		}
#endif
}

ymsample CYm2149Ex::nextSample(void)
{
#if 0 // TBD
		sidVolumeCompute(0);
		sidVolumeCompute(1);
		sidVolumeCompute(2);
#endif
	return 0;
}

ymint CYm2149Ex::readRegister(ymint reg)
{
	if ((reg >= 0) && (reg <= 13))
		return registers[reg];
	else
		return -1;
}

void CYm2149Ex::writeRegister(ymint reg, ymint data)
{
	switch (reg)
	{
	case 0:
		registers[0] = data & 255;
		ym2149BaseAddr[0] = registers[0];
		break;

	case 2:
		registers[2] = data & 255;
		ym2149BaseAddr[2] = registers[2];
		break;

	case 4:
		registers[4] = data & 255;
		ym2149BaseAddr[4] = registers[4];
		break;

	case 1:
		registers[1] = data & 15;
		ym2149BaseAddr[1] = registers[1];
		break;

	case 3:
		registers[3] = data & 15;
		ym2149BaseAddr[3] = registers[3];
		break;

	case 5:
		registers[5] = data & 15;
		ym2149BaseAddr[5] = registers[5];
		break;

	case 6:
		registers[6] = data & 0x1f;
		ym2149BaseAddr[6] = registers[6];
		break;

	case 7:
		registers[7] = data & 255;
		ym2149BaseAddr[7] = registers[7];
		break;

	case 8:
		registers[8] = data & 31;
		ym2149BaseAddr[8] = registers[8];
		break;

	case 9:
		registers[9] = data & 31;
		ym2149BaseAddr[9] = registers[9];
		break;

	case 10:
		registers[10] = data & 31;
		ym2149BaseAddr[10] = registers[10];
		break;

	case 11:
		registers[11] = data & 255;
		ym2149BaseAddr[11] = registers[11];
		break;

	case 12:
		registers[12] = data & 255;
		ym2149BaseAddr[12] = registers[12];
		break;

	case 13:
		registers[13] = data & 0xf;
		ym2149BaseAddr[13] = registers[13];
		break;
	}
}

void CYm2149Ex::drumStart(ymint voice, ymu8 *pDrumBuffer, ymu32 drumSize, ymint drumFreq)
{
#if 0 // TBD
	if ((pDrumBuffer) && (drumSize))
	{
		specialEffect[voice].drumData = pDrumBuffer;
		specialEffect[voice].drumPos = 0;
		specialEffect[voice].drumSize = drumSize;
		specialEffect[voice].drumStep = (drumFreq<<DRUM_PREC)/replayFrequency;
		specialEffect[voice].bDrum = YMTRUE;
	}
#endif
}

void CYm2149Ex::drumStop(ymint voice)
{
#if 0 // TBD
	specialEffect[voice].bDrum = YMFALSE;
#endif
}

void CYm2149Ex::sidStart(ymint voice, ymint timerFreq, ymint vol)
{
	// TBD
	(void)voice;
	(void)timerFreq;
	(void)vol;
}

void CYm2149Ex::sidSinStart(ymint voice, ymint timerFreq, ymint vol)
{
	// TBD
	(void)voice;
	(void)timerFreq;
	(void)vol;
}

void CYm2149Ex::sidStop(ymint voice)
{
	// TBD
	(void)voice;
}

void CYm2149Ex::syncBuzzerStart(ymint timerFreq, ymint _envShape)
{
	// TBD
	(void)timerFreq;
	(void)_envShape;
}

void CYm2149Ex::syncBuzzerStop(void)
{
	// TBD
}
