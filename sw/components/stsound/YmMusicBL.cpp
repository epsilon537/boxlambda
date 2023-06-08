/*-----------------------------------------------------------------------------

	ST-Sound ( YM files player library )

	YM Music Driver

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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "YmMusicBL.h"

#define	_LINEAR_OVRS				// Activate linear oversampling (best quality) Only used for DigiMix and UniversalTracker YM file type


// ATARI-ST MFP chip predivisor
static	const ymint	mfpPrediv[8] = {0,4,10,16,50,64,100,200};



CYmMusic::CYmMusic(volatile ymint* _ymChipBaseAddr, ymint _replayRate) :
    ymChip(_ymChipBaseAddr, ATARI_CLOCK, 1, _replayRate)
{
	pBigMalloc = NULL;
	pSongName = NULL;
	pSongAuthor = NULL;
	pSongComment = NULL;
	pSongType = NULL;
	pSongPlayer = NULL;

	//pBigSampleBuffer = NULL;
	//pMixBlock = NULL;

	replayRate = _replayRate;
	//currentPos = 0;
	nbDrum = 0;
	pDrumTab = NULL;
	setLoopMode(YMFALSE);

	//m_pTimeInfo = NULL;
}

void	CYmMusic::setTimeControl(ymbool bTime)
{
		if (bTime)
			attrib |= A_TIMECONTROL;
		else
			attrib &= (~A_TIMECONTROL);
}

CYmMusic::~CYmMusic()
{
		stop();
		unLoad();
}

void	CYmMusic::setLoopMode(ymbool bLoopMode)
{
		bLoop = bLoopMode;
}

void	CYmMusic::setPlayerRate(ymint rate)
{
		playerRate = rate;
}

ymu32 CYmMusic::getPos()
{
	if ((nbFrame>0) && (playerRate>0))
	{
		return ((ymu32)currentFrame*1000)/(ymu32)playerRate;
	}
	else
		return 0;

}

ymu32	CYmMusic::getMusicTime(void)
{

	if ((nbFrame>0) && (playerRate>0))
	{
		return ((ymu32)nbFrame*1000)/(ymu32)playerRate;
	}
	else
		return 0;

}

ymu32	CYmMusic::setMusicTime(ymu32 time)
{
		if (!isSeekable()) return 0;
		ymu32 newTime = 0;

		if ((songType>=YM_V2) && (songType<YM_VMAX))
		{
			newTime = time;
			if (newTime>=getMusicTime()) newTime = 0;
			currentFrame = (newTime*(ymu32)playerRate)/1000;
		}
		
		return newTime;
}

void	CYmMusic::restart(void)
{
	setMusicTime(0);
	bMusicOver = YMFALSE;
}



void	CYmMusic::getMusicInfo(ymMusicInfo_t *pInfo)
{
		if (pInfo)
		{
			pInfo->pSongName = pSongName;
			pInfo->pSongAuthor = pSongAuthor;
			pInfo->pSongComment = pSongComment;
			pInfo->pSongType = pSongType;
			pInfo->pSongPlayer = pSongPlayer;

			pInfo->musicTimeInMs = getMusicTime();
			pInfo->musicTimeInSec = pInfo->musicTimeInMs / 1000;
		}
}


void	CYmMusic::setAttrib(ymint _attrib)
{
		attrib = _attrib;
}

ymint		CYmMusic::getAttrib(void)
{
		return attrib;
}

ymbool	CYmMusic::isSeekable(void)
{
		return getAttrib()&A_TIMECONTROL;
}

void	CYmMusic::setLastError(const char *pError)
{
		pLastError = pError;
}

const char	*CYmMusic::getLastError(void)
{
		return pLastError;
}

void	CYmMusic::setVolume(ymint volume)
{
//		ymChip.setGlobalVolume(volume);
}

void	CYmMusic::player(void)
 {
 ymu8	*ptr;
 ymu32 prediv;
 ymint voice;
 ymint ndrum;


	if (currentFrame<0) currentFrame = 0;

	if (currentFrame>=nbFrame)
	{
		if (bLoop)
		{
			currentFrame = loopFrame;
		}
		else
		{
			bMusicOver = YMTRUE;
			ymChip.reset();
			return;
		}
	}

	ptr = pDataStream+currentFrame*streamInc;

	for (ymint i=0;i<=10;i++)
		ymChip.writeRegister(i,ptr[i]);

	ymChip.sidStop(0);
	ymChip.sidStop(1);
	ymChip.sidStop(2);
	ymChip.syncBuzzerStop();

	//---------------------------------------------
	// Check digi-drum
	//---------------------------------------------
	if (songType == YM_V2)		// MADMAX specific !
	{
		if (ptr[13]!=0xff)
		{
			ymChip.writeRegister(11,ptr[11]);
			ymChip.writeRegister(12,0);
			ymChip.writeRegister(13,10);				// MADMAX specific !!
		}
		if (ptr[10]&0x80)					// bit 7 volume canal C pour annoncer une digi-drum madmax.
		{
			ymint	sampleNum;
			ymu32 sampleFrq;
			ymChip.writeRegister(7,ymChip.readRegister(7)|0x24)	;	// Coupe TONE + NOISE canal C.
			sampleNum = ptr[10]&0x7f;		// Numero du sample

#if 0 //TBD-FIXME
			if (ptr[12])
			{
				sampleFrq = ((MFP_CLOCK/4) / ptr[12]);
				ymChip.drumStart(	2,							// Voice C
									sampleAdress[sampleNum],
									sampleLen[sampleNum],
									sampleFrq);
			}
#endif
		}
	}
	else if (songType >= YM_V3)
	{
		ymChip.writeRegister(11,ptr[11]);
		ymChip.writeRegister(12,ptr[12]);
		if (ptr[13]!=0xff)
		{
			ymChip.writeRegister(13,ptr[13]);
		}
	}
	currentFrame++;
 }

/*

	x x x x x x x x	r0
	0 0 0 0 x x x x	r1		// Special FX 1a
	x x x x x x x x	r2
	0 0 0 0 x x x x r3		// Special FX 2a
	x x x x x x x x r4
	0 0 0 0 x x x x r5
	0 0 0 x x x x x r6		// Special FX 1b
	0 0 x x x x x x r7
	0 0 0 x x x x x r8		// Special FX 2b
	0 0 0 x x x x x r9
	0 0 0 x x x x x r10
	x x x x x x x x r11
	x x x x x x x x r12
	0 0 0 0 x x x x r13
	0 0 0 0 0 0 0 0 r14		// Special FX 1c
	0 0 0 0 0 0 0 0 r15		// Special FX 2c


  Special Fx ?a
	0 0 0 0	: No special FX running
	0 0 0 1 : Sid Voice A
	0 0 1 0 : Sid Voice B
	0 0 1 1 : Sid Voice C
	0 1 0 0 : Extended Fx voice A
	0 1 0 1 : Digidrum voice A
	0 1 1 0 : Digidrum voice B
	0 1 1 1 : Digidrum voice C
	1 0 0 0 : Extended Fx voice B
	1 0 0 1 : Sinus SID voice A
	1 0 1 0 : Sinus SID voice B
	1 0 1 1 : Sinus SID voice C
	1 1 0 0 : Extended Fx voice C
	1 1 0 1 : Sync Buzzer voice A
	1 1 1 0 : Sync Buzzer voice B
	1 1 1 1 : Sync Buzzer voice C



*/
