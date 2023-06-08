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

#ifndef __YMMUSIC__
#define __YMMUSIC__

#include "YmTypes.h"
#include "StSoundLibrary.h"
#include "Ym2149ExBL.h"
#include "YmLoad.h"
#include "digidrum.h"

#define MAX_DIGIDRUM 128

#define YMTPREC 16
#define MAX_VOICE 8

typedef enum
{
	YM_V2,
	YM_V3,
	YM_V4,
	YM_V5,
	YM_V6,
	YM_VMAX,

	YM_TRACKER1 = 32,
	YM_TRACKER2,
	YM_TRACKERMAX,

	YM_MIX1 = 64,
	YM_MIX2,
	YM_MIXMAX,
} ymFile_t;

typedef struct
{
	ymu32 sampleStart;
	ymu32 sampleLength;
	ymu16 nbRepeat;
	ymu16 replayFreq;
} mixBlock_t;

typedef struct
{
	ymu32 size;
	ymu8 *pData;
	ymu32 repLen;
} digiDrum_t;

typedef struct
{
	ymint nbVoice;
	ymu32 nbVbl;
	ymu8 *pDataBufer;
	ymu32 currentVbl;
	ymu32 flags;
	ymbool bLoop;
} ymTrackerPartoche_t;

typedef struct
{
	ymu8 *pSample;
	ymu32 sampleSize;
	ymu32 samplePos;
	ymu32 repLen;
	yms32 sampleVolume;
	ymu32 sampleFreq;
	ymbool bLoop;
	ymbool bRunning;
} ymTrackerVoice_t;

typedef struct
{
	ymu8 noteOn;
	ymu8 volume;
	ymu8 freqHigh;
	ymu8 freqLow;
} ymTrackerLine_t;

enum
{
	A_STREAMINTERLEAVED = 1,
	A_DRUMSIGNED = 2,
	A_DRUM4BITS = 4,
	A_TIMECONTROL = 8,
	A_LOOPMODE = 16,
};

class CYmMusic
{

public:
	CYmMusic(volatile ymint *_ymChipBaseAddr, ymint _replayRate = 44100);
	~CYmMusic();

	ymbool load(const char *pName);
	ymbool loadMemory(void *pBlock, ymu32 size);

	void unLoad(void);
	ymbool isSeekable(void);
	ymu32 getPos(void);
	ymu32 getMusicTime(void);
	ymu32 setMusicTime(ymu32 time);
	void restart(void);
	void play(void);
	void pause(void);
	void stop(void);
	void setVolume(ymint volume);
	ymint getAttrib(void);
	void getMusicInfo(ymMusicInfo_t *pInfo);
	void setLoopMode(ymbool bLoop);
	const char *getLastError(void);
	int readYmRegister(ymint reg) { return ymChip.readRegister(reg); }
	void setLowpassFilter(ymbool bActive) { ymChip.setFilter(bActive); }

	void player(void);

	ymbool getMusicOver(void) const { return (bMusicOver); }
	ymint GetNbFrame() const { return nbFrame; }
	ymint GetStreamInc() const { return streamInc; }
	const ymu8 *GetDataStream() const { return pDataStream; }

	//-------------------------------------------------------------
	// WAVE Generator
	//-------------------------------------------------------------
	int waveCreate(char *fName);

	ymbool bMusicOver;

private:
	ymbool checkCompilerTypes();

	void setPlayerRate(ymint rate);
	void setAttrib(ymint _attrib);
	void setLastError(const char *pError);
	ymu8 *depackFile(ymu32 size);
	ymbool deInterleave(void);
	void setTimeControl(ymbool bFlag);

	CYm2149Ex ymChip;
	const char *pLastError;
	ymFile_t songType;
	int nbFrame;
	int loopFrame;
	int currentFrame;
	int nbDrum;
	digiDrum_t *pDrumTab;
	int musicTime;
	ymu8 *pBigMalloc;
	ymu8 *pDataStream;
	ymbool bLoop;
	ymint fileSize;
	ymbool ymDecode(void);
	ymint playerRate;
	ymint attrib;
	volatile ymbool bMusicOk;
	volatile ymbool bPause;
	ymint streamInc;
	ymint replayRate;

	ymchar *pSongName;
	ymchar *pSongAuthor;
	ymchar *pSongComment;
	ymchar *pSongType;
	ymchar *pSongPlayer;
};

/*
		int	version;
		SD	pos;
		UD	inc;
		UD	timeSec;
		UD	timeMin;
		UD	loopSec;
		UD	loopMin;
		UD	nbVbl;
		UD	vblRestart;
		UD	ymFreq;
		UD	playerFreq;
		UB	*pRegister;
		UB	*pFileBuffer;
		UD	fileSize;
		UD	attrib;
		char *pSongName;
		char *pSongComment;
		char *pSongAuthor;
		mixBlock_t *pMixBlock;
		long	nbMixBlock;
		UD	nbDrum;
		digiDrum_t *pDrumTab;
		int	nbVoice;
		ymu32 currentVbl;
		int	bTimeControl;
		int	timeTotal;
		int	ymtFreqShift;
*/

#endif
