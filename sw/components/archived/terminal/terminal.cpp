//This is a libtmt-based terminal emulator for BoxLambda. All output goes to
//the VGA display using VERA.

#include <stdio.h>
#include <stdlib.h>
#include "tmt.h"

/* Forward declaration of a callback.
 * libtmt will call this function when the terminal's state changes.
 */
void tmt_callback(tmt_msg_t m, TMT *vt, const void *a, void *p);

TMT* terminal_init(void)
{
    /* Open a virtual terminal with 2 lines and 10 columns.
     * The first NULL is just a pointer that will be provided to the
     * callback; it can be anything. The second NULL specifies that
     * we want to use the default Alternate Character Set; this
     * could be a pointer to a wide string that has the desired
     * characters to be displayed when in ACS mode.
     */
    TMT *vt = tmt_open(2, 10, callback, NULL, NULL);
    if (!vt)
        return perror("could not allocate terminal"), EXIT_FAILURE;

    return vt;
}

void terminal_close(TMT *vt) {
    tmt_close(vt);
}

void
tmt_callback(tmt_msg_t m, TMT *vt, const void *a, void *p)
{
    /* grab a pointer to the virtual screen */
    const TMTSCREEN *s = tmt_screen(vt);
    const TMTPOINT *c = tmt_cursor(vt);

    switch (m){
        case TMT_MSG_BELL:
            /* the terminal is requesting that we ring the bell/flash the
             * screen/do whatever ^G is supposed to do; a is NULL
             */
            printf("bing!\n");
            break;

        case TMT_MSG_UPDATE:
            /* the screen image changed; a is a pointer to the TMTSCREEN */
            for (size_t r = 0; r < s->nline; r++){
                if (s->lines[r]->dirty){
                    for (size_t c = 0; c < s->ncol; c++){
                        printf("contents of %zd,%zd: %lc (%s bold)\n", r, c,
                               s->lines[r]->chars[c].c,
                               s->lines[r]->chars[c].a.bold? "is" : "is not");
                    }
                }
            }

            /* let tmt know we've redrawn the screen */
            tmt_clean(vt);
            break;

        case TMT_MSG_ANSWER:
            /* the terminal has a response to give to the program; a is a
             * pointer to a string */
            printf("terminal answered %s\n", (const char *)a);
            break;

        case TMT_MSG_MOVED:
            /* the cursor moved; a is a pointer to the cursor's TMTPOINT */
            printf("cursor is now at %zd,%zd\n", c->r, c->c);
            break;
    }
}
Data Types and Enumerations
/* an opaque structure */
typedef struct TMT TMT;

/* possible messages sent to the callback */
typedef enum{
    TMT_MSG_MOVED,  /* the cursor changed position       */
    TMT_MSG_UPDATE, /* the screen image changed          */
    TMT_MSG_ANSWER, /* the terminal responded to a query */
    TMT_MSG_BELL    /* the terminal bell was rung        */
} tmt_msg_T;

/* a callback for the library
 * m is one of the message constants above
 * vt is a pointer to the vt structure
 * r is NULL for TMT_MSG_BELL
 *   is a pointer to the cursor's TMTPOINT for TMT_MSG_MOVED
 *   is a pointer to the terminal's TMTSCREEN for TMT_MSG_UPDATE
 *   is a pointer to a string for TMT_MSG_ANSWER
 * p is whatever was passed to tmt_open (see below).
 */
typedef void (*TMTCALLBACK)(tmt_msg_t m, struct TMT *vt,
                            const void *r, void *p);

/* color definitions */
typedef enum{
    TMT_COLOR_BLACK,
    TMT_COLOR_RED,
    TMT_COLOR_GREEN,
    TMT_COLOR_YELLOW,
    TMT_COLOR_BLUE,
    TMT_COLOR_MAGENTA,
    TMT_COLOR_CYAN,
    TMT_COLOR_WHITE,
    TMT_COLOR_DEFAULT /* whatever the host terminal wants it to mean */
} tmt_color_t;

/* graphical rendition */
typedef struct TMTATTRS TMTATTRS;
struct TMTATTRS{
    bool bold;      /* character is bold             */
    bool dim;       /* character is half-bright      */
    bool underline; /* character is underlined       */
    bool blink;     /* character is blinking         */
    bool reverse;   /* character is in reverse video */
    bool invisible; /* character is invisible        */
    tmt_color_t fg; /* character foreground color    */
    tmt_color_t bg; /* character background color    */
};

/* characters */
typedef struct TMTCHAR TMTCHAR;
struct TMTCHAR{
    wchar_t  c; /* the character */
    TMTATTRS a; /* its rendition */
};

/* a position on the screen; upper left corner is 0,0 */
typedef struct TMTPOINT TMTPOINT;
struct TMTPOINT{
    size_t r; /* row    */
    size_t c; /* column */
};

/* a line of characters on the screen;
 * every line is always as wide as the screen
 */
typedef struct TMTLINE TMTLINE;
struct TMTLINE{
    bool dirty;     /* line has changed since it was last drawn */
    TMTCHAR chars;  /* the contents of the line                 */
};

/* a virtual terminal screen image */
typedef struct TMTSCREEN TMTSCREEN;
struct TMTSCREEN{
    size_t nline;    /* number of rows          */
    size_t ncol;     /* number of columns       */
    TMTLINE **lines; /* the lines on the screen */
};
