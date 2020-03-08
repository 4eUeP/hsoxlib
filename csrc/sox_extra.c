#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sox.h>
#include "utils.h"

// -------------------------------------

static sox_uint64_t read_wide_samples = 0;
static double input_read_time = 0;

typedef struct { sox_format_t * file; } priv_t;

static int
input_getopts(sox_effect_t * effp, int argc, char * * argv)
{
    read_wide_samples = 0;
    input_read_time = 0;

    priv_t * p = (priv_t *)effp->priv;

    if (argc != 2 ||
        !(p->file = (sox_format_t *)argv[1]) ||
        p->file->mode != 'r') return SOX_EOF;

    return SOX_SUCCESS;
}

static int
input_drain(sox_effect_t * effp, sox_sample_t * obuf, size_t * osamp)
{
    priv_t * p = (priv_t *)effp->priv;

    /* ensure that *osamp is a multiple of the number of channels. */
    *osamp -= *osamp % effp->out_signal.channels;

    /* Read up to *osamp samples into obuf; store the actual number read
     * back to *osamp */
    *osamp = sox_read(p->file, obuf, *osamp);

    /* sox_read may return a number that is less than was requested; only if
     * 0 samples is returned does it indicate that end-of-file has been reached
     * or an error has occurred */
    if (!*osamp && p->file->sox_errno)
        lsx_fail("%s: %s", p->file->filename, p->file->sox_errstr);

    read_wide_samples += *osamp / effp->in_signal.channels;
    input_read_time = (double)read_wide_samples / effp->in_signal.rate;

    return *osamp? SOX_SUCCESS : SOX_EOF;
}

sox_effect_handler_t const *
input_effect_fn0(void)
{
    static sox_effect_handler_t handler = {
        "input0", NULL, SOX_EFF_MCHAN | SOX_EFF_LENGTH | SOX_EFF_INTERNAL,
        input_getopts, NULL, NULL, input_drain, NULL, NULL, sizeof(priv_t)
    };
    return &handler;
}

sox_uint64_t
get_read_wide_samples(void)
{
    return read_wide_samples;
}

double
get_input_read_time(void)
{
    return input_read_time;
}

// -------------------------------------

char const *
device_name(char const * const type)
{
    char * name = NULL, * from_env = getenv("AUDIODEV");

    if (!type)
        return NULL;

    if (0
        || !strcmp(type, "sunau")
        || !strcmp(type, "oss" )
        || !strcmp(type, "ossdsp")
        || !strcmp(type, "alsa")
        || !strcmp(type, "ao")
        || !strcmp(type, "sndio")
        || !strcmp(type, "coreaudio")
        || !strcmp(type, "pulseaudio")
        || !strcmp(type, "waveaudio")
        )
        name = "default";

    return name? from_env? from_env : name : NULL;
}

char const *
try_device(char const * name)
{
    sox_format_handler_t const * handler = sox_find_format(name, sox_false);
    if (handler) {
        sox_format_t format, * ft = &format;
        lsx_debug("Looking for a default device: trying format `%s'", name);
        memset(ft, 0, sizeof(*ft));
        ft->filename = (char *)device_name(name);
        ft->priv = lsx_calloc(1, handler->priv_size);
        if (handler->startwrite(ft) == SOX_SUCCESS) {
            handler->stopwrite(ft);
            free(ft->priv);
            return name;
        }
        free(ft->priv);
    }
    return NULL;
}
