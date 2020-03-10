#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sox.h>
#include "utils.h"

// ----------------------------------------------------------------------------
// Effects

typedef struct { sox_format_t * file; } eff_priv_t;

// -------------------------------------
// Input effect

static sox_uint64_t read_wide_samples = 0;
static double input_read_time = 0;

void
input_clear_globals(void)
{
    read_wide_samples = 0;
    input_read_time = 0;
}

static int
input_getopts(sox_effect_t * effp, int argc, char * * argv)
{
    input_clear_globals();

    eff_priv_t * p = (eff_priv_t *)effp->priv;

    if (argc != 2 ||
        !(p->file = (sox_format_t *)argv[1]) ||
        p->file->mode != 'r') return SOX_EOF;

    return SOX_SUCCESS;
}

static int
input_drain(sox_effect_t * effp, sox_sample_t * obuf, size_t * osamp)
{
    eff_priv_t * p = (eff_priv_t *)effp->priv;

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
        input_getopts, NULL, NULL, input_drain, NULL, NULL, sizeof(eff_priv_t)
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
// Output effect

static sox_sample_t global_omax[2], global_omin[2];

void
output_clear_globals(void)
{
    memset(global_omax, 0, sizeof(global_omax));
    memset(global_omin, 0, sizeof(global_omin));
}

static int
output_getopts(sox_effect_t * effp, int argc, char * * argv)
{
    output_clear_globals();

    eff_priv_t * p = (eff_priv_t *)effp->priv;
    if (argc != 2 ||
        !(p->file = (sox_format_t *)argv[1]) ||
        p->file->mode != 'w')
        return SOX_EOF;
    return SOX_SUCCESS;
}

static int
output_flow(
        sox_effect_t *effp, sox_sample_t const * ibuf,
        sox_sample_t * obuf, size_t * isamp, size_t * osamp)

{
  eff_priv_t * p = (eff_priv_t *)effp->priv;

  for (size_t i = 0; i < *isamp; i += effp->in_signal.channels) {
    global_omax[0] = max(global_omax[0], ibuf[i]);
    global_omin[0] = min(global_omin[0], ibuf[i]);
    if (effp->in_signal.channels > 1) {
      global_omax[1] = max(global_omax[1], ibuf[i + 1]);
      global_omin[1] = min(global_omin[1], ibuf[i + 1]);
    }
    else {
      global_omax[1] = global_omax[0];
      global_omin[1] = global_omin[0];
    }
  }

  /* Write out *isamp samples */
  size_t len = sox_write(p->file, ibuf, *isamp);

  /* len is the number of samples that were actually written out; if this is
   * different to *isamp, then something has gone wrong--most often, it's
   * out of disc space */
  if (len != *isamp) {
    lsx_fail("%s: %s", p->file->filename, p->file->sox_errstr);
    return SOX_EOF;
  }

  /* Outputting is the last `effect' in the effect chain so always passes
   * 0 samples on to the next effect (as there isn't one!) */
  (void)obuf, *osamp = 0;
  return SOX_SUCCESS; /* All samples output successfully */
}

sox_effect_handler_t const *
output_effect_fn0(void)
{
  static sox_effect_handler_t handler = {
    "output0", NULL, SOX_EFF_MCHAN | SOX_EFF_INTERNAL,
    output_getopts, NULL, output_flow, NULL, NULL, NULL, sizeof(eff_priv_t)
  };
  return &handler;
}

static char const *
vu(unsigned channel)
{
  static char const * const text[][2] = {
    /* White: 2dB steps */
    {"", ""}, {"-", "-"}, {"=", "="}, {"-=", "=-"},
    {"==", "=="}, {"-==", "==-"}, {"===", "==="}, {"-===", "===-"},
    {"====", "===="}, {"-====", "====-"}, {"=====", "====="},
    {"-=====", "=====-"}, {"======", "======"},
    /* Red: 1dB steps */
    {"!=====", "=====!"},
  };
  int const red = 1, white = array_length(text) - red;
  double const MAX = SOX_SAMPLE_MAX, MIN = SOX_SAMPLE_MIN;
  double linear = max(global_omax[channel] / MAX, global_omin[channel] / MIN);
  double dB = linear_to_dB(linear);
  int vu_dB = linear? floor(2 * white + red + dB) : 0;
  int index = vu_dB < 2 * white? max(vu_dB / 2, 0) : min(vu_dB - white, red + white - 1);
  global_omax[channel] = global_omin[channel] = 0;

  return text[index][channel];
}

char const *
get_vu_meter_fst(void)
{
    return vu(0);
}

char const *
get_vu_meter_snd(void)
{
    return vu(1);
}

// ----------------------------------------------------------------------------

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
