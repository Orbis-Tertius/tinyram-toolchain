/* -*- mode: C -*-
 *
 * Copyright (c) 2007-2011, 2013, 2014 The University of Utah
 * All rights reserved.
 *
 * This file is part of `csmith', a random generator of C programs.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */


#ifndef RANDOM_RUNTIME_H
#define RANDOM_RUNTIME_H

#include "runtime.h"

/*****************************************************************************/

#include <stdint.h>
#include <limits.h>
#include "safe_math_macros.h"

#if 0
static uint32_t crc32_tab[256] = {
	0,
	1996959894,
	3993919788,
	2567524794,
	124634137,
	1886057615,
	3915621685,
	2657392035,
	249268274,
	2044508324,
	3772115230,
	2547177864,
	162941995,
	2125561021,
	3887607047,
	2428444049,
	498536548,
	1789927666,
	4089016648,
	2227061214,
	450548861,
	1843258603,
	4107580753,
	2211677639,
	325883990,
	1684777152,
	4251122042,
	2321926636,
	335633487,
	1661365465,
	4195302755,
	2366115317,
	997073096,
	1281953886,
	3579855332,
	2724688242,
	1006888145,
	1258607687,
	3524101629,
	2768942443,
	901097722,
	1119000684,
	3686517206,
	2898065728,
	853044451,
	1172266101,
	3705015759,
	2882616665,
	651767980,
	1373503546,
	3369554304,
	3218104598,
	565507253,
	1454621731,
	3485111705,
	3099436303,
	671266974,
	1594198024,
	3322730930,
	2970347812,
	795835527,
	1483230225,
	3244367275,
	3060149565,
	1994146192,
	31158534,
	2563907772,
	4023717930,
	1907459465,
	112637215,
	2680153253,
	3904427059,
	2013776290,
	251722036,
	2517215374,
	3775830040,
	2137656763,
	141376813,
	2439277719,
	3865271297,
	1802195444,
	476864866,
	2238001368,
	4066508878,
	1812370925,
	453092731,
	2181625025,
	4111451223,
	1706088902,
	314042704,
	2344532202,
	4240017532,
	1658658271,
	366619977,
	2362670323,
	4224994405,
	1303535960,
	984961486,
	2747007092,
	3569037538,
	1256170817,
	1037604311,
	2765210733,
	3554079995,
	1131014506,
	879679996,
	2909243462,
	3663771856,
	1141124467,
	855842277,
	2852801631,
	3708648649,
	1342533948,
	654459306,
	3188396048,
	3373015174,
	1466479909,
	544179635,
	3110523913,
	3462522015,
	1591671054,
	702138776,
	2966460450,
	3352799412,
	1504918807,
	783551873,
	3082640443,
	3233442989,
	3988292384,
	2596254646,
	62317068,
	1957810842,
	3939845945,
	2647816111,
	81470997,
	1943803523,
	3814918930,
	2489596804,
	225274430,
	2053790376,
	3826175755,
	2466906013,
	167816743,
	2097651377,
	4027552580,
	2265490386,
	503444072,
	1762050814,
	4150417245,
	2154129355,
	426522225,
	1852507879,
	4275313526,
	2312317920,
	282753626,
	1742555852,
	4189708143,
	2394877945,
	397917763,
	1622183637,
	3604390888,
	2714866558,
	953729732,
	1340076626,
	3518719985,
	2797360999,
	1068828381,
	1219638859,
	3624741850,
	2936675148,
	906185462,
	1090812512,
	3747672003,
	2825379669,
	829329135,
	1181335161,
	3412177804,
	3160834842,
	628085408,
	1382605366,
	3423369109,
	3138078467,
	570562233,
	1426400815,
	3317316542,
	2998733608,
	733239954,
	1555261956,
	3268935591,
	3050360625,
	752459403,
	1541320221,
	2607071920,
	3965973030,
	1969922972,
	40735498,
	2617837225,
	3943577151,
	1913087877,
	83908371,
	2512341634,
	3803740692,
	2075208622,
	213261112,
	2463272603,
	3855990285,
	2094854071,
	198958881,
	2262029012,
	4057260610,
	1759359992,
	534414190,
	2176718541,
	4139329115,
	1873836001,
	414664567,
	2282248934,
	4279200368,
	1711684554,
	285281116,
	2405801727,
	4167216745,
	1634467795,
	376229701,
	2685067896,
	3608007406,
	1308918612,
	956543938,
	2808555105,
	3495958263,
	1231636301,
	1047427035,
	2932959818,
	3654703836,
	1088359270,
	936918000,
	2847714899,
	3736837829,
	1202900863,
	817233897,
	3183342108,
	3401237130,
	1404277552,
	615818150,
	3134207493,
	3453421203,
	1423857449,
	601450431,
	3009837614,
	3294710456,
	1567103746,
	711928724,
	3020668471,
	3272380065,
	1510334235,
	755167117
};
#endif

#if 0
static uint32_t crc32_context = 0xFFFFFFFFUL;
#endif

#define NOINLINE __attribute__((noinline))
//#define NOINLINE

static void
crc32_gentab (void)
{
#if 0
	uint32_t crc;
	const uint32_t poly = 0xEDB88320UL;
	int i, j;

	for (i = 0; i < 256; i++) {
		crc = i;
		for (j = 8; j > 0; j--) {
			if (crc & 1) {
				crc = (crc >> 1) ^ poly;
			} else {
				crc >>= 1;
			}
		}
		crc32_tab[i] = crc;
	}

	printf("!!!");
	for (int k = 0; k < 256; k++) {
          printf("%u,\r\n", crc32_tab[k]);
	}
	printf("!!!");
#endif
}

//NOINLINE
static void
crc32_byte (uint8_t b) {
#if 0
	crc32_context =
		((crc32_context >> 8) & 0x00FFFFFF) ^
		crc32_tab[(crc32_context ^ b) & 0xFF];
        //printf("crc32_byte: %lX\n", crc32_context);
#endif
}

//NOINLINE
static void
crc32_8bytes (uint64_t val)
{
	crc32_byte ((val>>0) & 0xff);
	crc32_byte ((val>>8) & 0xff);
	crc32_byte ((val>>16) & 0xff);
	crc32_byte ((val>>24) & 0xff);
	crc32_byte ((val>>32) & 0xff);
	crc32_byte ((val>>40) & 0xff);
	crc32_byte ((val>>48) & 0xff);
	crc32_byte ((val>>56) & 0xff);
}

NOINLINE
static void
transparent_crc (uint64_t val, char* vname, int flag)
{
        //printf("transparent_crc: %lX, %lX\n", (uint32_t)val, (uint32_t)(val >> 32));
	crc32_8bytes(val);
	if (flag) {
#if 0
		printf("...checksum after hashing %s : %lX\n", vname, crc32_context ^ 0xFFFFFFFFUL);
#endif
	}
}

//NOINLINE
static void
transparent_crc_bytes (char *ptr, int nbytes, char* vname, int flag){
    int i;
    for (i=0; i<nbytes; i++) {
        crc32_byte(ptr[i]);
    }
	if (flag) {
#if 0
		printf("...checksum after hashing %s : %lX\n", vname, crc32_context ^ 0xFFFFFFFFUL);
#endif
	}
}

#endif /* RANDOM_RUNTIME_H */
