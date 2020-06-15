/*
This is the c configuration file for the keymap

Copyright 2012 Jun Wako <wakojun@gmail.com>
Copyright 2015 Jack Humbert

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#pragma once

//#define USE_MATRIX_I2C

/* Select hand configuration */

#define MASTER_LEFT
// #define MASTER_RIGHT
// #define EE_HANDS

#define SSD1306OLED

#define USE_SERIAL_PD2

#define TAPPING_FORCE_HOLD
//#define TAPPING_TERM 100
#undef TAPPING_TERM
#define TAPPING_TERM 200
#define PERMISSIVE_HOLD

#undef RGBLED_NUM
#define RGBLIGHT_ANIMATIONS
#define RGBLED_NUM 27
#define RGBLIGHT_LIMIT_VAL 120
#define RGBLIGHT_HUE_STEP 10
#define RGBLIGHT_SAT_STEP 17
#define RGBLIGHT_VAL_STEP 17

// Underglow
/*
#undef RGBLED_NUM
#define RGBLED_NUM 14    // Number of LEDs
#define RGBLIGHT_ANIMATIONS
#define RGBLIGHT_SLEEP
*/

#define JIS_MINS KC_MINS // - and =
#define JIS_CIRC KC_EQL  // ^ and ~
#define JIS_JYEN KC_JYEN // \ and |
#define JIS_ATMK KC_LBRC // @ and `
#define JIS_LBRC KC_RBRC // [ and {
#define JIS_SCLN KC_SCLN // ; and +
#define JIS_COLN KC_QUOT // : and *
#define JIS_RBRC KC_BSLS // ] and }
#define JIS_COMM KC_COMM // , and <
#define JIS_DOT  KC_DOT  // . and >
#define JIS_SLSH KC_SLSH // / and ?
// #define JIS_BSLS 0x87    // \ and _(いわゆる「ろ」キー)
#define JIS_BSLS KC_RO    // \ and _(いわゆる「ろ」キー)

// for Mac


#define KC_KANA_MAC KC_LANG1 // MAC kana key
#define KC_EISU_MAC KC_LANG2 // MAC eisu key
