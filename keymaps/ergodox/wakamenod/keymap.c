// Netable differences vs. the default firmware for the ErgoDox EZ:
// 1. The Cmd key is now on the right side, making Cmd+Space easier.
// 2. The media keys work on OSX (But not on Windows).
#include QMK_KEYBOARD_H
#include "debug.h"
#include "action_layer.h"
#include "keymap_JIS.h"

#define BASE 0 // default layer
#define SYMB 1 // symbols
#define MDIA 2 // media keys

#define M_ESC M(1)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
/* Keymap 0: Basic layer
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * | Alfred |   1  |   2  |   3  |   4  |   5  |Fuwari|           | OSCap|   6  |   7  |  8 ( |  9 ) |   0  | Back   |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * | Tab    |   Q  |   W  |   E  |   R  |   T  |  -=  |           |  [   |   Y  |   U  |   I  |   O  |   P  |   @`   |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * | ALL    |   A  |   S  |   D  |   F  |   G  |------|           |------|   H  |   J  |   K  |   L  |  ;+  |  :*    |
 * |--------+------+------+------+------+------|  ^~  |           |  ]   |------+------+------+------+------+--------|
 * | Sfhit  |   Z  |   X  |   C  |   V  |   B  |      |           |      |   N  |   M  |   ,  |   .  |   /  | Shift  |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |Option| Cmd  |CtrEIS|                                       |CtrKan|  Cmd |      |      |   \    |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |  ←   | →     |
 *                                 ,------|------|------|       |------+-------+------.
 *                                 |      |      |      |       |  ↑   |       |      |
 *                                 | Space|Opt(R)|------|       |------| Shift |Enter |
 *                                 | /SYMB|      |      |       |  ↓   |       |      |
 *                                 `--------------------'       `---------------------'
 */
// If it accepts an argument (i.e, is a function), it doesn't need KC_.
// Otherwise, it needs KC_*
[BASE] = LAYOUT_ergodox(  // layer 0 : default
        // left hand
        HYPR(KC_SPC),   KC_1,    KC_2,      KC_3,    KC_4,      KC_5,  HYPR(KC_5),
        KC_TAB,         KC_Q,    KC_W,      KC_E,    KC_R,      KC_T,  JIS_MINS,
        MEH_T(KC_LGUI), KC_A,    KC_S,      KC_D,    KC_F,      KC_G,
        KC_LSFT,        KC_Z,    KC_X,      KC_C,    KC_V,      KC_B,  JIS_CIRC,
        KC_NO,          KC_NO,   KC_LALT,   KC_LGUI,  CTL_T(KC_EISU_MAC),
                                             KC_NO,   KC_NO,
                                                      KC_NO,
                            LT(SYMB,KC_SPC), KC_RALT, KC_NO,
        // right hand
        SGUI(KC_4), KC_6,   KC_7,               KC_8,    KC_9,    KC_0,     KC_BSPC,
        JIS_LBRC,   KC_Y,   KC_U,               KC_I,    KC_O,    KC_P,     JIS_ATMK,
                    KC_H,   KC_J,               KC_K,    KC_L,    JIS_SCLN, JIS_COLN,
        JIS_RBRC,   KC_N,   KC_M,               KC_COMM, KC_DOT,  KC_SLSH,  KC_RSFT,
                            CTL_T(KC_KANA_MAC), KC_RGUI, KC_NO,   KC_NO,    LALT(KC_JYEN),
        KC_LEFT,   KC_RGHT,
        KC_UP,
        KC_DOWN,   KC_RSFT, KC_ENT
    ),

/* Keymap 1: Symbol Layer
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * | RESET  |  F1  |  F2  |  F3  |  F4  |  F5  |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |   !  |   @  |   {  |   }  |   |  |      |           |   &  |   !  |   1  |      |      |   0  |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |    0   |   #  |   $  |   (  |   )  |   `  |------|           |------|   ←  | ↓  |  ↑  |  →  |      |    \   |
 * |--------+------+------+------+------+------|      |           |   |  |------+------+------+------+------+--------|
 * |        |   %  |   ^  |   [  |   ]  |   ~  |      |           |      |   ¥  |      |      |      |      |   \_   |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      |      |      |                                       |      |      |      |      |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |      |
 *                                 |      |      |------|       |------|      |      |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// SYMBOLS
[SYMB] = LAYOUT_ergodox(
       // left hand
       RESET, KC_F1,  KC_F2,  KC_F3,  KC_F4,  KC_F5,  KC_TRNS,
       KC_TRNS,KC_EXLM,KC_AT,  KC_LCBR,KC_RCBR,KC_PIPE,KC_TRNS,
       KC_TRNS,KC_HASH,KC_DLR, KC_LPRN,KC_RPRN,KC_GRV,
       KC_TRNS,KC_PERC,KC_CIRC,KC_LBRC,KC_RBRC,KC_TILD,KC_TRNS,
       KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,KC_TRNS,
                                       KC_TRNS,KC_TRNS,
                                               KC_TRNS,
                               KC_TRNS,KC_TRNS,KC_TRNS,
       // right hand
       KC_TRNS,    KC_TRNS,  KC_TRNS,  KC_TRNS,  KC_TRNS, KC_TRNS,  KC_TRNS,
       S(KC_6),    KC_EXLM,  KC_1,     KC_TRNS,  KC_TRNS, KC_0,     KC_TRNS,
                   KC_LEFT,  KC_DOWN,  KC_UP,    KC_RGHT, KC_TRNS,  LALT(KC_JYEN),
       S(KC_JYEN), KC_JYEN,  KC_TRNS,  KC_TRNS,  KC_TRNS, KC_TRNS,  JIS_BSLS,
                             KC_TRNS,  KC_TRNS,  KC_TRNS, KC_TRNS,  KC_JYEN,
       KC_TRNS, KC_TRNS,
       KC_TRNS,
       KC_TRNS, KC_TRNS, HYPR(KC_ENT)
),
/* Keymap 2: Media and mouse keys
 *
 * ,--------------------------------------------------.           ,--------------------------------------------------.
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * |--------+------+------+------+------+-------------|           |------+------+------+------+------+------+--------|
 * |        |      |      | MsUp |      |      |      |           |      |   ←  |   ↓  |  ↑   |  →   |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |MsLeft|MsDown|MsRght|      |------|           |------|   ←  |   ↓  |  ↑   |  →   |      |        |
 * |--------+------+------+------+------+------|      |           |      |------+------+------+------+------+--------|
 * |        |      |      |      |      |      |      |           |      |      |      |      |      |      |        |
 * `--------+------+------+------+------+-------------'           `-------------+------+------+------+------+--------'
 *   |      |      |      | Lclk | Rclk |                                       |       |     |      |      |      |
 *   `----------------------------------'                                       `----------------------------------'
 *                                        ,-------------.       ,-------------.
 *                                        |      |      |       |      |      |
 *                                 ,------|------|------|       |------+------+------.
 *                                 |      |      |      |       |      |      |Brwser|
 *                                 |      |      |------|       |------|      |Back  |
 *                                 |      |      |      |       |      |      |      |
 *                                 `--------------------'       `--------------------'
 */
// MEDIA AND MOUSE
[MDIA] = LAYOUT_ergodox(
       KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_MS_U, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_MS_L, KC_MS_D, KC_MS_R, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS, KC_TRNS, KC_BTN1, KC_BTN2,
                                           KC_TRNS, KC_TRNS,
                                                    KC_TRNS,
                                  KC_TRNS, KC_TRNS, KC_TRNS,
    // right hand
       KC_TRNS,  KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS,  KC_MS_L, KC_MS_D, KC_MS_U, KC_MS_R, KC_TRNS, KC_TRNS,
                 KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, KC_TRNS, KC_TRNS,
       KC_TRNS,  KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
                          KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS, KC_TRNS,
       KC_TRNS, KC_TRNS,
       KC_TRNS,
       KC_TRNS, KC_TRNS, KC_WBAK
),
};

const uint16_t PROGMEM fn_actions[] = {
    [1] = ACTION_LAYER_TAP_TOGGLE(SYMB)                // FN1 - Momentary Layer 1 (Symbols)
};

const macro_t *action_get_macro(keyrecord_t *record, uint8_t id, uint8_t opt)
{
  // MACRODOWN only works in this function
  switch(id) {
    case 0:
      if (record->event.pressed) {
        register_code(KC_RSFT);
      } else {
        unregister_code(KC_RSFT);
      }
      break;
    case 1:// Escと英数の同時押し
      if (record->event.pressed) {
        return MACRO(T(ESC), T(LANG2), END);
      }
      break;
  }
  return MACRO_NONE;
};

// Runs just one time when the keyboard initializes.
void matrix_init_user(void) {

};

// Runs constantly in the background, in a loop.
void matrix_scan_user(void) {

  uint8_t layer = biton32(layer_state);

  ergodox_board_led_off();
  ergodox_right_led_1_off();
  ergodox_right_led_2_off();
  ergodox_right_led_3_off();
  switch (layer) {
    // TODO: Make this relevant to the ErgoDox EZ.
    case SYMB:
      ergodox_right_led_1_on();
      break;
    case MDIA:
      ergodox_right_led_2_on();
      break;
    default:
      // none
      break;
  }

};
