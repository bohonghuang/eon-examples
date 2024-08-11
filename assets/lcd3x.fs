// Source: https://github.com/CrossVR/emulator-shaders/blob/master/assets/lcd3x.shader

// Author: Gigaherz
// License: Public domain

#version 330

uniform sampler2D texture0;
in vec2 vTexCoord;
in vec2 omega;
out vec4 finalColor;

/* configuration (higher values mean brighter image but reduced effect depth) */
const float brighten_scanlines = 16.0;
const float brighten_lcd = 4.0;

const vec3 offsets = 3.141592654 * vec3(1.0/2.0,1.0/2.0 - 2.0/3.0,1.0/2.0-4.0/3.0);

void main() {
  vec2 angle = vTexCoord * omega;

  float yfactor = (brighten_scanlines + sin(angle.y)) / (brighten_scanlines + 1.0);
  vec3 xfactors = (brighten_lcd + sin(angle.x + offsets)) / (brighten_lcd + 1.0);

  finalColor.rgb = yfactor * xfactors * texture2D(texture0, vTexCoord).rgb;
  finalColor.a = 1.0;
}
