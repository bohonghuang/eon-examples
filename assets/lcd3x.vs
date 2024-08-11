// Source: https://github.com/CrossVR/emulator-shaders/blob/master/assets/lcd3x.shader

// Author: Gigaherz
// License: Public domain

#version 330

uniform vec2 inputSize;
uniform mat4 mvp;
in vec2 vertexPosition;
in vec2 vertexTexCoord;
out vec2 vTexCoord;
out vec2 omega;

void main() {
  gl_Position = mvp * vec4(vertexPosition, 0.0, 1.0);
  vTexCoord = vertexTexCoord;
  omega = 3.141592654 * 2.0 * inputSize;
}
