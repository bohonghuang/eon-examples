// Source: https://github.com/CrossVR/emulator-shaders/blob/master/assets/hq2x.shader

#version 330

uniform vec2 inputSize;
in vec2 vertexPosition;
in vec2 vertexTexCoord;
out vec4 vTexCoord[5];

uniform mat4 mvp;

void main() {
  vec2 dg1 = 0.5 / inputSize;
  vec2 dg2 = vec2(-dg1.x, dg1.y);
  vec2 dx = vec2(dg1.x, 0.0);
  vec2 dy = vec2(0.0, dg1.y);

  vTexCoord[0].xy = vertexTexCoord;
  vTexCoord[1].xy = vertexTexCoord - dg1;
  vTexCoord[1].zw = vertexTexCoord - dy;
  vTexCoord[2].xy = vertexTexCoord - dg2;
  vTexCoord[2].zw = vertexTexCoord + dx;
  vTexCoord[3].xy = vertexTexCoord + dg1;
  vTexCoord[3].zw = vertexTexCoord + dy;
  vTexCoord[4].xy = vertexTexCoord + dg2;
  vTexCoord[4].zw = vertexTexCoord - dx;

  gl_Position = mvp * vec4(vertexPosition, 0.0, 1.0);
}
