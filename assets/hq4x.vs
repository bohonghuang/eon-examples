// Source: https://github.com/CrossVR/emulator-shaders/blob/master/assets/hq4x.shader

// 4xGLSLHqFilter shader
// 
// Copyright (C) 2005 guest(r) - guest.r@gmail.com
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#version 330

uniform vec2 inputSize;

in vec2 vertexPosition;
in vec2 vertexTexCoord;
out vec4 vTexCoord[7];

uniform mat4 mvp;

void main() {
  vec2 dg1 = 0.5 / inputSize;
  vec2 dg2 = vec2(-dg1.x, dg1.y);
  vec2 sd1 = dg1 * 0.5;
  vec2 sd2 = dg2 * 0.5;
  vec2 ddx = vec2(dg1.x, 0.0);
  vec2 ddy = vec2(0.0, dg1.y);

  gl_Position = mvp * vec4(vertexPosition, 0.0, 1.0);
  vTexCoord[0].xy = vertexTexCoord;
  vTexCoord[1].xy = vertexTexCoord - sd1;
  vTexCoord[2].xy = vertexTexCoord - sd2;
  vTexCoord[3].xy = vertexTexCoord + sd1;
  vTexCoord[4].xy = vertexTexCoord + sd2;
  vTexCoord[5].xy = vertexTexCoord - dg1;
  vTexCoord[6].xy = vertexTexCoord + dg1;
  vTexCoord[5].zw = vertexTexCoord - dg2;
  vTexCoord[6].zw = vertexTexCoord + dg2;
  vTexCoord[1].zw = vertexTexCoord - ddy;
  vTexCoord[2].zw = vertexTexCoord + ddx;
  vTexCoord[3].zw = vertexTexCoord + ddy;
  vTexCoord[4].zw = vertexTexCoord - ddx;
}
