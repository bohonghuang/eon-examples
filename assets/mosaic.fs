#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

out vec4 finalColor;

uniform float width = 320.0;
uniform float height = 192.0;
uniform float intensity = 16.0;
uniform float progress = 0.0;

void main() {
  float mosaicSize = intensity * progress;
  vec2 d = mosaicSize * vec2(1.0 / width, 1.0 / height);
  vec2 coord = min(mosaicSize <= 1.0 ? fragTexCoord : d * round(fragTexCoord / d), vec2(0.999999, 0.999999));
  finalColor = texture(texture0, coord);
  finalColor *= fragColor;
}
