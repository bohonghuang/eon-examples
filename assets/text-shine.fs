#version 330

in vec2 fragTexCoord;
in vec3 fragPosition;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

uniform vec2 size = vec2(320.0, 192.0);
uniform sampler2D shineMask;
uniform vec2 shineOffset;

out vec4 finalColor;

void main() {
  vec4 texelColor = texture(texture0, fragTexCoord);
  if (texelColor.a == 0.0) discard;
  finalColor = texelColor * fragColor * colDiffuse;
  vec4 maskColor = texture(shineMask, fragPosition.xy / size - shineOffset);
  finalColor.rgb += maskColor.rgb * maskColor.a;
}
