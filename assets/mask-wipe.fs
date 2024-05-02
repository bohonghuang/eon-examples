#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;

out vec4 finalColor;

uniform float progress = 0.0;
uniform vec4 background = vec4(0.0);
uniform sampler2D mask;
uniform float smoothness = 0.0;
uniform bool flipXP = false;
uniform bool flipYP = false;

void main() {
  vec2 uv = vec2(flipXP ? 1.0 - fragTexCoord.s : fragTexCoord.s, flipYP ? 1.0 - fragTexCoord.t : fragTexCoord.t);
  finalColor = texture(texture0, fragTexCoord) * fragColor;
  float amount = smoothstep(mix(0.0, - smoothness, progress), mix(smoothness, 0.0, progress), progress - dot(texture(mask, uv).rgb, vec3(0.333)));
  finalColor.rgb = mix(finalColor.rgb, background.rgb, amount * background.a);
  finalColor.a = mix(finalColor.a, background.a, amount);
}
