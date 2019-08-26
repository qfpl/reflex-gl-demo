#version 410

in vec3 vp;
uniform mat4 proj;
out vec3 color;

void main() {
  gl_Position = proj * vec4(vp, 1.0);
  color = vec3((vp.x + 6)/12, (vp.y + 10)/20, 0.5);
}
