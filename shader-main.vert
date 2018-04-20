#version 330 core
in vec2 i_vertex;
out vec3 v_pos;
void main() {
        v_pos = vec3(i_vertex.xy*vec2(1280.0/800.0, 1.0), 1.0);
        gl_Position = vec4(i_vertex, -1.0, 1.0);
}
// vim: set syntax=c :
