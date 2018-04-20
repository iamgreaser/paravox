#version 330 core
in vec3 v_pos;
out vec4 f_color;

uniform usampler3D tex_map;

uniform vec3 cam_accel;
uniform vec3 cam_pos;
uniform vec3 cam_dir;

uvec4 last_blk = uvec4(255U,255U,255U,255U);
float dens_old(vec3 sp) {
        vec3 p = vec3(sp);
        p.z = max(p.z, -6.0);
        p.z = min(p.z, -2.0);
        p = mod(p, 4.0);
        p -= 2.0;
        vec3 a = abs(p);
        float pmax = max(a.z, max(a.x, a.y));
        float pmin = min(a.z, min(a.x, a.y));
        float pmed = (a.x + a.y + a.z) - pmax - pmin;
        float d_lattice = pmed-0.2;
        float d_sphere = length(sp-vec3(0.0, 0.0, -3.0))-0.4;
        if(sp.z >= 4.0) {
                d_lattice = max(sp.z-4.0, d_lattice);
        }
        return min(d_lattice, d_sphere);
}

float dens(vec3 sp) {
        vec3 p = vec3(sp);
        float boxmin = min(min(p.x, p.y), p.z);
        if(boxmin < 0.0) {
                return -boxmin;
        }
        float boxmax = max(max(p.x-512.0, p.y-64.0), p.z-512.0);
        if(boxmax > 0.0) {
                return boxmax;
        }
        ivec3 cell = ivec3(floor(p));
        vec3 subp = (p - vec3(cell));
        vec3 cellmin = vec3(0.0);
        vec3 cellmax = vec3(1.0);
        uvec4 t = texelFetch(tex_map, cell, 0).rgba;
        if(t.a <= 3U) {
                uvec4 th = texelFetch(tex_map, cell + ivec3(0,4,0), 0).rgba;
                if(th.a >= t.a+4U && th.a < 0x80U) {
                        t = th;
                        cell.y += 4;
                        subp.y -= 4.0;
                }
        }
        cellmin -= float(t.a);
        cellmax += float(t.a);
        subp = min(subp-cellmin, cellmax-subp);
        if(t.a >= 0x80U) {
                last_blk = uvec4(t);
                //return -min(subp.z, min(subp.x, subp.y))-0.01;
                return -0.5;
        }
        return max(min(subp.z, min(subp.x, subp.y)), 0.01);
}

void main() {
        vec3 cp = cam_pos;
        vec3 vforward = normalize(cam_dir);
        vec3 vright = normalize(cross(vforward, vec3(0.0, -1.0, 0.0)));
        vec3 vup = normalize(cross(vforward, vright));
        vec3 cv = normalize(v_pos.x*vright + v_pos.z*vforward + v_pos.y*vup);
        
        vec3 cc = cv*0.5+0.5;
        vec3 p = cp;
        vec3 v = cv;
        vec3 a = cam_accel;
        bool hit_thing = false;
        
        for(int reps = 0; reps < 50; reps++) {
                float dist = dens(p);
                vec3 ev = normalize(v);
                if(p.x < 0.0 && v.x <= 0.0 && a.x <= 0.0) { break; }
                if(p.y < 0.0 && v.y <= 0.0 && a.y <= 0.0) { break; }
                if(p.z < 0.0 && v.z <= 0.0 && a.z <= 0.0) { break; }
                if(p.x >= 512.0 && v.x >= 0.0 && a.x >= 0.0) { break; }
                if(p.y >= 64.0  && v.y >= 0.0 && a.y >= 0.0) { break; }
                if(p.z >= 512.0 && v.z >= 0.0 && a.z >= 0.0) { break; }
                
                if(dist < 0.0) {
                        const float noffs = 0.2;
                        uvec4 saved_blk = uvec4(last_blk);
                        vec3 sn0 = vec3(dist);
                        vec3 sn1 = vec3(
                                dens(p+noffs*vec3(1.0,0.0,0.0)),
                                dens(p+noffs*vec3(0.0,1.0,0.0)),
                                dens(p+noffs*vec3(0.0,0.0,1.0)));
                        vec3 sn2 = vec3(
                                dens(p-noffs*vec3(1.0,0.0,0.0)),
                                dens(p-noffs*vec3(0.0,1.0,0.0)),
                                dens(p-noffs*vec3(0.0,0.0,1.0)));
                        vec3 sn = normalize(sn1-sn2);
                        //float diff = max(0.2, -dot(sn, normalize(p - cp)));
                        float diff = max(0.2, -dot(sn, ev));
                        cc = vec3(saved_blk.xyz)/255.0;
                        cc *= diff;
                        //cc = vec3(1.0)*diff;
                        hit_thing = true;
                        break;
                }
                float adv = max(0.1, min(50.0, 0.5*dist))/length(v);
                p += v*adv + a*adv*adv/2.0;
                v += a*adv;
        }
        
        if(!hit_thing) {
                cc = normalize(v)*0.5+0.5;
        }
        
        f_color = vec4(cc, 1.0);
}

// vim: set syntax=c :
