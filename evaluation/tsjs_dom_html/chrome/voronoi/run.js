function run() {
  if(freeze_frame_switch) { return; }
  context.globalCompositeOperation = "source-over";
  context.fillStyle = "rgba(8, 8, 12, .65)";
  context.fillRect(0, 0, canvas_width, canvas_heigth);
  context.globalCompositeOperation = "lighter";
  context.strokeStyle = "white";
  mouse_vx = mouse_x-previous_mouse_x;
  mouse_vy = mouse_y-previous_mouse_y;
  previous_mouse_x = mouse_x;
  previous_mouse_y = mouse_y;
  var minV = 0;
  var maxV = 0;
  init_delaunay();
  var i = num_vertices;
  while(i--) {
    var vertex = vertices[i];
    var x = vertex.x;
    var y = vertex.y;
    var vx = vertex.vx;
    var vy = vertex.vy;
    var dx = x-mouse_x;
    var dy = y-mouse_y;
    var distance = Math.sqrt(dx*dx+dy*dy);
    var angle = Math.atan2(dy, dx);
    var cos_angle = Math.cos(angle);
    var sin_angle = Math.sin(angle);
    if(is_mouse_down) {
        if(distance<blow_distance) {
          var blow_acceleration = (1-(distance/blow_distance))*14;
          vx += cos_angle*blow_acceleration+.5-Math.random();
          vy += sin_angle*blow_acceleration+.5-Math.random();
       }
    }
    if(distance<follow_distance) {
      var follow_acceleration = (1-(distance/follow_distance))*canvas_width*.0008;
      vx -= cos_angle*follow_acceleration;
      vy -= sin_angle*follow_acceleration;
    }
    if(distance<stir_distance) {
      var stir_acceleration = (1-(distance/stir_distance))*canvas_width*.00022;
      vx += mouse_vx*stir_acceleration;
      vy += mouse_vy*stir_acceleration;
    }
    vx *= viscosity;
    vy *= viscosity;
    var abs_vx = Math.abs(vx);
    var abs_vy = Math.abs(vy);
    if(abs_vx<.1) vx*=Math.random()*3;
    if(abs_vy<.1) vy*=Math.random()*3;
    var next_x = x+vx;
    var next_y = y+vy;
    if(next_x>canvas_width) {
      next_x = canvas_width;
      vx *= -1;
    }
    else if(next_x<0) {
      next_x = 0;
      vx *= -1;
    }
    if(next_y>canvas_heigth) {
      next_y = canvas_heigth;
      vy *= -1;
    }
    else if(next_y<0) {
      next_y = 0;
      vy *= -1;
    }
    vertex.vx=vx;
    vertex.vy=vy;
    vertex.x=next_x;
    vertex.y=next_y;
    var velocity_sqr=vx*vx + vy*vy;
    if(velocity_sqr<minV) minV=velocity_sqr;
    if(velocity_sqr>maxV) maxV=velocity_sqr;
    vertex.payload["velocity"] = velocity_sqr;
    insert(vertex);
  }

  var velocity_range = maxV-minV;
  var velocity_mid = (maxV-minV)/2;
  edges.each(function(an_edge){
    var org = an_edge.org();
    var right = an_edge.right();
    var dest = an_edge.dest();
    var left = an_edge.left();
    if(crust_skeleton_switch && !an_edge.is_infinite_edge()) {
      if(an_edge.is_crust()){ draw_line(right, left, 2); }
      else{ draw_line(org, dest, 2); }
    }
    if(delaunay_switch) {
      if(!an_edge.is_infinite_edge()) draw_line(org, dest, 2);
      if(color_switch && !an_edge.is_infinite_edge()){
        if(left.payload["painted"]==false && !an_edge.onext().is_infinite_edge()) {
          onext_velocity=0;
          [an_edge.org().payload["velocity"], an_edge.dest().payload["velocity"], an_edge.onext().dest().payload["velocity"]].each(function(velocity){onext_velocity += velocity/3;});
          onext_color=heatmap(velocity_range, velocity_mid, onext_velocity);
          draw_triangle(org, dest, an_edge.onext().dest(), onext_color);
          left.payload["painted"]=true;
        }

        if(right.payload["painted"]==false && !an_edge.oprev().is_infinite_edge()) {
          oprev_velocity=0;
          [an_edge.org().payload["velocity"], an_edge.dest().payload["velocity"], an_edge.oprev().dest().payload["velocity"]].each(function(velocity){oprev_velocity += velocity/3;});
          oprev_color=heatmap(velocity_range, velocity_mid, oprev_velocity);
          draw_triangle(org, dest, an_edge.oprev().dest(), oprev_color);
          right.payload["painted"]=true;
        }
      }
    }
    if(voronoi_switch) {
      if(!an_edge.is_infinite_edge()) draw_line(right, left, 2);
      if(color_switch) {
        var org_color = heatmap(velocity_range, velocity_mid, an_edge.org().payload["velocity"]);
        var dest_color = heatmap(velocity_range, velocity_mid, an_edge.dest().payload["velocity"]);
        if(!org.is_infinity && right!=null && left!=null) { draw_triangle(org, right, left, org_color); }
        if(!dest.is_infinity && right!=null && left!=null) { draw_triangle(dest, left, right, dest_color); }
      }
    }
  });
}