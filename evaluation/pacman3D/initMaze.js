function initMaze() {
  var r,c, imgsrc, i, id, x, y, z, mpr;
  var maze = elements.maze=document.getElementById('maze');
  var misieFilter="progid:DXImageTransform.Microsoft.AlphaImageLoader(src='"+imgPath+"x.png', sizingMethod='crop')";
  for (r=0; r<22; r++) {
    mpr = mazePos[r+1] = new Array();
    for (c=0; c<19; c++) {
      x= (21-r)*14+c*24;
      y= 20+r*11+c*6;
      z= 1+c*6+r*114;
      mpr[c+1]= {x: x-2, y: y-10, z:z};
      id='t'+(r+1)+'_'+(c+1);
      i=document.createElement('img');
      i.id=id;
      if (msieLe6) {
	i.src=imgPath+'xx.gif';
	i.style.filter=misieFilter;
      }
      else {
	i.src=imgPath+'x.png';
      }
      i.width=28; i.height=17;
      i.style.display='block';
      i.style.position='absolute';
      i.style.left=x+'px';
      i.style.top=y+'px';
      i.style.zIndex=z;
      maze.appendChild(i);
      elements[id]=i;
    }
  }
}