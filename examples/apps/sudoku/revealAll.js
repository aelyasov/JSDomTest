/*t dom */
function test() {
  for(var row=0;row<2;row++){
    for(var col=0;col<2;col++){
      var obj = document.getElementById('square_'+row+'_'+col);
      var spans = obj.getElementsByTagName('SPAN');
      spans[0].style.display='';
      spans[1].style.display='none';
      spans[1].style.color='#000000';
    }
  }
}
