/*t dom */
function test() {
  var obj = document.getElementById('sudoku');
  var subObjects = obj.getElementsByTagName('DIV');
  for(var no=0; no<subObjects.length; no++){
    if(subObjects[no].className=='sudokuSquare'){
      subObjects[no].style.backgroundColor='';
      var spans = subObjects[no].getElementsByTagName('SPAN');
      spans[0].style.display='none';
      spans[1].innerHTML = '';
    }
  }
}