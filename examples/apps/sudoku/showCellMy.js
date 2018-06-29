/*t dom : string */
function test(inputDiv) {
  if (inputDiv == 'generateDOMInput') {
    inputDiv = document.getElementById('ConfixDummyNode1');
    inputDiv.style.backgroundColor='#DDD';
  }

  var span = document.getElementsByTagName('SPAN')[0];
  span.style.display='';
  span.style.color='#317082';
  var typingSpan = document.getElementsByTagName('SPAN')[1];
  typingSpan.style.display='none';
}