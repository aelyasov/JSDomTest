/*t dom : string */
function test(inputDiv) {
  if (inputDiv == 'generateDOMInput') {
    inputDiv = document.getElementById('ConfixDummyNode1');
  }

  var span = inputDiv.getElementsByTagName('SPAN')[0];
  span.style.display='';
  inputDiv.style.backgroundColor='#DDD';
  span.style.color='#317082';
  var typingSpan = inputDiv.getElementsByTagName('SPAN')[1];
  typingSpan.style.display='none';
}