function deleteTask() {
  console.log("Delete task....");
  
  var listItem = this.parentNode;
  var ul = listItem.parentNode;
  ul.removeChild(listItem);
}