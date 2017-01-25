 function bindTaskEvents(taskListItem, checkBoxEventHandler) {
   //Select li children
   var checkBox = taskListItem.querySelector("input[type=checkbox]");
   var editButton = taskListItem.querySelector("button.edit");
   var deleteButton = taskListItem.querySelector("button.delete");
   
   //Bind the editTask to edit button
   editButton.onclick = editTask;
   //Bind deleteTask to delete button	
   deleteButton.onclick = deleteTask;
   //Bind checkBoxEventHandler to checkbox	
   checkBox.onchange = checkBoxEventHandler;
 }