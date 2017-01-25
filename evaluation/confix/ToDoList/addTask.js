function addTask() {
  console.log("Add task...")
  var listItem = createNewTaskElement(taskInput.value);
  //append listItem to incompleteTasksHolder
  incompleteTasksHolder.appendChild(listItem);
  bindTaskEvents(listItem, tasksCompleted);
  
  //clear input value after task is added
  taskInput.value = "";
}