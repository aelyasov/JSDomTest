function tasksCompleted() {
  console.log("complete task....")
  //Append the task li to the ul #completed-tasks
  var listItem = this.parentNode;
  completedTasksHolder.appendChild(listItem);
  bindTaskEvents(listItem, tasksIncomplete);
}