 function tasksIncomplete() {
   console.log("Task incomplete...")
   //Append the task li to the ul #incomplete-tasks
   var listItem = this.parentNode;
   incompleteTasksHolder.appendChild(listItem);
   bindTaskEvents(listItem, tasksCompleted);
}