# taskStack

A program for remembering what the hell you were doing while under heavy cognitive load

This is not a todo list. You are only ever working on one task at a time. 
However, that task may be composed of subtasks, which may be composed of subtasks, etc

This tool is for remembering the specific subtask you're working on, and how it relates to your higher level goal.

## Example usage

note: some features may not be implemented yet (TODO)

```bash
$ tstk push "A task"

$ tstk peek
A task

$ tstk push "A subtask"

$ tstk push "A subtask of a subtask"

$ tstk list
A task
A subtask
A subtask of a subtask

$ tstk pop
Task completed: A subtask of a subtask

$ tstk done
Task completed: A subtask

$ tstk peek
A task

$ echo "foo\nbar" | tstk push

$ tstk list
A task
foo
bar

$ tstk clear

$ tstk peek
There are no tasks on the task stack!

$ tstk undo
$ tstk list
A task
foo
bar

```

