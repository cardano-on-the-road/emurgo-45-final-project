<<<<<<< HEAD
=======
# Revision history for emurgo45-evenodd

## 0.1.1.0 -- 2022-03-28

* First revision

### 1. Use OverloadedStrings extension
-> VM:  Done

### 2. Why do you need NA in Direction - Use Maybe Direction instead
-> VM: I should combine MaybeT with RSWT + IO. I would need an help to do it.

### 3. Can't you combine logEvent and tell 
-> VM: Done

### 4. feedSnake function can be written in a more clever manner One equation is redundant
-> VM: Done
### 5. Challenge: implement it pointfree that is without arguments
-> VM: Done

### 6. moveSnake function is doing what state should do. rmLastEl function is just the init function

### 7. You can make the drawing logic simpler by creating an Env -> GameState -> String function And use a separate state monad computation for that Where state is the Matrix
VM -> It’s true. This is the best way. I decided to print the matrix to not create manually the string

### 8. Also modularise the code, everything is in a single file right now
VM -> Done


### 9. Basically any piece of code you felt was repetitive or hard to write or hard to read should be rewritten. Overall looks good to me! Will wait for some of the minor changes before final approval
>>>>>>> dev
